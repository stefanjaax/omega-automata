-- | Definition of various kinds of omega automata
module OmegaAutomata.Automata where
import qualified Data.Set as S
import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Bimap as B
import Data.List (groupBy, nub)

type State = Int

data Rank = Rank Int | Bot deriving (Eq, Ord, Show)

type Ranking q = M.Map q Rank

-- | Data-type for non-deterministic Buchi automata
data NBA q a l = NBA{ states :: S.Set q       -- ^ The states of the NBA
                    , bimap :: B.Bimap q Node -- ^ Bijection between states and nodes in the graph
                    , graph :: Gr l a         -- ^ The internal graph representing transitions
                    , start :: S.Set q        -- ^ The initial states in the NBA
                    , accept :: S.Set q       -- ^ The set of accepting states in the NBA
                    } deriving (Show)

-- | State-data type used in the complement-construction
data (Ord q) => CompState q = PowerState [q] | RankState (Ranking q, [q]) deriving Show

instance (Ord q) => Eq (CompState q) where
  (==) (PowerState qs1) (PowerState qs2) = qs1 == qs2
  (==) (RankState (r1, qs1)) (RankState (r2, qs2)) = r1 == r2 && qs1 == qs2
  (==) _ _ = False

instance (Ord q) => Ord (CompState q) where
  compare (PowerState qs1) (PowerState qs2) = compare qs1 qs2
  compare (RankState (r1, qs1)) (RankState (r2, qs2)) =
    case compare r1 r2 of
      EQ -> compare qs1 qs2
      a -> a
  compare (PowerState _) (RankState _) = LT
  compare (RankState _) (PowerState _) = GT


-- | Returns node in internal graph corresponding to state
toNode :: (Ord q) => NBA q a l -> q -> Node
toNode a q = (bimap a) B.! q


-- | Returns state corresponding to node in internal graph
toState :: (Ord q) => NBA q a l -> Node -> q
toState a q = (bimap a) B.!> q


-- | Returns successors of state with corresponding edge-labels
succs :: (Ord q) => NBA q a l   -- ^ The NBA the state belongs to
                 -> q           -- ^ The state
                 -> [(q, a)]    -- ^ Successor states and corresponding edge-label.
succs a q = (\(q', l) -> (toState a q', l)) <$> lsuc (graph a) (toNode a q)


-- | Returns predecessors of state with corresponding edge-labels
pres :: (Ord q) => NBA q a l    -- ^ The NBA the state belongs to
                -> q            -- ^ The state
                -> [(q, a)]     -- ^ Predecessors and corresponding edge-label.
pres a q = (\(q', l) -> (toState a q', l)) <$> lpre (graph a) (toNode a q)


-- | Returns list of successor-states for a given state and edge-label
aSuccs :: (Ord q, Eq a) => NBA q a l   -- ^ The NBA the state belongs to
                        -> q           -- ^ The state
                        -> a           -- ^ The edge-label
                        -> [q]         -- ^ List of successors for state and edge-label.
aSuccs a q b = [toState a q' | (q', b') <- lsuc (graph a) (toNode a q), b' == b]


-- | Returns list of predecessor-states for a given state and edge-label
aPres :: (Ord q, Eq a) => NBA q a l   -- ^ The NBA the state belongs to
                       -> q           -- ^ The state
                       -> a           -- ^ The edge-label
                       -> [q]         -- ^ List of predecessors for state and edge-label.
aPres a q b = [toState a q' | (q', b') <- lpre (graph a) (toNode a q), b' == b]


-- | Returns all labelled transitions defined in a given NBA
trans :: (Ord q) => NBA q a l     -- ^ The NBA
                 -> [(q, a, q)]   -- ^ List of labelled transitions defined in NBA
trans a = [(q1, l, q2) | (i1, i2, l) <- labEdges (graph a)
                       , let q1 = (bimap a) B.!> i1
                       , let q2 = (bimap a) B.!> i2]


-- | Collects states with same label in a list
combine :: (Eq a, Ord q) => [(q, a)] -> [([q], a)]
combine xs = let xs' = groupBy (\x y -> snd x == snd y) xs in
  case xs' of
    [] -> []
    _ -> map (\ys -> (fst <$> ys, head (snd <$> ys))) xs'


-- | Lifts successor-relation of a state to successor-relation of lists of states
powerSucc :: (Eq a, Ord q) => NBA q a l -> [q] -> [([q], a)]
powerSucc a qs = map (\(xs, l) -> (nub xs, l)) $ combine $ concat [succs a q | q <- qs]


-- | Same as 'powerSucc', only for distinct symbol
powerASucc :: (Ord a, Ord q) => NBA q a l -> [q] -> a -> [q]
powerASucc a qs l = concat [aSuccs a q l | q <- qs]


-- | Annotates states of an automaton
annotateStates :: (Ord q, Ord i) => i                 -- ^ The value used for annotating
                                 -> NBA q a l         -- ^ The original NBA
                                 -> NBA (i, q) a l    -- ^ The annotated NBA
annotateStates i a = let annotate = S.map (\x -> (i, x)) in
                      NBA{ states = annotate (states a)
                         , bimap = B.fromList [((i, q), n) | (q, n) <- (B.assocs (bimap a))]
                         , graph = graph a
                         , start = annotate (start a)
                         , accept = annotate (accept a)
                         }


-- | Inserts states into an NBA
insertStates :: (Ord q) => [(q, l)]     -- ^ List of labelled states to be inserted
                        -> NBA q a l    -- ^ The original NBA
                        -> NBA q a l    -- ^ The NBA resulting from inserting states
insertStates qls a = let qs = fst <$> qls
                         ls = snd <$> qls
                         newNs = newNodes (length qs) (graph a)
                         newBimap = B.fromList (B.assocs (bimap a) ++ zip qs newNs) in
                          NBA{ states = S.union (states a) (S.fromList qs)
                             , bimap = newBimap
                             , graph = insNodes (zip newNs ls) (graph a)
                             , start = start a
                             , accept = accept a
                             }


-- | Returns list of edge-labels defined in an NBA
alphabet :: (Eq a) => NBA q a l   -- ^ The NBA
                   -> [a]         -- ^ List of edge-labels defined in NBA
alphabet a =  nub $ edgeLabel <$> labEdges (graph a)


graphNodes :: NBA q a l -> [Node]
graphNodes a = nodes (graph a)


-- | Returns label of a state in an NBA
label :: (Ord q) => q           -- The state
                 -> NBA q a l   -- The NBA the state is contained in
                 -> l           -- The label of the state
label q a = let l = lab (graph a) (bimap a B.! q) in
  case l of
    (Just x) -> x
    Nothing -> error "Node has no label." -- This should never happen.


-- | Inserts transitions into NBA.
insertTrans :: (Ord q) => [(q, a, q)]  -- ^ List of transitions
                       -> NBA q a l    -- ^ The original NBA
                       -> NBA q a l    -- ^ The NBA resulting from inserting the transitions
insertTrans ts a = let es = [(i1, i2, l) | (q1, l, q2) <- ts
                                         , let i1 = bimap a B.! q1
                                         , let i2 = bimap a B.! q2] in
                        a{graph = insEdges es (graph a)}


-- | Returns union of two Buchi automata.
-- | If the automata are limit-deterministic theb so is the returned automaton.
buchiUnion :: (Ord q) => NBA q a l                -- ^ The first NBA
                      -> NBA q a l                -- ^ The second NBA
                      -> NBA (Int, q) a l         -- ^ The union-NBA
buchiUnion a1 a2 = let (a1', a2') = (annotateStates 1 a1, annotateStates 2 a2)
                       qs2' = [(q, label q a2') | q <- S.toList (states a2')]
                       trans2' = trans a2'
                       a = insertTrans trans2' $ insertStates qs2' a1' in
                        a{ start = S.union (start a1') (start a2')
                         , accept = S.union (accept a1') (accept a2')}


-- | Returns Intersection of two Buchi automata.
--   If the automata are limit-deterministic then so is the returned automaton.
buchiIntersection :: (Ord q, Eq a) => NBA q a l              -- ^ The first NBA
                             -> NBA q a l                    -- ^ The second NBA
                             -> NBA (Int, (q, q)) a (l, l)   -- ^ The intersection-NBA
buchiIntersection a1 a2 = let
  stateList = S.toList . states
  newStates = [(q1, q2) | q1 <- (stateList a1), q2 <- (stateList a2)]
  newTrans = [((q1, q2), l, (q1', q2')) | (q1, l, q1') <- trans a1
                                        , (q2, l', q2') <- trans a2
                                        , l == l'
                                        , not (S.member q1 (accept a1))
                                        , not (S.member q2 (accept a2))]
  trans1 = [((1, (q1, q2)), l, (2, (q1', q2'))) | (q1, l, q1') <- trans a1
                                                , (S.member q1 (accept a1))
                                                , (q2, l', q2') <- trans a2
                                                , l == l']
  trans2 = [((2, (q1, q2)), l, (1, (q1', q2'))) | (q2, l, q2') <- trans a2
                                                , (S.member q2 (accept a2))
                                                , (q1, l', q1') <- trans a1
                                                , l == l']
  newLabelQs = [((q1, q2), (l1, l2)) | (q1, q2) <- newStates
                                     , let l1 = label q1 a1
                                     , let l2 = label q2 a2]
  newStart = S.fromList [(1, (q1, q2)) | q1 <- S.toList (start a1)
                                       , q2 <- S.toList (start a2)]
  newAcc = S.fromList [(2, (q1, q2)) | q1 <- stateList a1
                                     , q2 <- S.toList (accept a2)]
  a = makeNBA newLabelQs newTrans [] []
  compAutomaton = (insertTrans (trans1 ++ trans2) (buchiUnion a a)){ start = newStart
                                                                   , accept = newAcc
                                                                   }
  in
    reduceAutomaton  compAutomaton


-- | Returns Complement-automaton of a Buchi automaton.
-- Resulting automaton is always limit-deterministic.
buchiComplement :: (Ord q, Ord a) => NBA q a l       -- ^ The NBA to be complemented
                                  -> NBA (CompState q) a ()  -- ^ The complement-NBA
buchiComplement a = let
  sigma = alphabet a
  newStart = [PowerState (S.toList (start a))]
  powerTrans qs = powerSucc a qs
  trans' (PowerState qs) = [(PowerState qs', l) | (qs', l) <- powerTrans qs] ++
                           [(RankState (r, []), l) | (qs', l) <- powerTrans qs
                                                  , r <- generateLevelRankings a (S.fromList qs')]
  trans' (RankState (ranking, qs)) = let ranking' l = aSuccRanking a ranking l in
    case qs of
      [] -> [(RankState (ranking' l, evenPart (ranking' l)), l) | l <- sigma]
      _  -> [(RankState (ranking' l, [q' | q' <- powerASucc a qs l
                                         , isEven (ranking' l M.! q')]), l)
            | l <- sigma]
  (ss, newTrans) = transClosure newStart trans'
  newQs = S.toList ss
  newAcc =  [RankState (r, o) | RankState (r, o) <- newQs
                              , o == []]

  in
    makeNBA [(q, ()) | q <- newQs] newTrans newStart newAcc


-- | Reduces automaton to states reachable from initial states
reduceAutomaton :: (Ord q, Eq a) => NBA q a l   -- ^ Input automaton
                                 -> NBA q a l   -- ^ Equivalent reduced automaton
reduceAutomaton a = let (qs, newTs) = transClosure (S.toList (start a)) (succs a)
                        newStart = S.toList $ S.intersection (start a) qs
                        newAcc = S.toList $ S.intersection (accept a) qs
                        newStates = [(q, l) | q <- S.toList qs, let l = label q a] in
  makeNBA newStates newTs newStart newAcc


-- | Constructs an NBA from components
makeNBA :: (Ord q) => [(q, l)]           -- ^ List of labelled states
                   -> [(q, a, q)]        -- ^ List of labelled transitions
                   -> [q]                -- ^ List of start-states
                   -> [q]                -- ^ List of accepting states
                   -> NBA q a l          -- ^ The resulting NBA
makeNBA qs ts ss as = let lNodes = zip [1..] (snd <$> qs)
                          assoc = zip (fst <$> qs) [1..] in
                           insertTrans ts $ NBA{ states = S.fromList [q | (q, _) <- qs]
                                               , graph = mkGraph lNodes []
                                               , bimap = B.fromList assoc
                                               , start = S.fromList ss
                                               , accept = S.fromList as
                                               }


isEven :: Rank -> Bool
isEven (Rank i) = even i
isEven _ = False


evenPart :: (Ord q) => Ranking q -> [q]
evenPart ranking = [q | (q, r) <- M.assocs ranking, not (isEven r)]


oddPart :: (Ord q) => Ranking q -> [q]
oddPart ranking = [q | (q, r) <- M.assocs ranking, not (isEven r)]


-- Count number of states of lower odd rank.
countOddUnderlings :: (Ord q) => q          -- ^ Input state
                              -> Ranking q  -- ^ Input ranking
                              -> Int        -- ^ Number of states with lower odd ranking
countOddUnderlings q ranking = case ranking M.! q of
  Bot      -> 0
  (Rank i) ->  length [q' | (q', Rank i') <- M.assocs ranking,
                           not (even i'), i' < i]


roundEven :: Int -> Int
roundEven i = if even i then i else i - 1


predRanks :: (Ord q, Eq a) => q -> a -> Ranking q -> NBA q a l -> [Int]
predRanks q l ranking automaton = let qs = aPres automaton q l in
  [i | (Rank i) <- [ranking M.! q' | q' <- qs, (ranking M.! q') /= Bot]]


tighten :: (Ord q) => Ranking q -> NBA q a l -> Ranking q
tighten ranking a = M.mapWithKey newRank ranking where
  newRank _ Bot = Bot
  newRank q (Rank _) = if S.member q (accept a) then
                        Rank $ 2 * (countOddUnderlings q ranking)
                       else
                        Rank $ 2 * (countOddUnderlings q ranking) + 1


aSuccRanking :: (Ord q, Eq a) => NBA q a l -> Ranking q -> a -> Ranking q
aSuccRanking automaton ranking l = tighten ranking' automaton where
  newRank q _ = let ps = predRanks q l ranking automaton in
    if ps == [] then
      Bot
    else
      if S.member q  (accept automaton) then
        Rank $ roundEven (minimum ps)
      else
        Rank $ (minimum ps)
  ranking' = M.mapWithKey newRank ranking


generateLevelRankings :: (Ord q) => NBA q a l    -- ^ input NBA
                                 -> S.Set q      -- ^ set of states that are not mapped to Bottom
                                 -> [Ranking q]  -- ^ List of possible level rankings
generateLevelRankings a xs = let
  m = 2*(S.size (states a) - S.size (accept a))
  ranks q = if S.member q xs then
              if S.member q (accept a) then
                [Rank i | i <- [0..m], even i]
              else
                [Rank i | i <- [0..m]]
            else
              [Bot]
  rs []  = []
  rs [q] = [[(q, r)] | r <-ranks q]
  rs (q:qs) = [(q, r):rs' | r <- (ranks q), rs' <- rs qs]
  in
    map M.fromList $ rs $ S.toList (states a)

-- | Compute transitive closure starting from a list of states
transClosure :: (Ord q, Eq a) => [q]                    -- ^ list of initial states
                              -> (q -> [(q, a)])        -- ^ custom transition function
                              -> (S.Set q, [(q, a, q)]) -- the states in the closure and the transitions
transClosure qs f = (qs', nub ts) where
  (qs', ts) = fst $ closure ((S.fromList [], []), qs) f


closure :: (Ord q, Eq a) => ((S.Set q, [(q, a, q)]), [q])
                         -> (q -> [(q, a)])
                         -> ((S.Set q, [(q, a, q)]), [q])
closure ((ss, ts), qs') f = case qs' of
  (_:_) -> let newTrans = nub [(q1, a, q2) | q1 <- qs', (q2, a) <- f q1]
               newQs = [q | (_, _, q) <- newTrans, not (S.member q ss)]
               newSS = S.union (S.fromList qs') ss
               in
                closure ((newSS, ts ++ newTrans), newQs) f
  []    -> ((ss, ts), qs')
