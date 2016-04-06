-- | Definition of various kinds of omega automata
module OmegaAutomata.Automata where
import qualified Data.Set as S
import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Bimap as B
import Data.List (groupBy, nub)

type State = Int

-- | Data-type for non-deterministic Buchi automata
data NBA q a l = NBA{ states :: S.Set q       -- ^ The states of the NBA
                    , bimap :: B.Bimap q Node -- ^ Bijection between states and nodes in the graph
                    , graph :: Gr l a         -- ^ The internal graph representing transitions
                    , start :: S.Set q        -- ^ The initial states in the NBA
                    , accept :: S.Set q       -- ^ The set of accepting states in the NBA
                    } deriving (Show)


-- | Returns node in internal graph corresponding to state
toNode :: (Ord q) => NBA q a l -> q -> Node
toNode a q = (bimap a) B.! q


-- | Returns state corresponding to node in internal graph
toState :: (Ord q) => NBA q a l -> Node -> q
toState a q = (bimap a) B.!> q
   -- ^ The intersection-NBA


-- | Returns successors of state with corresponding edge-labels
succs :: (Ord q) => NBA q a l   -- ^ The NBA the state belongs to
                 -> q           -- ^ The state
                 -> [(q, a)]    -- ^ Successor states and corresponding edge-label.
succs a q = (\(q, l) -> (toState a q, l)) <$> lsuc (graph a) (toNode a q)


-- | Returns predecessors of state with corresponding edge-labels
pres :: (Ord q) => NBA q a l    -- ^ The NBA the state belongs to
                -> q            -- ^ The state
                -> [(q, a)]     -- ^ Predecessors and corresponding edge-label.
pres a q = (\(q, l) -> (toState a q, l)) <$> lpre (graph a) (toNode a q)


-- | Returns list of successor-states for a given state and edge-label
aSuccs :: (Ord q) => NBA q a l   -- ^ The NBA the state belongs to
                  -> q           -- ^ The state
                  -> a           -- ^ The edge-label
                  -> [q]         -- ^ List of successors for state and edge-label.
aSuccs a q b = [toState a q' | (q', b) <- lsuc (graph a) (toNode a q)]


-- | Returns list of predecessor-states for a given state and edge-label
aPres :: (Ord q) => NBA q a l   -- ^ The NBA the state belongs to
                 -> q           -- ^ The state
                 -> a           -- ^ The edge-label
                 -> [q]         -- ^ List of predecessors for state and edge-label.
aPres a q b = [toState a q' | (q', b) <- lpre (graph a) (toNode a q)]


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
insertTrans ts a = let edges = [(i1, i2, l) | (q1, l, q2) <- ts
                                            , let i1 = bimap a B.! q1
                                            , let i2 = bimap a B.! q2] in
                        a{graph = insEdges edges (graph a)}


-- | Returns union of two Buchi automata.
-- | If the automata are limit-deterministic, the so is the returned automaton.
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
--   If the automata are limit-deterministic, then so is the returned automaton.
buchiIntersection :: (Ord q) => NBA q a l                    -- ^ The first NBA
                             -> NBA q a l                    -- ^ The second NBA
                             -> NBA (Int, (q, q)) a (l, l)   -- ^ The intersection-NBA
buchiIntersection a1 a2 = let stateList = S.toList . states
                              newStates = [(q1, q2) | q1 <- (stateList a1), q2 <- (stateList a2)]
                              newTrans = [((q1, q2), l, (q1', q2')) | (q1, l, q1') <- trans a1
                                                                    , (q2, l, q2') <- trans a2
                                                                    , not (S.member q1 (accept a1))
                                                                    , not (S.member q2 (accept a2))]
                              trans1 = [((1, (q1, q2)), l, (2, (q1', q2'))) | (q1, l, q1') <- trans a1
                                                                            , (S.member q1 (accept a1))
                                                                            , (q2, l, q2') <- trans a2]
                              trans2 = [((2, (q1, q2)), l, (1, (q1', q2'))) | (q2, l, q2') <- trans a2
                                                                            , (S.member q2 (accept a2))
                                                                            , (q1, l, q1') <- trans a1]
                              newLabelQs = [((q1, q2), (l1, l2)) | (q1, q2) <- newStates
                                                                 , let l1 = label q1 a1
                                                                 , let l2 = label q2 a2]
                              newStart = S.fromList [(1, (q1, q2)) | q1 <- S.toList (start a1)
                                                                   , q2 <- S.toList (start a2)]
                              newAcc = S.fromList [(2, (q1, q2)) | q1 <- stateList a1
                                                                 , q2 <- S.toList (accept a2)]
                              a = makeNBA newLabelQs newTrans [] [] in
                                (insertTrans (trans1 ++ trans2) (buchiUnion a a)){ start = newStart
                                                                                 , accept = newAcc
                                                                                 }

-- | Returns Complement-automaton of a Buchi automaton.
-- Resulting automaton is always limit-deterministic.
buchiComplement :: (Ord q) => NBA q a l   -- ^ The NBA to be complemented
                           -> NBA q a l   -- ^ The complement-NBA
buchiComplement a = undefined


-- | Constructs an NBA from components
makeNBA :: (Ord q) => [(q, l)]           -- ^ List of labelled states
                   -> [(q, a, q)]        -- ^ List of labelled transitions
                   -> [q]                -- ^ List of start-states
                   -> [q]                -- ^ List of accepting states
                   -> NBA q a l          -- ^ The resulting NBA
makeNBA qs ts ss as = let lNodes = zip [1..] (snd <$> qs)
                          assoc = zip (fst <$> qs) [1..] in
                           insertTrans ts $ NBA{ states = S.fromList [q | (q, l) <- qs]
                                               , graph = mkGraph lNodes []
                                               , bimap = B.fromList assoc
                                               , start = S.fromList ss
                                               , accept = S.fromList as
                                               }
