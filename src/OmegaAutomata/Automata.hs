-- | Definition of various kinds of omega automata
module OmegaAutomata.Automata where
import qualified Data.Set as S
import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Bimap as B

type State = Int

data NBA q a l = NBA{ states :: S.Set q
                    , bimap :: B.Bimap q Node
                    , graph :: Gr l a
                    , start :: S.Set q
                    , accept :: S.Set q
                    }

toNode :: (Ord q) => NBA q a l -> q -> Node
toNode a q = (bimap a) B.! q


toState :: (Ord q) => NBA q a l -> Node -> q
toState a q = (bimap a) B.!> q


succ :: (Ord q) => NBA q a l -> q -> [q]
succ a q = (toState a . fst) <$> lsuc (graph a) (toNode a q)


pre :: (Ord q) => NBA q a l -> q -> [q]
pre a q = (toState a . fst) <$> lpre (graph a) (toNode a q)


aSucc :: (Ord q) => NBA q a l -> q -> a -> [q]
aSucc a q b = [toState a q' | (q', b) <- lsuc (graph a) (toNode a q)]


aPre :: (Ord q) => NBA q a l -> q -> a -> [q]
aPre a q b = [toState a q' | (q', b) <- lpre (graph a) (toNode a q)]


trans :: (Ord q) => NBA q a l -> [(q, a, q)]
trans a = [(q1, l, q2) | (i1, i2, l) <- labEdges (graph a)
                       , let q1 = (bimap a) B.!> i1
                       , let q2 = (bimap a) B.!> i2]


annotateStates :: (Ord q, Ord i) => i -> NBA q a l -> NBA (i, q) a l
annotateStates i a = let annotate = S.map (\x -> (i, x)) in
                      NBA{ states = annotate (states a)
                         , bimap = B.fromList [((i, q), n) | (q, n) <- (B.assocs (bimap a))]
                         , graph = graph a
                         , start = annotate (start a)
                         , accept = annotate (accept a)
                         }


insertStates :: (Ord q) => [(q, l)] -> NBA q a l -> NBA q a l
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


label :: (Ord q) => q -> NBA q a l -> l
label q a = let l = lab (graph a) (bimap a B.! q) in
  case l of
    (Just x) -> x
    Nothing -> error "Node has no label."


insertTrans :: (Ord q) => [(q, a, q)] -> NBA q a l -> NBA q a l
insertTrans ts a = let edges = [(i1, i2, l) | (q1, l, q2) <- ts
                                            , let i1 = bimap a B.! q1
                                            , let i2 = bimap a B.! q2] in
                        a{graph = insEdges edges (graph a)}


buchiUnion :: (Ord q) => NBA q a l -> NBA q a l -> NBA (Int, q) a l
buchiUnion a1 a2 = let (a1', a2') = (annotateStates 1 a1, annotateStates 2 a2)
                       qs2' = [(q, label q a2') | q <- S.toList (states a2')]
                       trans2' = trans a2'
                       a = insertTrans trans2' $ insertStates qs2' a1' in
                        a{ start = S.union (start a1') (start a2')
                         , accept = S.union (accept a1') (accept a2')}


buchiIntersection :: (Ord q) => NBA q a l -> NBA q a l -> NBA (Int, (q, q)) a (l, l)
buchiIntersection a1 a2 = let stateList = S.toList . states
                              newStates = zip (stateList a1) (stateList a2)
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
                              newStart = S.fromList [(1, (q1, q2)) | q1 <- S.toList (states a1)
                                                                   , q2 <- S.toList (states a2)]
                              newAcc = S.fromList [(2, (q1, q2)) | q1 <- stateList a1
                                                                 , q2 <- S.toList (accept a2)]
                              a = makeNBA newLabelQs newTrans [] [] in
                                (insertTrans (trans1 ++ trans2) (buchiUnion a a)){ start = newStart
                                                                                 , accept = newAcc
                                                                                 }


buchiComplement :: (Ord q) => NBA q a l -> NBA q a l
buchiComplement a = undefined


makeNBA :: (Ord q) => [(q, l)] -> [(q, a, q)] -> [q] -> [q] -> NBA q a l
makeNBA qs ts ss as = let lNodes = zip [1..] (snd <$> qs)
                          assoc = zip (fst <$> qs) [1..] in
                           insertTrans ts $ NBA{ states = S.fromList []
                                               , graph = mkGraph lNodes []
                                               , bimap = B.fromList assoc
                                               , start = S.fromList ss
                                               , accept = S.fromList as
                                               }
