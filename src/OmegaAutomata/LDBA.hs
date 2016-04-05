-- | Manipulation of Limit-deterministic Buchi automata
module OmegaAutomata.LDBA where
import OmegaAutomata.Automata
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.TransClos (trc)
import qualified Data.Set as S
import qualified  Data.Map as  M
import qualified Data.Bimap as B
import Data.List (nub)

type PowerPair q = ([q], [q])

isLimitDeterministic :: (Ord q) => NBA q a l -> Bool
isLimitDeterministic a = let g = graph a
                             fs = accept a
                             ls = [q2 | (q1, q2, _) <- (labEdges . trc) g, S.member (toState a q1) fs]
                             detSucc q = outdeg g q <= noNodes g  in
                              all detSucc ls


pairTrans :: (Ord q, Ord a) => NBA q a l -> PowerPair q -> [(PowerPair q, a)]
pairTrans a (qs1, qs2) = let ts1 = powerSucc a qs1
                             acc1 = [f | f <- qs1, S.member f (accept a)]
                             qs2' l = if qs1 == qs2 then
                                        powerASucc a acc1 l
                                      else
                                        powerASucc a (qs2 ++ [f | f <- acc1, not (f `elem` qs2)]) l
                             in
                              [((qs1', nub (qs2' l)), l) | (qs1', l) <- ts1]


pairTransClosure :: (Ord q, Ord a) => NBA q a l -> [PowerPair q] -> [(PowerPair q, a, PowerPair q)]
pairTransClosure a ps = [(ps1, l, ps2) | (ps1, vs) <- M.assocs (genPowerSet a (M.fromList []) ps)
                                       , (ps2, l) <- vs]


genPowerSet ::  (Ord q, Ord a) =>
                NBA q a l ->
                M.Map (PowerPair q) [(PowerPair q, a)] ->
                [PowerPair q] -> M.Map (PowerPair q) [(PowerPair q, a)]
genPowerSet a m (p:ps) = let psl' = (pairTrans a p)
                             ps' = [p' | (p', l) <- psl', not (M.member p' m)] in
                              if M.member p m then
                                genPowerSet a m ps
                              else
                                genPowerSet a (M.insert p psl' m) (ps' ++ ps)
genPowerSet a m [] = m


liftStateToPair :: (Ord q, Ord a) => NBA q a l -> NBA (PowerPair q) a ([l], [l])
liftStateToPair a = let liftToPair q = ([q], []) in
  a{ states = S.map liftToPair (states a)
   , bimap = B.map liftToPair (bimap a)
   , graph = nmap (\l -> ([l], [])) (graph a)
   , start = S.map liftToPair (start a)
   , accept = S.map liftToPair (accept a)
   }


labels :: (Ord q) => [q] -> NBA q a l -> [l]
labels qs a = [label q a | q <- qs]


toLDBA :: (Ord q, Ord a) =>  NBA q a l -> NBA (Int, PowerPair q) a ([l], [l])
toLDBA a = let ts = pairTransClosure a [([f],[f]) | f <- S.toList (accept a)]
               ps = concat [[p1, p2] | (p1, a, p2) <- ts]
               ls = [(labels q1 a, labels q2 a) | (q1, q2) <- ps]
               as = [(qs1, qs2) | (qs1, qs2) <- ps, qs1 == qs2]
               part2 = makeNBA (zip ps ls) ts [] as
               trans12 = [((1, (qs, [])), l, (2, ([f], [f]))) | f <- S.toList (accept a)
                                                   , (qs, l) <- combine (pres a f)]
               part1 = (liftStateToPair a){accept = S.fromList []}
               in
                insertTrans trans12 $ buchiUnion part1 part2
