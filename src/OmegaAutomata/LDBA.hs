-- | Manipulation of Limit-deterministic Buchi automata
module OmegaAutomata.LDBA where
import OmegaAutomata.Automata
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.TransClos (trc)
import qualified Data.Set as S
import qualified  Data.Map as  M
import Data.Maybe (maybeToList)

isLimitDeterministic :: (Ord q) => NBA q a l -> Bool
isLimitDeterministic a = let g = graph a
                             as = accept a
                             ls = [q2 | (q1, q2, _) <- (labEdges . trc) g, S.member (toState a q1) as]
                             detSucc q = outdeg g q <= noNodes g  in
                              all detSucc ls


toLDBA :: NBA q a l -> NBA q a l
toLDBA a = undefined
