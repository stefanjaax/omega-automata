-- | Manipulation of Limit-deterministic Buchi automata
module OmegaAutomata.LDBA where
import OmegaAutomata.Automata
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.TransClos (trc)
import Data.Set as S

isLimitDeterministic :: NBA alphabet label -> Bool
isLimitDeterministic (NBA a) = let g = (digraph . graph) a
                                   (NBAccCond as) = accept a
                                   ls = [q2 | (q1, q2, _) <- (labEdges . trc) g, S.member q1 as]
                                   detSucc q = outdeg g q <= noNodes g  in
                                    all detSucc ls


ldUnion :: NBA alphabet label -> NBA alphabet label -> NBA alphabet label
ldUnion (NBA a1) (NBA a2) = undefined


ldIntersection :: NBA alphabet label -> NBA alphabet label -> NBA alphabet label
ldIntersection (NBA a1) (NBA a2) = undefined


ldComplement :: NBA alphabet label -> NBA alphabet label
ldComplement (NBA a) = undefined


toLDBA :: NBA alphabet label -> NBA alphabet label
toLDBA (NBA a) = undefined


makeStartUnique :: Automaton acc alphabet (Maybe label) -> Automaton acc alphabet (Maybe label)
makeStartUnique a = let dg =  digraph $ graph a
                        q0 = head $ newNodes 1 dg
                        newStart = [(q0, Nothing)]
                        links = [(q0, q, b) |
                                  s <- toList ((start . graph) a)
                                , (b, q) <- lneighbors dg s]
                        newGraph = PointedGraph{ digraph = insEdges links $ insNodes newStart $ dg
                                               , start = fromList [q0]}
                        in
                          Automaton{ graph = newGraph
                                   , accept = accept a
                                   }


intersectBuchi :: NBA alphabet label -> NBA alphabet label -> NBA alphabet label
intersectBuchi (NBA a1) (NBA a2) = undefined

