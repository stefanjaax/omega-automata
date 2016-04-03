-- | Definition of various kinds of omega automata
module OmegaAutomata.Automata where
import Data.Set (Set, fromList, toList)
import Data.Graph.Inductive (Node, lsuc, Gr, mkGraph)
import Control.Applicative ((<$>))


type State = Node
type Trans alphabet =  (State, alphabet, State)
data AccCond alphabet = Fin [Trans alphabet] | Inf [Trans alphabet]
data PointedGraph alphabet label = PointedGraph
                                   { digraph :: Gr label alphabet
                                   , start :: Set State
                                   }
                                   deriving Show


outTrans :: PointedGraph alphabet label -> State -> [Trans alphabet]
outTrans pg q = let dg = digraph pg
                    a_succ = lsuc dg q in
                  [(q, a, q') | (q', a) <- a_succ]



statesToOutTrans :: PointedGraph alphabet label -> [State] -> [Trans alphabet]
statesToOutTrans pg qs = concat $ outTrans pg <$> qs



data Automaton acc alphabet label = Automaton
                                    { graph :: PointedGraph alphabet label
                                    , accept :: acc
                                    }
                                    deriving Show


newtype NBAccCond = NBAccCond (Set State)
newtype GNBAccCond = GNBAccCond [[State]]
newtype TNBAccCond alphabet = TNBAccCond [Trans alphabet]
newtype TGNBAccCond alphabet = TGNBAccCond [[Trans alphabet]]
newtype NRAccCond = NRAccCond [([State], [State])]
newtype TNRAccCond alphabet = TNRAccCond [([Trans alphabet], [Trans alphabet])]


type OmegaAutomaton alphabet label = Automaton [[AccCond alphabet]] alphabet label
newtype NBA alphabet label = NBA (Automaton NBAccCond alphabet label)
newtype TNBA alphabet label = TNBA (Automaton (TNBAccCond alphabet) alphabet label)
newtype GNBA alphabet label = GNBA (Automaton GNBAccCond alphabet label)
newtype TGNBA alphabet label = TGNBA (Automaton (TGNBAccCond alphabet) alphabet label)
newtype NRA alphabet label = NRA (Automaton NRAccCond alphabet label)
newtype TNRA alphabet label = TNRA (Automaton (TNRAccCond alphabet) alphabet label)


class TransBasedAccCond a where
  convTransToAccCond :: a alphabet -> [[AccCond alphabet]]



class StateBasedAccCond a where
  convStatesToAccCond :: a -> PointedGraph alphabet label -> [[AccCond alphabet]]



class OmegaRegular a where
  convToOmegaAutomaton :: a alphabet label -> OmegaAutomaton alphabet label


makeNBA :: [(State, label)] ->
           [(State, State, alphabet)] ->
           [State] ->
           [State] ->
           NBA alphabet label
makeNBA qs ts ss as = NBA $ Automaton{ graph = PointedGraph{ digraph = mkGraph qs ts
                                                           , start = fromList ss
                                                           }
                                     , accept = NBAccCond (fromList as)
                                     }


makeTNBA :: [(State, label)] ->
            [(State, State, alphabet)] ->
            [State] ->
            [(State, State, alphabet)] ->
            TNBA alphabet label
makeTNBA qs ts ss as = TNBA $ Automaton{ graph = PointedGraph{ digraph = mkGraph qs ts
                                                           , start = fromList ss
                                                           }
                                       , accept = TNBAccCond [(q1,a,q2) | (q1,q2,a) <- as]
                                       }


instance OmegaRegular NBA where
  convToOmegaAutomaton (NBA (Automaton pg a)) = Automaton { graph = pg
                                                          , accept = convStatesToAccCond a pg
                                                          }


instance OmegaRegular GNBA where
  convToOmegaAutomaton (GNBA (Automaton pg a)) = Automaton { graph = pg
                                                           , accept = convStatesToAccCond a pg
                                                           }


instance OmegaRegular NRA where
  convToOmegaAutomaton (NRA (Automaton pg a)) = Automaton { graph = pg
                                                          , accept = convStatesToAccCond a pg
                                                          }


instance  OmegaRegular TGNBA where
  convToOmegaAutomaton (TGNBA (Automaton pg a)) = Automaton { graph = pg
                                                            , accept = convTransToAccCond a
                                                            }


instance OmegaRegular TNRA where
  convToOmegaAutomaton (TNRA (Automaton pg a)) = Automaton { graph = pg
                                                           , accept = convTransToAccCond a
                                                           }


instance TransBasedAccCond TNBAccCond where
  convTransToAccCond (TNBAccCond ts) = [[Inf ts]]


instance TransBasedAccCond TGNBAccCond where
  convTransToAccCond (TGNBAccCond ts) = [[Inf fs] | fs <- ts]


instance TransBasedAccCond TNRAccCond where
  convTransToAccCond (TNRAccCond  ts) = [[Inf infs, Fin fins] | (infs, fins) <- ts]


instance StateBasedAccCond NBAccCond where
  convStatesToAccCond (NBAccCond qs) pg = [[Inf (statesToOutTrans pg (toList qs))]]


instance StateBasedAccCond GNBAccCond where
  convStatesToAccCond (GNBAccCond qs) pg = [[Inf (statesToOutTrans pg fs) | fs <- qs]]


instance StateBasedAccCond NRAccCond where
  convStatesToAccCond (NRAccCond qs) pg = [[Inf (statesToOutTrans pg infs),
                                            Fin (statesToOutTrans pg  fins)]
                                           | (infs, fins) <- qs]
