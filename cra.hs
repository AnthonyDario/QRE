module CRA where

import Control.Exception (Exception, throw)
import Data.Map (Map, adjust, (!))
import qualified Data.Map (toList, fromList, lookup)

{-
   The CRA type    
   ----------------------------------
   a CRA is a tuple A = (Q, X, Delta, I, F)
   - Q: Finite set of states
   - X: Finite set of registers
   - Delta: A set of transitions: Q x sigma x U_O x Q
   - I: Initialization Function: Q -> (X -> E_O[(/))
   - F: Finalization Function: Q -> E_O[X]
-}

type State         = String
type Register      = String
type Registers   v = Map Register v
data Label     t   = Tag t | Wildcard deriving (Eq, Ord, Show)
type Element   t v = (Label t, v)
type RegUpdate t v = v -> Registers v -> Registers v

type Delta t v = Map (State, Label t) (RegUpdate t v, State) -- Transition function
type I       v = (State, Registers v)                        -- Initialization function
type F       v = State -> Registers v -> Maybe v             -- Finalization (lifting) function

type CRA t v = (Delta t v, I v, F v)

instance Show (F v)           where show _ = "F"
instance Show (RegUpdate t v) where show _ = "RegUpdate"

{- CRA Helpers -}

-- Call the finalization function on a state
output :: CRA t v -> State -> Registers v -> Maybe v
output (_, _, f) s rs = f s rs

-- Update the CRA to a new state and registers
update :: (Ord t) => CRA t v -> State -> Registers v -> Element t v -> (State, Registers v)
update (d, _, _) s rs e@(label, val) = case Data.Map.lookup (s, label) d of 
                                            Just (theta, s') -> (s', theta val rs)
                                            Nothing -> case Data.Map.lookup (s, Wildcard) d of
                                                            Just (theta, s') -> (s', theta val rs)
                                                            Nothing          -> error "Invalid Transition"

{-
    Running a CRA:

    Outer function initializes the CRA to a state then calls 
    the inner function.

    The inner function will execute the state machine a character at a time
-}

run :: (Ord t) => CRA t v -> [Element t v] -> Maybe v
run cra@(_, i, _) w = runInternal cra init rs w
                      where (init, rs) = i

runInternal :: (Ord t) => CRA t v -> State -> Registers v -> [Element t v] -> Maybe v
runInternal cra s rs []     = output cra s rs
runInternal cra s rs (c:cs) = case update cra s rs c of 
                                   (s', rs') -> runInternal cra s' rs' cs
