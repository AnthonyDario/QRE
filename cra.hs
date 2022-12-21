module CRA where

import Control.Exception (Exception, throw)
import Data.Map (Map, adjust, (!))
import qualified Data.Map (toList, fromList)

data CRAException = InvalidState deriving Show

instance Exception CRAException

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

type State      = String
type Register   = String
type Registers  = Map Register Int
type Tag        = (Char, Int)
type RegUpdate  = Int -> Registers ->  Registers
data Expression = Const Int | Var String | Op 

type Delta = Map (State, Char) (RegUpdate, State)
type I     = State -> Registers
type F     = State -> Registers -> Maybe Int

type CRA = (Delta, I, F)

instance Show F where show _ = "F"
instance Show I where show _ = "I"
instance Show RegUpdate where show _ = "RegUpdate"

{- CRA Helpers -}

-- Call the finalization function on a state
output :: CRA -> State -> Registers -> Maybe Int
output (_, _, f) s rs = f s rs

-- Update the CRA to a new state and registers
update :: CRA -> State -> Registers -> Tag -> (State, Registers)
update (d, _, _) s rs (label, val) = case d ! (s, label) of 
                                          (theta, s') -> (s', theta val rs)

{-
    Running a CRA:

    Outer function initializes the CRA to a state then calls 
    the inner function.

    The inner function will execute the state machine a character at a time
-}

run :: CRA -> State -> [Tag] -> Maybe Int
run cra@(_, i, _) s w = runInternal cra s (i s) w

runInternal :: CRA -> State -> Registers -> [Tag] -> Maybe Int
runInternal cra s rs []     = output cra s rs
runInternal cra s rs (c:cs) = case update cra s rs c of 
                                   (s', rs') -> runInternal cra s' rs' cs


