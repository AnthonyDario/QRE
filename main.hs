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
type RegUpdate  = Tag -> Registers ->  Registers
data Expression = Const Int | Var String | Op 

type Q     = [State]
type X     = Registers
type Delta = Map (State, Char) (RegUpdate, State)
type I     = State -> Registers
type F     = State -> Registers -> Maybe Int

type CRA = (Q, X, Delta, I, F)

instance Show F where show _ = "F"
instance Show I where show _ = "I"
instance Show RegUpdate where show _ = "RegUpdate"

{- CRA Helpers -}

-- Call the finalization function on a state
output :: CRA -> State -> Registers -> Maybe Int
output (_, _, _, _, f) s rs = f s rs

-- Update the CRA to a new state and registers
update :: CRA -> State -> Registers -> (Char, Int) -> (State, Registers)
update (_, _, d, _, _) s rs e@(tag, _) = case d ! (s, tag) of 
                                              (theta, s') -> (s', theta e rs)

{-
    Running a CRA:

    Outer function initializes the CRA to a state then calls 
    the inner function.

    The inner function will execute the state machine a character at a time
-}

run :: CRA -> State -> [Tag] -> Maybe Int
run cra@(_, _, _, i, _) s w = runInternal cra s (i s) w

runInternal :: CRA -> State -> Registers -> [Tag] -> Maybe Int
runInternal cra s rs []     = output cra s rs
runInternal cra s rs (c:cs) = case update cra s rs c of 
                                   (s', rs') -> runInternal cra s' rs' cs


{-
    Examples from the Paper
    ------------------------------
-}

{- 
    Example 3

    A copyless DCRA.  Outputs the sum of all values labelled with the label of
    the last seen element

-}
cra3 :: CRA
cra3 = let states = ["p", "qa", "qb"] -- Currently unused...
           tags   = ['a', 'b']        -- Also unused...
           theta  = Data.Map.fromList [("x", 0), ("y", 0)]
           thetaA = (\(t, v) rs -> (adjust (\x -> x + v) "x" rs))
           thetaB = (\(t, v) rs -> (adjust (\y -> y + v) "y" rs))
           delta  = Data.Map.fromList [(("p",  'a'), (thetaA, "qa")),
                                       (("p",  'b'), (thetaB, "qb")),
                                       (("qa", 'a'), (thetaA, "qa")),
                                       (("qa", 'b'), (thetaB, "qb")),
                                       (("qb", 'a'), (thetaA, "qa")),
                                       (("qb", 'b'), (thetaB, "qb"))]
           init   = (\s -> case s of 
                                "p" -> theta
                                _   -> throw InvalidState)
           final  = (\s rs -> case s of
                                   "qa" -> Just (rs ! "x")
                                   "qb" -> Just (rs ! "y")
                                   _    -> Nothing)
        in
           (states, init "p", delta, init, final)

{- 
    Example 5 
    
    A copyless DCRA.  The rate is (a+#)*.  Outputs the block with the maximum
    sum, where a block is a series of a's separated by a #.
-}
cra5 = let states = ["p", "q"]
           tags   = ['a', '#']
           theta  = Data.Map.fromList [("x", 0), ("y", 0)]
           thetaA = (\(_, v) rs -> (adjust (\x -> x + v) "x" rs))
           thetaH = (\(t, v) rs -> (adjust (\x -> 0) "x" 
                                           (adjust (\_ -> max (rs ! "x") (rs ! "y")) "y" rs)))
           delta  = Data.Map.fromList [(("p", 'a'), (thetaA, "q")),
                                       (("q", 'a'), (thetaA, "q")),
                                       (("q", '#'), (thetaH, "p"))]
           init   = (\s -> case s of
                                "p" -> theta
                                _   -> throw InvalidState)
           final  = (\s rs -> case s of
                                   "p" -> Just (rs ! "y")
                                   _   -> Nothing)
       in
           (states, init "p", delta, init, final)

main = do
    print (run cra3 "p" [('a', 4), ('a', 5), ('b', 10), ('a', 3), ('b', 4)])
    print (run cra5 "p" [('a', 2), ('a', 4), ('#', 8), ('a', 1), ('a', 1), ('a', 2), ('#', 1)])
