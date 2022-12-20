import Control.Exception (Exception, throw)
import Data.Map ((!))
import qualified Data.Map (Map, fromList, adjust, lookup)

{-
 -  a CRA is a tuple A = (Q, X, Delta, I, F)
 -  Q: Finite set of states
 -  X: Finite set of registers
 -  Delta: A set of transitions: Q x sigma x U_O x Q
 -  I: Initialization Function: Q -> (X -> E_O[(/))
 -  F: Finalization Function: Q -> E_O[X]
 -}

data CRAException = InvalidState deriving Show

instance Exception CRAException

type State      = String
type Register   = String
type Registers  = Data.Map.Map Register Int
type RegUpdate  = Int -> Registers -> Registers
type Tag        = Char
data Expression = Const Int | Var String | Op 

type Q     = [State]
type X     = Data.Map.Map Register Int
type Delta = Data.Map.Map (State, Tag) (RegUpdate, State)
type I     = State -> Registers
type F     = State -> Registers -> Int

type CRA = (Q, X, Delta, I, F)

instance Show F where show _ = "F"
instance Show I where show _ = "I"
instance Show RegUpdate where show _ = "RegUpdate"

{-
thetaA :: RegUpdate
thetaA v rs = (Data.Map.adjust (\x -> x + v) "x" rs)

thetaB :: RegUpdate
thetaB v rs = (Data.Map.adjust (\y -> y + v) "y" rs)
-}

main = do
    print (let 
        states = ["p", "qa", "qb"]
        tags   = ['a', 'b']
        theta  = Data.Map.fromList [("x", 0), ("y", 0)] :: Registers
        thetaA = (\v rs -> (Data.Map.adjust (\x -> x + v) "x" rs)) :: RegUpdate
        thetaB = (\v rs -> (Data.Map.adjust (\y -> y + v) "x" rs)) :: RegUpdate
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
                                "qa" -> rs ! "x"
                                "qb" -> rs ! "y") :: F
        cra = (states, init "p", delta, init, final) :: CRA
        in
            cra)
