-- Standard Lib
import Control.Exception (Exception, throw)
import Data.Map (Map, adjust, (!))
import qualified Data.Map (toList, fromList)

-- Project Files
import CRA
import QRE

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
cra3 = let theta  = Data.Map.fromList [("x", 0), ("y", 0)]
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
           (delta, init, final)

{- 
    Example 5 
    
    A copyless DCRA.  The rate is (a+#)*.  Outputs the block with the maximum
    sum, where a block is a series of a's separated by a #.
-}
cra5 = let theta  = Data.Map.fromList [("x", 0), ("y", 0)]
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
           (delta, init, final)

main = do
    print (run cra3 "p" [('a', 4), ('a', 5), ('b', 10), ('a', 3), ('b', 4)])
    print (run cra5 "p" [('a', 2), ('a', 4), ('#', 8), ('a', 1), ('a', 1), ('a', 2), ('#', 1)])
    print (run (compile (Atom 'a' 5)) "p" [('a', 10), ('a', 10)])
