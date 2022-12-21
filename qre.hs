module QRE where

-- Standard Lib
import Control.Exception (Exception, throw)
import Data.Map (Map, adjust, (!))
import qualified Data.Map (toList, fromList)

-- Project Files
import CRA

data QRE a b = Atom a b

{-
    Atom (a, 5) : Returns 5 when the tag is 'a'

          */@*
         /---\
         |   |
         |   v         
        +-----+  a/@*  +-----+
     @  |     |------->|     |---+
    --->|  p  |        | q|x |   | a/@*   
        |     |<-------|     |<--+
        +-----+  */@*  +-----+

    Where @ represents register update functions (theta) and * represents any
    character not 'a' in the alphabet (sigma).

    @  = x := 5
    @* = x := x
-}     

compile :: QRE Char Int -> CRA
compile (Atom t v) = let theta  = Data.Map.fromList [("x", v)]
                         thetaS = (\(t, v) rs -> (adjust (\x -> x) "x" rs))
                         delta  = Data.Map.fromList [(("p", t  ), (thetaS, "q")),
                                                     (("p", '*'), (thetaS, "p")),
                                                     (("q", t  ), (thetaS, "q")),
                                                     (("q", '*'), (thetaS, "p"))]
                         init   = (\s -> case s of
                                              "p" -> theta
                                              _   -> throw InvalidState)
                         final  = (\s rs -> case s of
                                                 "q" -> Just (rs ! "x")
                                                 _   -> Nothing)
                         in
                            (delta, init, final)

