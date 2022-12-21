module QRE where

-- Standard Lib
import Control.Exception (Exception, throw)
import Data.Map (Map, adjust, (!))
import qualified Data.Map (toList, fromList)

-- Project Files
import CRA

data QRE a b = Empty b | Atom a b

{-
    empty(5): Matches on the empty stream and returns 5

        +-----+         +-----+
     @  |     |  * | @  |     |---+
    --->| p|x |-------->|  q  |   | * | @  
        |     |         |     |<--+
        +-----+         +-----+
    
    @ = x := 5
-}

compile :: QRE Char Int -> CRA
compile (Empty v) = let theta  = Data.Map.fromList [("x", v)]
                        thetaS = (\_ rs -> Data.Map.fromList [("x", v)]) -- To conform with the interface, I should unify these things
                        delta  = Data.Map.fromList [(("p", Wildcard), (thetaS, "q")),
                                                    (("q", Wildcard), (thetaS, "q"))]
                        init   = ("p", theta)
                        final  = (\s rs -> case s of
                                                "p" -> Just (rs ! "x")
                                                _   -> Nothing)
                    in
                        (delta, init, final)

{-
    Atom(a, 5) : Returns 5 when the tag is 'a'

         * | @*
         /---\
         |   |
         |   v         
        +-----+ a | @* +-----+
     @  |     |------->|     |---+
    --->|  p  |        | q|x |   | a | @* 
        |     |<-------|     |<--+
        +-----+ * | @* +-----+

    Where @ represents register update functions (theta) and * represents any
    character not 'a' in the alphabet (sigma).

    @  = x := 5
    @* = x := x
-}     

compile (Atom t v) = let theta  = Data.Map.fromList [("x", v)]
                         thetaS = (\v rs -> (adjust (\x -> x) "x" rs))
                         delta  = Data.Map.fromList [(("p", Tag t  ), (thetaS, "q")),
                                                     (("p", Wildcard), (thetaS, "p")),
                                                     (("q", Tag t  ), (thetaS, "q")),
                                                     (("q", Wildcard), (thetaS, "p"))]
                         init   = ("p", theta)
                         final  = (\s rs -> case s of
                                                 "q" -> Just (rs ! "x")
                                                 _   -> Nothing)
                         in
                            (delta, init, final)
