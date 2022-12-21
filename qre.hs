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
                        thetaS = (\_ _ -> Data.Map.fromList [("x", v)]) -- To conform with the interface, I should unify these things
                        delta  = Data.Map.fromList [(("p", '*'), (thetaS, "q")),
                                                    (("q", '*'), (thetaS, "q"))]
                        init   = (\s -> case s of
                                             "p" -> theta
                                             _   -> throw InvalidState)
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
