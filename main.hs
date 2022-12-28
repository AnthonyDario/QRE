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

                                    |
                                    | @
                                    v
                                 +------+
                       a | @a    |      |    b | @b
                    +------------|  p   |------------+
                    |            |      |            |
                    |            +------+            |
                    v                                v
                +------+          b | @b          +------+
           /--->|      |------------------------->|      |----\
    a | @a |    | qa|x |                          | qb|y |    | b | @b
           \----|      |<-------------------------|      |<---/
                +------+          a | @a          +------+

         | x := 0                | x := x + 1             | x := x
    @ = <|                 @a = <|                  @b = <|
         | y := 0                | y := y                 | y := y + 1

-}
cra3 :: CRA Char Int
cra3 = let theta  = Data.Map.fromList [("x", 0), ("y", 0)]
           thetaA = (\v rs -> (adjust (\x -> x + v) "x" rs))
           thetaB = (\v rs -> (adjust (\y -> y + v) "y" rs))
           delta  = Data.Map.fromList [(("p",  Tag 'a'), (thetaA, "qa")),
                                       (("p",  Tag 'b'), (thetaB, "qb")),
                                       (("qa", Tag 'a'), (thetaA, "qa")),
                                       (("qa", Tag 'b'), (thetaB, "qb")),
                                       (("qb", Tag 'a'), (thetaA, "qa")),
                                       (("qb", Tag 'b'), (thetaB, "qb"))]
           init   = ("p", theta)
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

         +------+          a | @a          +------+
      @  |      |------------------------->|      |----\
    ---->| p|y  |                          |  q   |    | a | @a
         |      |<-------------------------|      |<---/
         +------+          # | @#          +------+

         | x := 0                | x := x + val           | x := 0
    @ = <|                 @a = <|                  @b = <|
         | y := 0                | y := y                 | y := max(y, x)

-}
cra5 = let theta  = Data.Map.fromList [("x", 0), ("y", 0)]
           thetaA = (\v rs -> (adjust (\x -> x + v) "x" rs))
           thetaH = (\v rs -> (adjust (\x -> 0) "x" 
                                      (adjust (\_ -> max (rs ! "x") (rs ! "y")) "y" rs)))
           delta  = Data.Map.fromList [(("p", Tag 'a'), (thetaA, "q")),
                                       (("q", Tag 'a'), (thetaA, "q")),
                                       (("q", Tag '#'), (thetaH, "p"))]
           init   = ("p", theta) 
           final  = (\s rs -> case s of
                                   "p" -> Just (rs ! "y")
                                   _   -> Nothing)
       in
           (delta, init, final)

main = do

    let asbs   = toStream [('a', 4), ('a', 5), ('b', 10), ('a', 3), ('b', 4)]
    let blocks = toStream [('a', 2), ('a', 4), ('#', 8), ('a', 1), ('a', 1), ('a', 2), ('#', 1)]
    let as     = toStream [('a', 10), ('a', 10)] 
    let wilds  = toStream [('a', 10), ('i', 10)]

    -- CRA tests (Examples from Streamable Regular Transductions)
    putStr ("\nCRA tests -----\n")
    print  (run cra3 asbs)
    print  (run cra5 blocks)

    -- Atom tests
    putStr ("\nAtom tests ----\n")
    print  (run (compile (Atom 'a' 5)) as)    -- Atom matches on single elements items
    print  (run (compile (Atom 'a' 5)) wilds) -- Atom doesn't match anything else

    -- Empty tests
    putStr ("\nEmpty tests ----\n")
    print  (run (compile (Empty 5)) (toStream [])) -- Empty matches an empty stream
    print  (run (compile (Empty 5)) wilds)         -- Empty doesn't match a wildcard
