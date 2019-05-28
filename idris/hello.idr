module Main

import Data.Vect

data LimitedQueue n a = LimitedQueue (Vect n a)

push : a -> LimitedQueue n a -> LimitedQueue n a
 

always : a  ->  b -> a
always x _ = x

vectToList : Vect n a -> List a
vectToList [] = []
vectToList (x :: xs) = x :: vectToList xs

sparkString : Int -> String
sparkString 0 = "▁"
sparkString 1 = "▂"
sparkString 2 = "▃"
sparkString 3 = "▄"
sparkString 4 = "▅"
sparkString 5 = "▆"
sparkString _ = "█"

sparkPos : Int -> Int -> Int -> String
sparkPos min max val = sparkString $ 7 * (divInt (val -  min) (max - min))

spark : Vect n Int -> Vect n String
spark [] = []
spark xs =
  let
    min_ = foldl min 10000000 xs
    max_ = foldl max 0 xs
  in
    map (sparkPos min_ max_) xs

main : IO ()
main = putStr $ unwords $ vectToList $ spark [1, 23, 4]
