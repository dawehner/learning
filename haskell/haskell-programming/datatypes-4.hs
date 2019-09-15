module DataTypes where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

llength :: [a] -> Int
llength = foldr (const ((+) 1)) 0

main = do
  print $ length [1, 2, 3, 4, 5]
  print $ length [(1, 2), (2, 3), (3, 4)]
  print $ length allAwesome
  print $ length (concat allAwesome)

data NotZero a = NotZero a

createNotZero :: (Num a, Eq a) => a -> Maybe (NotZero a)
createNotZero a =
  if a == 0 then Nothing
  else Just $ NotZero a

divide :: (Num a, Fractional a) => a -> NotZero a -> a
divide a (NotZero b) = a / b
  
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1)
  else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f ab cd =
  ((snd ab, snd cd), (fst ab, fst cd))
