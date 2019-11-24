module Main where

-- 1.

myId :: a -> a
myId x = x

-- 2. 
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = g . f

-- 3.

composeTest = do
  print $ compose myId myId 2
  print $ compose (+1) (+3) 2

-- 4.
--
-- Category: WWW
-- objects: sites
-- links: morphisms
-- ID: link to itself

-- 5.
-- Category: facebook
-- No, because bi-directional relationship.

-- 6.
-- TODO
-- Directed graph is a category if the ???
--

main = do
  composeTest

