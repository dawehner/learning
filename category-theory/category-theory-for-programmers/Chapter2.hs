module Main where

import Control.Monad.State.Lazy
import qualified Data.Map as Map

-- 1.
memoize :: (a -> b) -> State (Map.Map a b) (a -> b)
memoize f = do
  map <- get
  \x -> case Map.lookup x of
    Just y -> lift y
    Nothing -> 
      let
        y = f x
        map' = insert a y
      in
        
        
    
    
    

main = print "ok, cool"
