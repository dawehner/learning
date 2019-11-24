module Main where

import Data.Function ((&))
import Control.Monad (liftM4)


type Grid = [Double]

dropEnd :: Int -> [a] -> [a]
dropEnd n = drop n . reverse

timeStep :: Double -> Double -> Double -> Grid -> Grid -> (Grid, Grid)
timeStep dt dx cs gRho gRhoU =
   -- Calculate 
  where
    -- Advection without source terms on the cells borders
    q1_n12 = zip gRho gRhoU
      & fmap (\(rho u) -> rho - dt * dx * rho * u)
    q2_n12 = zip gRho gRhoU
      & fmap (\(rho u) -> rho * u - dt * dx * rho * u * u)
    p_n12 = fmap ((*) (cs * cs))
    -- Source terms on the cells borders
    q1n = q1_n12
    q2n = fmap (\(q2, p) -> q2 - dx * p) (zip q2_n12 p_n12)
    -- Calculate the velocity in the center of the cells
    u_12 = fmap (\(q1, q1_1, q2, q2_1) -> 0.5 * (q2/q1 + q2_1/q1_1))
       $ liftM4 (dropEnd 1 q1n) (dropEnd 1 q2n) (drop 1 q1n) (drop 1 q2n)
    -- Calculate the flux
    f1_12 = fmap ((\q1, q1_1, u) ->
        if u > 0 then q1 * u
        else q1_1 * u
      ) $ zip3 (dropEnd 1 q1n) (drop 1 q1n) u_12
    f2_12 = fmap ((\q2, q2_1, u) ->
        if u > 0 then q2 * u
        else q2_1 * u
      ) $ zip3 (dropEnd 1 q2n) (drop 1 q2n) u_12

main =
  print "test"
  where
    gridRho = [0.0, 0.0, 1.0, 1.0, 1.0, 0.5, 0.0]
    gridRhoU = [0.0, 0.0, 1.0, 1.0, 1.0, 0.5, 0.0]
    dx = 1.0
    dt = 0.5