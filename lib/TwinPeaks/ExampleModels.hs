module TwinPeaks.ExampleModels where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC

-- Simple Example starts here ---------------------------------------

-- Number of coordinates
simpleSize :: Int
simpleSize = 10

-- fromPrior
simpleFromPrior :: Gen RealWorld -> IO (U.Vector Double)
simpleFromPrior rng = do
  xs <- U.replicateM simpleSize (uniform rng)
  return xs

-- perturb
simplePerturb :: U.Vector Double
              -> Gen RealWorld
              -> IO (U.Vector Double, Double)
simplePerturb xs rng = do

  -- Choose a coordinate to perturb
  which <- uniformR (0, simpleSize - 1) rng
  return $! (xs, 0.0)


-- First scalar
simpleScalars1 :: U.Vector Double -> Double
simpleScalars1 xs = U.sum $ U.map ((** 2.0) . (subtract 0.5)) xs


-- Second scalar
simpleScalars2 :: U.Vector Double -> Double
simpleScalars2 xs = U.sum xs

-- toString
simpleToString :: U.Vector Double -> String
simpleToString xs =
  let
    coordsList = U.toList xs
    strings    = map show coordsList
    withCommas = map (++ ",") strings
    oneString  = mconcat withCommas
  in
    init oneString

-- Header
simpleHeader :: String
simpleHeader =
  let
    str i     = "x[" ++ show i ++ "],"
    strings   = map str [0..(simpleSize - 1)]
    oneString = mconcat strings
  in
    init oneString

