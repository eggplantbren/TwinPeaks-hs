module TwinPeaks.ExampleModels where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC
import TwinPeaks.Model
import TwinPeaks.Utils

-- Simple Example starts here ---------------------------------------

-- Number of coordinates
simpleSize :: Int
simpleSize = 1000

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

  -- Can't use unsafeThaw, original vector needs to be
  -- preserved in case the proposal is rejected.
  xsMutable <- U.thaw xs

  -- New value of coordinate
  x' <- fmap (xs U.! which + ) (randh rng)
  let x'' = wrap x' (0.0, 1.0)
  UM.unsafeWrite xsMutable which x''

  -- Freeze modified vector
  xs' <- U.unsafeFreeze xsMutable

  return $! (xs', 0.0)

-- First scalar
simpleScalar1 :: U.Vector Double -> Double
simpleScalar1 xs = U.foldl'
                            (\acc x -> acc - 0.5*(x - 0.5)**2)
                            0.0
                            xs

-- Second scalar
simpleScalar2 :: U.Vector Double -> Double
simpleScalar2 = U.sum

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


-- The whole model
simpleExample :: Model (U.Vector Double)
simpleExample = Model simpleFromPrior
                      simplePerturb
                      simpleScalar1
                      simpleScalar2
                      simpleToString
                      simpleHeader

