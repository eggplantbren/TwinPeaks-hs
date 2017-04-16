module TwinPeaks.ExampleModels where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC

-- Simple Example starts here ---------------------------------------

-- fromPrior
simpleFromPrior :: Gen RealWorld -> IO (U.Vector Double)
simpleFromPrior rng = do
  xs <- U.replicateM 10 (uniform rng)
  return xs

-- perturb
simplePerturb :: U.Vector Double
              -> Gen RealWorld
              -> IO (U.Vector Double, Double)
simplePerturb xs rng = do

  -- Choose a coordinate to perturb
  which <- uniformR (0, U.length xs - 1) rng
  return $! (xs, 0.0)


-- First scalar
simpleScalars1 :: U.Vector Double -> Double
simpleScalars1 xs = U.sum $ U.map ((** 2.0) . (subtract 0.5)) xs

-- Second scalar
simpleScalars2 :: U.Vector Double -> Double
simpleScalars2 xs = U.sum xs

--                   -- Two 'scalars'
--                   scalars1  :: a -> Double,
--                   scalars2  :: a -> Double,

--                   -- Convert to a string, for CSV output
--                   toString  :: a -> String,

--                   -- For header of CSV output, name the columns
--                   header    :: String
