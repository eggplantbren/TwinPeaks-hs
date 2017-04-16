module Main where

-- Imports
import Control.Monad.Primitive (RealWorld)
import Data.Vector.Unboxed as U
import System.Random.MWC
import TwinPeaks.ExampleModels
import TwinPeaks.Sampler

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  sampler <- generateSampler simpleExample 10 rng
  print $ U.sum (samplerScalars1 sampler)
  return ()

