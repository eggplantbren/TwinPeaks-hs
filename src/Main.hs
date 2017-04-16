module Main where

-- Imports
import Control.Monad.Primitive (RealWorld)
import System.Random.MWC
import TwinPeaks.ExampleModels
import TwinPeaks.Sampler

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  _ <- generateSampler simpleExample 100 rng
  return ()

