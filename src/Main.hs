module Main where

-- Imports
import System.Random.MWC
import TwinPeaks.ExampleModels
import TwinPeaks.Sampler

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  putStrLn "# Running TwinPeaks..."
  return ()

