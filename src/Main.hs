module Main where

-- Imports
import Data.Maybe
import System.Random.MWC
import TwinPeaks.ExampleModels
import TwinPeaks.Sampler

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Print a welcome message
  putStrLn "# Running TwinPeaks..."

  -- A set of sampler options
  let someOptions = fromMaybe
                      (error "Error. Invalid options.")
                      (createOptions 1 10000 10)

  -- Display the sampler options
  putStrLn $ "# Sampler options set to:"
  putStrLn $ "#   " ++ show someOptions

  return ()

