{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import TwinPeaks.Model

-- A type that contains sampler options
data Options = Options
               {
                  -- Number of particles
                  numParticles :: !Int,

                  -- MCMC steps per level
                  mcmcSteps    :: !Int,

                  -- Backtracking length
                  lambda       :: !Double
               } deriving (Eq, Read, Show)

-- Smart constructor of options
createOptions :: Int -> Int -> Double -> Maybe Options
createOptions n m l
  | n <= 0 = Nothing
  | m <= 0 = Nothing
  | l <= 0.0 = Nothing
  | otherwise = Just $ Options n m l


-- A type that represents a sampler's state
data Sampler a = Sampler
                 {
                   -- The model specification
                   theModel :: !(Model a),

                   -- The options
                   options :: !Options,

                   -- The particles! What else?
                   theParticles :: !(V.Vector a),

                   -- Particle indices
                   particleIndices :: !(U.Vector (Int, Int))
                 }




