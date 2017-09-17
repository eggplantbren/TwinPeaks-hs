{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import TwinPeaks.Model

-- A type that contains sampler options
data SamplerOptions = SamplerOptions
                      {
                        -- Number of particles
                        numParticles :: !Int,

                        -- MCMC steps per level
                        mcmcSteps    :: !Int,

                        -- Backtracking length
                        lambda       :: !Double
                      }


-- A type that represents a sampler's state
data Sampler a = Sampler
                 {
                   -- The model specification
                   theModel :: !(Model a),

                   -- The particles! What else?
                   theParticles :: !(V.Vector a),

                   -- Particle indices
                   particleIndices :: !(U.Vector (Int, Int))
                 }

