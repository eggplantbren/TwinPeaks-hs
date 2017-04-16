{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO
import TwinPeaks.Model
import System.Random.MWC

-- A particle can be either in the foreground or
-- in the background.
data ParticleStatus = Foreground | Background

-- A type that defines the state of a
-- sampler over space 'a'
data Sampler a = Sampler
                 {
                     -- All particles
                     samplerParticles :: V.Vector a,

                     -- The values of the particles' scalars
                     samplerScalars1  :: U.Vector Double,
                     samplerScalars2  :: U.Vector Double,

                     -- Status of each particle
                     samplerStatuses  :: V.Vector ParticleStatus,

                     -- NS iteration counter
                     samplerIteration :: Int
                 }


-- IO action to generate an initialised sampler.
generateSampler :: Show a =>
                   Model a
                -> Int
                -> Gen RealWorld
                -> IO (Sampler a)
generateSampler Model {..} numParticles rng = do

  -- Print a message
  putStr $ "Creating sampler with " ++ show numParticles ++
           " particles..."
  hFlush stdout

  -- Generate the particles
  particles <- V.replicateM numParticles (fromPrior rng)

  -- Evaluate the scalars
  let theScalars1 = V.map scalar1 particles
  let theScalars2 = V.map scalar2 particles

  -- Convert to unboxed vectors
  let theScalars1' = U.convert theScalars1 :: U.Vector Double
  let theScalars2' = U.convert theScalars2 :: U.Vector Double

  -- Statuses
  let statuses = V.replicate numParticles Foreground

  -- Put the sampler together
  let sampler = Sampler particles
                        theScalars1'
                        theScalars2'
                        statuses 0

  -- Use 'seq' to force evaluation of everything
  seq sampler (return ())       :: IO ()

  putStrLn "done."

 -- Use a strict return
  return $! sampler

