{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO
import TwinPeaks.Model
import System.Random.MWC

-- A type that defines the state of a
-- sampler over space 'a'
data Sampler a = Sampler
                 {
                     -- All particles
                     samplerParticles :: !(V.Vector a),

                     -- The values of the particles' scalars
                     samplerScalars1  :: !(U.Vector Double),
                     samplerScalars2  :: !(U.Vector Double),

                     -- NS iteration counter
                     samplerIteration :: !Int
                 } deriving Show


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

  -- Put the sampler together
  let sampler = Sampler particles
                        theScalars1'
                        theScalars2'
                        0

  -- Uses sampler for the sole purpose of forcing its evaluation
  seq sampler (return ())
  putStrLn "done."

  -- Use a strict return
  return $! sampler

-- Find worst index for scalar 1
worstIndexScalar1 :: Sampler a -> Int
worstIndexScalar1 (Sampler {..}) = U.minIndex samplerScalars1

-- Find worst index for scalar 2
worstIndexScalar2 :: Sampler a -> Int
worstIndexScalar2 (Sampler {..}) = U.minIndex samplerScalars2

