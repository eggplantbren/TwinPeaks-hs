{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import System.IO
import System.Random.MWC
import TwinPeaks.Model

-- A type that contains sampler options
data Options = Options
               {
                  -- Number of particles
                  optionsNumParticles :: !Int,

                  -- MCMC steps per level
                  optionsMcmcSteps    :: !Int,

                  -- Backtracking length
                  optionsLambda       :: !Double
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
                   samplerModel :: !(Model a),

                   -- The options
                   samplerOptions :: !Options,

                   -- The particles! What else?
                   samplerParticles :: !(V.Vector a),

                   -- The scalars
                   samplerFs :: !(U.Vector Double),
                   samplerGs :: !(U.Vector Double),

                   -- Particle indices
                   samplerParticleIs :: !(U.Vector Int),
                   samplerParticleJs :: !(U.Vector Int)
                 }


-- Initialise a Sampler
initSampler :: Model a -> Options -> Gen RealWorld -> IO (Sampler a)
initSampler Model {..} Options {..} rng = do
  let n = optionsNumParticles
  let is = U.replicate n 0
  let js = U.replicate n 0

  -- Print a message
  putStr $ "# Generating " ++ show n ++ " particles "
  putStr "from the prior..."
  hFlush stdout

  -- Generate and evaluate particles
  particles <- V.replicateM n (modelFromPrior rng)
  let fs = U.convert $ V.map modelScalar1 particles
  let gs = U.convert $ V.map modelScalar2 particles

  -- Create the sampler
  let sampler = Sampler Model {..} Options {..} particles fs gs is js

  -- Print 'done', but only after sampler is fully evaluated
  seq sampler $ putStrLn "done."
  return sampler

