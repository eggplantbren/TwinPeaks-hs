{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
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
                   samplerIs :: !(U.Vector Int),
                   samplerJs :: !(U.Vector Int)
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


-- Do a single Metropolis step of a particle
doStep :: Sampler a
       -> Gen RealWorld
       -> IO (Sampler a)
doStep Sampler {..} rng = do

  -- Choose a particle to move
  let n = optionsNumParticles samplerOptions
  k <- uniformR (0, n - 1) rng :: IO Int

  -- Generate proposal
  (proposal, logH) <- modelPerturb samplerModel (samplerParticles V.! k) rng
  let proposalF = modelScalar1 samplerModel proposal
  let proposalG = modelScalar2 samplerModel proposal

  -- Acceptance probability
  let alpha = if logH >= 0.0 then 1.0 else exp logH :: Double
  u <- uniform rng :: IO Double

  -- Accept?
  sampler' <- if u <= alpha then do
                    -- Replace particle with proposal
                    samplerParticles' <- V.unsafeThaw samplerParticles
                    VM.write samplerParticles' k proposal
                    samplerParticles'' <- V.unsafeFreeze samplerParticles'

                    -- Replace f and g value with those of proposal
                    samplerFs' <- U.unsafeThaw samplerFs
                    samplerGs' <- U.unsafeThaw samplerGs
                    UM.write samplerFs' k proposalF
                    UM.write samplerGs' k proposalG
                    samplerFs'' <- U.unsafeFreeze samplerFs'
                    samplerGs'' <- U.unsafeFreeze samplerGs'

                    return $ Sampler samplerModel
                                     samplerOptions
                                     samplerParticles''
                                     samplerFs''
                                     samplerGs''
                                     samplerIs
                                     samplerJs
                 else return Sampler {..}

  return sampler'


-- Do some exploration for a while
doSteps :: Sampler a
        -> Int
        -> Gen RealWorld
        -> IO (Sampler a)
doSteps !sampler i rng
  | i >= optionsMcmcSteps (samplerOptions sampler) = return sampler
  | otherwise = do
                    sampler' <- doStep sampler rng
                    putStr "."
                    hFlush stdout
                    doSteps sampler' (i+1) rng

