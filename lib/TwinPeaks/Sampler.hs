{-# LANGUAGE RecordWildCards #-}

module TwinPeaks.Sampler where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO
import TwinPeaks.Model
import System.Random.MWC
import System.Random.MWC.Distributions

-- A type that defines the state of a
-- sampler over space 'a'
data Sampler a = Sampler
                 {
                     -- All particles
                     samplerParticles :: !(V.Vector a),

                     -- The values of the particles' scalars
                     samplerScalars1  :: !(U.Vector Double),
                     samplerScalars2  :: !(U.Vector Double),

                     -- Threshold
                     samplerThresh1   :: !Double,
                     samplerThresh2   :: !Double,

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
                        (-1E300)
                        (-1E300)
                        0

  -- Uses sampler for the sole purpose of forcing its evaluation
  seq sampler (return ())
  putStrLn "done."

  -- Use a strict return
  return $! sampler

-- Choose which scalar to ascend (1 or 2)
chooseScalar :: Gen RealWorld
             -> IO Int
chooseScalar rng = do

  -- Choose which scalar to ascend
  n1 <- fmap (*3.0) $ standard rng
  n2 <- fmap (*3.0) $ standard rng
  let p1 = exp n1 / (exp n1 + exp n2)
  u <- uniform rng :: IO Double
  let whichScalar = if u < p1 then 1 else 2 :: Int

  return whichScalar

-- Does a single iteration of TwinPeaks sampling, ascending
-- one scalar or the other.
doIteration :: Sampler a
            -> Gen RealWorld
            -> IO (Sampler a)
doIteration sampler@(Sampler {..}) rng = do
  putStr $ "Iteration " ++ show (samplerIteration + 1) ++ ". "

  -- Choose a scalar to ascend
  whichScalar <- chooseScalar rng
  let worstIndex = case whichScalar of
                     1 -> worstIndexScalar1 sampler
                     2 -> worstIndexScalar2 sampler
                     _ -> error "Error."

  -- New threshold
  let thresh1' = case whichScalar of
                   1 -> samplerScalars1 U.! worstIndex
                   2 -> samplerThresh1
                   _ -> error "Error."
  let thresh2' = case whichScalar of
                   1 -> samplerThresh2
                   2 -> samplerScalars2 U.! worstIndex
                   _ -> error "Error."

  putStrLn $ "New threshold = (" ++ show thresh1' ++ ", " ++
                 show thresh2' ++ ")."

  -- Replace the particle that was used to update the threshold. 
  return sampler

-- Find worst index for scalar 1
worstIndexScalar1 :: Sampler a -> Int
worstIndexScalar1 (Sampler {..}) = U.minIndex samplerScalars1

-- Find worst index for scalar 2
worstIndexScalar2 :: Sampler a -> Int
worstIndexScalar2 (Sampler {..}) = U.minIndex samplerScalars2

