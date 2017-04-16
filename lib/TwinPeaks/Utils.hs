module TwinPeaks.Utils where

-- Imports
import Control.Monad.Primitive (RealWorld)
import System.Random.MWC as MWC (Gen, uniform, uniformR)
import System.Random.MWC.Distributions as MWC (standard)

-- Logsumexp
logsumexp :: Double -> Double -> Double
logsumexp a b = log (exp (a - xm) + exp (b - xm)) + xm where
  xm = max a b

-- Logdiffexp
logdiffexp :: Double -> Double -> Double
logdiffexp a b
    | b >= a    = 0
    | otherwise = b + log (exp (a - b) - 1.0)

-- Mod
myMod :: Double -> Double -> Double
myMod y x = (y/x - (fromIntegral . myFloor) (y/x))*x
  where myFloor = floor :: Double -> Int

-- Wrap
wrap :: Double -> (Double, Double) -> Double
wrap x (a, b)
    | x < xmin || x > xmax = myMod (x - xmin) (xmax - xmin) + xmin
    | otherwise            = x
  where
    xmin = min a b
    xmax = max a b

-- My favourite heavy tailed distribution
randh :: Gen RealWorld -> IO Double
randh rng = do
    a <- MWC.standard rng
    b <- MWC.uniform  rng
    n <- MWC.standard rng
    return $! transform a b n
  where
    transform a b n =
      let t = a/sqrt (- (log b))
      in  10.0**(1.5 - 3.0 * abs t)*n

-- Choose a number in the supplied range
-- that is different from the supplied reference.
chooseCopy :: Int -> Int -> Gen RealWorld -> IO Int
chooseCopy ref n = loop where
  loop rng = do
    index <- MWC.uniformR (0, n - 1) rng
    if   index == ref && n > 1
    then loop rng
    else return $! index

