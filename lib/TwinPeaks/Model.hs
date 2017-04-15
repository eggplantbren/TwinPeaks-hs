module TwinPeaks.Model where

-- Imports
import Control.Monad.Primitive (RealWorld)
import System.Random.MWC

-- To run TwinPeaks on a certain model, you need to specify
-- the model. That means a set of functions that are needed.

data Model a = Model
               {
                   -- A function to generate from the prior
                   fromPrior :: Gen RealWorld -> IO a,

                   -- A function to do a Metropolis proposal
                   -- and return a perturbed particle, along
                   -- with logH
                   perturb   :: a
                             -> Gen RealWorld
                             -> IO (a, Double),

                   -- Two 'scalars'
                   scalars1  :: a -> Double,
                   scalars2  :: a -> Double,

                   -- Convert to a string, for CSV output
                   toStrLn   :: a -> String,

                   -- For header of CSV output, name the columns
                   header    :: String
               }

