module TwinPeaks.Model where

-- Imports
import Control.Monad.Primitive
import System.Random.MWC

-- To run TwinPeaks on a certain model, you need to specify
-- the model. That means a set of functions are needed.

data Model a = Model
               {
                   -- A function to generate from the prior
                   modelFromPrior :: Gen RealWorld
                                  -> IO a,

                   -- A function to do a Metropolis proposal
                   -- and return a perturbed particle, along
                   -- with logH
                   modelPerturb   :: a
                                  -> Gen RealWorld
                                  -> IO (a, Double),

                   -- The two scalar functions to ascend
                   modelScalar1   :: a -> Double,
                   modelScalar2   :: a -> Double,

                   -- Convert to a string, for CSV output
                   modelToString  :: a -> String,

                   -- For header of CSV output, name the columns
                   modelHeader    :: String
               }

