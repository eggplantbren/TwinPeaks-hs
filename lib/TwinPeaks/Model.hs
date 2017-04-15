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

                   -- Convert to a string, for CSV output
                   render    :: a -> String
               }
