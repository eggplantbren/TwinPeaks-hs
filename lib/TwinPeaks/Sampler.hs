module TwinPeaks.Sampler where

-- Imports
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- A particle can be either in the foreground or
-- in the background.
data ParticleStatus = Foreground | Background

-- A type that defines a Sampler over space a
data Sampler a = Sampler
                 {
                     particles  :: V.Vector a,      -- All particles
                     scalars1   :: U.Vector Double, -- The first scalar
                     scalars2   :: U.Vector Double, -- The second scalar
                     statuses   :: U.Vector Bool    -- Statuses of particles
                 }

