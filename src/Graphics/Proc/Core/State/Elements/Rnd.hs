module Graphics.Proc.Core.State.Elements.Rnd(
    RndState(..),
    NoiseDetail(..), Seed
) where

import Data.Default
import System.Random

data RndState = RndState
  { rndRandomGen     :: Maybe StdGen
  , rndNoiseGen      :: Maybe Int
  , rndNoiseDetail   :: NoiseDetail
  }

instance Default RndState where
  def = RndState def def def

type Seed = Maybe Int

-- | Parameters for perlin noise. See docs for function @noiseDetail@.
data NoiseDetail = NoiseDetail
  { noiseDetailsOctaves :: Int
  , noiseDetailsFalloff :: Float
  }

instance Default NoiseDetail where
  def = NoiseDetail 4 0.5
