module Graphics.Proc.Core.State.Elements.Rnd(
    RndState(..),
    NoiseDetails(..), Seed
) where

import Data.Default 
import System.Random

data RndState = RndState 
  { rndRandomGen     :: Maybe StdGen
  , rndNoiseGen      :: Maybe Int
  , rndNoiseDetails  :: NoiseDetails
  }

instance Default RndState where
  def = RndState def def def  

type Seed = Maybe Int

data NoiseDetails = NoiseDetails 
  { noiseDetailsOctaves :: Int
  , noiseDetailsFalloff :: Float
  }

instance Default NoiseDetails where
  def = NoiseDetails 4 0.5
