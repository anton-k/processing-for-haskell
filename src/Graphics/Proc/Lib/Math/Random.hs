module Graphics.Proc.Lib.Math.Random(	
	randomSeed, random, random2, randomP2, randomCol, 
	NoiseDetails(..), noiseSeed, noise1, noise2, noise3,
	randomGaussian
) where

import qualified Control.Arrow as Arr(first, second)
import qualified Control.Monad.Trans.State.Strict as S

import GHC.Float

import qualified System.Random as S
import System.Random hiding (random)
import qualified Numeric.Noise.Perlin as Perlin
import qualified Numeric.Noise as Perlin

import Graphics.Proc.Core
import Graphics.Proc.Lib.Environment

onRandom :: (Maybe StdGen -> IO (a, Maybe StdGen)) -> Pio a
onRandom f = Pio $ do
  st <- S.get
  (res, gen) <- liftIO $ f (globalRandomGen st)
  S.put (st { globalRandomGen = gen })
  return res

onNoise :: (NoiseDetails -> Maybe Perlin.Seed -> IO (a, Maybe Perlin.Seed)) -> Pio a
onNoise f = Pio $ do
  st <- S.get
  (res, gen) <- liftIO $ f (globalNoiseDetails st) (globalNoiseGen st)
  S.put (st { globalNoiseGen = gen })
  return res

randomSeed :: Int -> Pio ()
randomSeed n = onRandom $ const $ return ((), Just (mkStdGen n))

random :: Float -> Pio Float
random maxVal= random2 (0, maxVal)

random2 :: (Float, Float) -> Pio Float
random2 (minVal, maxVal) = onRandom $ \mg -> 
  case mg of 
    Just g  -> return $ Arr.second Just $ randomR (minVal, maxVal) g
    Nothing -> do
      res <- randomRIO (minVal, maxVal)
      return (res, Nothing)

randomP2 :: Pio P2
randomP2 = do
  x <- random =<< winWidth
  y <- random =<< winHeight
  return (x, y)

randomCol :: Pio Col
randomCol = liftA3 (\r g b -> Col r g b 1) (random 1) (random 1) (random 1)

randomGaussian :: Pio Float
randomGaussian = do
  r1 <- random 1
  r2 <- random 1
  return $ boxMuller 0 1 (r1, r2)

-- | BoxMuller algorythm for creation of Gaussian random values out of two uniform number r1, r2.
-- r1, r2 should belong to the interval [0, 1]
boxMuller :: Floating a => a -> a -> (a, a) -> a
boxMuller mu sigma (r1,r2) =  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)

noiseSeed :: Int -> Pio ()
noiseSeed n = onNoise $ const $ const $ return ((), Just n)

noiseOctaves :: Int -> Pio ()
noiseOctaves octaves = Pio $ do
  st <- S.get  
  let details = globalNoiseDetails st
  S.put (st { globalNoiseDetails = details { noiseDetailsOctaves = octaves }})

noiseDetails :: Int -> Float -> Pio ()
noiseDetails octaves fallOff = Pio $ do
  st <- S.get  
  S.put (st { globalNoiseDetails = NoiseDetails octaves fallOff })

noise1 :: Float -> Pio Float
noise1 x = noise3 (x, 0, 0)

noise2 :: P2 -> Pio Float
noise2 (x, y) = noise3 (x, y, 0) 

noise3 :: P3 -> Pio Float
noise3 p = onNoise $ \details mseed -> case mseed of
  Just g  -> return $ (noiseValue g details p, mseed)
  Nothing -> do
    seed <- randomIO 
    return $ (noiseValue seed details p, mseed)
  where
    noiseValue seed (NoiseDetails octaves fallOff) p = 
      double2Float $ rescale $ Perlin.noiseValue (Perlin.perlin seed octaves scale persistance) (toDoublePoint p)
      where
        scale = 1
        persistance = f2d fallOff    
        toDoublePoint (x, y, z) = (f2d x, f2d y, f2d z)

        -- from [-1,1] to [0,1]
        rescale x = 0.5 * (x + 1)
