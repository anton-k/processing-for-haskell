module Graphics.Proc.Lib.Math.Random(
  randomSeed, random, random2, randomP2, randomCol, randomCola,
  -- * Perlin noise
  -- | Returns the Perlin noise value at specified coordinates. Perlin noise is a random sequence generator producing a more natural, harmonic succession of numbers than that of the standard random() function. It was developed by Ken Perlin in the 1980s and has been used in graphical applications to generate procedural textures, shapes, terrains, and other seemingly organic forms.
  --
  -- processing docs: <https://processing.org/reference/noise_.html>
  NoiseDetail(..), noiseDetail, noiseOctaves, noiseSeed,
  noise1, noise2, noise3,
  randomGaussian
) where

import qualified Control.Arrow as Arr(first, second)
import qualified Control.Monad.State.Strict as S

import GHC.Float

import qualified System.Random as S
import System.Random hiding (random)
import qualified Numeric.Noise.Perlin as Perlin
import qualified Numeric.Noise as Perlin

import Graphics.Proc.Core
import Graphics.Proc.Lib.Environment

onRandom :: (Maybe StdGen -> IO (a, Maybe StdGen)) -> Pio a
onRandom f = do
  gen <- getRandomGen
  (res, gen1) <- liftIO $ f gen
  putRandomGen gen1
  return res

onNoise :: (NoiseDetail -> Maybe Perlin.Seed -> IO (a, Maybe Perlin.Seed)) -> Pio a
onNoise f = do
  noiseDetail <- getNoiseDetail
  gen <- getNoiseGen
  (res, gen1) <- liftIO $ f noiseDetail gen
  putNoiseGen gen1
  return res

-- | Sets the seed value for random(). By default, random() produces different results each time the program is run. Set the seed parameter to a constant to return the same pseudo-random numbers each time the software is run.
--
-- processing docs: <https://processing.org/reference/randomSeed_.html>
randomSeed :: Int -> Pio ()
randomSeed n = onRandom $ const $ return ((), Just (mkStdGen n))

-- | Generates random numbers. Each time the random() function is called, it returns an unexpected value within the specified range. If only one parameter is passed to the function, it will return a float between zero and the value of the high parameter. For example, random(5) returns values between 0 and 5 (starting at zero, and up to, but not including, 5).
--
-- processing docs: <https://processing.org/reference/random_.html>
random :: Float -> Pio Float
random maxVal= random2 (0, maxVal)

-- | Genrates random numbers within the given range.
random2 :: (Float, Float) -> Pio Float
random2 (minVal, maxVal) = onRandom $ \mg ->
  case mg of
    Just g  -> return $ Arr.second Just $ randomR (minVal, maxVal) g
    Nothing -> do
      res <- randomRIO (minVal, maxVal)
      return (res, Nothing)

-- | Creates random point within the ranges of the size of the screen.
randomP2 :: Pio P2
randomP2 = do
  x <- random =<< winWidth
  y <- random =<< winHeight
  pure (P2 x y)

-- | Creates random color.
randomCol :: Pio Col
randomCol = liftA3 (\r g b -> Col r g b 1) (random 1) (random 1) (random 1)

-- | Creates random color with transparency.
randomCola :: Pio Col
randomCola = liftA4 Col (random 1) (random 1) (random 1) (random 1)
  where liftA4 f a b c d = f <$> a <*> b <*> c <*> d

-- | Returns a float from a random series of numbers having a mean of 0 and standard deviation of 1. Each time the randomGaussian() function is called, it returns a number fitting a Gaussian, or normal, distribution. There is theoretically no minimum or maximum value that randomGaussian() might return. Rather, there is just a very low probability that values far from the mean will be returned; and a higher probability that numbers near the mean will be returned.
--
-- processing docs: <https://processing.org/reference/randomGaussian_.html>
randomGaussian :: Pio Float
randomGaussian = do
  r1 <- random 1
  r2 <- random 1
  return $ boxMuller 0 1 (r1, r2)

-- | BoxMuller algorythm for creation of Gaussian random values out of two uniform number r1, r2.
-- r1, r2 should belong to the interval [0, 1]
boxMuller :: Floating a => a -> a -> (a, a) -> a
boxMuller mu sigma (r1,r2) =  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)

-- | Sets the seed value for noise(). By default, noise() produces different results each time the program is run. Set the seed parameter to a constant to return the same pseudo-random numbers each time the software is run.
--
-- processing docs: <https://processing.org/reference/noiseSeed_.html>
noiseSeed :: Int -> Pio ()
noiseSeed n = onNoise $ const $ const $ return ((), Just n)

-- | Sets the number of octaves for perlin noise.
noiseOctaves :: Int -> Pio ()
noiseOctaves octaves = putOctaves octaves

-- | Adjusts the character and level of detail produced by the Perlin noise function. Similar to harmonics in physics, noise is computed over several octaves. Lower octaves contribute more to the output signal and as such define the overal intensity of the noise, whereas higher octaves create finer-grained details in the noise sequence.
--
-- By default, noise is computed over 4 octaves with each octave contributing exactly half than its predecessor, starting at 50% strength for the first octave. This falloff amount can be changed by adding an additional function parameter. For example, a falloff factor of 0.75 means each octave will now have 75% impact (25% less) of the previous lower octave. While any number between 0.0 and 1.0 is valid, note that values greater than 0.5 may result in noise() returning values greater than 1.0.
--
-- By changing these parameters, the signal created by the noise() function can be adapted to fit very specific needs and characteristics.
--
-- processing docs: <https://processing.org/reference/noiseDetail_.html>
noiseDetail :: Int -> Float -> Pio ()
noiseDetail octaves fallOff = putNoiseDetail $ NoiseDetail octaves fallOff

-- | Returns 1D Perlin noise.
noise1 :: Float -> Pio Float
noise1 x = noise3 (P3 x 0 0)

-- | Returns 2D Perlin noise.
noise2 :: P2 -> Pio Float
noise2 = noise3 . toP3

-- | Returns 3D Perlin noise.
noise3 :: P3 -> Pio Float
noise3 p = fmap d2f $ onNoise $ \details mseed -> case mseed of
  Just g  -> return $ (noiseValue g details p, mseed)
  Nothing -> do
    seed <- randomIO
    return $ (noiseValue seed details p, mseed)
  where
    noiseValue seed (NoiseDetail octaves fallOff) p =
      rescale $ Perlin.noiseValue (Perlin.perlin seed octaves scale (f2d persistance)) (toFloatPoint p)
      where
        scale = 1
        persistance = fallOff
        toFloatPoint (P3 x y z) = (f2d x, f2d y, f2d z)

        -- from [-1,1] to [0,1]
        rescale x = 0.5 * (x + 1)
