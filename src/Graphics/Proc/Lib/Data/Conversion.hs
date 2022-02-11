module Graphics.Proc.Lib.Data.Conversion(
  int, float
) where

-- | Converts ints to doubles.
float :: Int -> Float
float = fromIntegral

-- | Converts doubles to ints.
int :: Float -> Int
int = floor
