module Graphics.Proc.Lib.Data.Conversion(
	float, int
) where

-- | Converts ints to floats.
float :: Int -> Float
float = fromIntegral

-- | Converts floats to ints.
int :: Float -> Int 
int = floor