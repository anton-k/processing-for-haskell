module Graphics.Proc.Lib.Data.Conversion(
	float, int
) where

float :: Int -> Float
float = fromIntegral

int :: Float -> Int 
int = floor