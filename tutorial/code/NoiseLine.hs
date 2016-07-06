import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400

setup = do
	size (width, height)	
	forM [0 .. width] $ \x -> do
		y <- noise1 (x / 100)
		return (x, (30 * y - 15) + 0.5 * height)
		
draw ps = do
	linePath ps	

