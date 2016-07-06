import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400
center = (0.5 * width, 0.5 * height)

setup = do
	size (width, height)
	background (grey 255)		
	frameRate 7

draw () = do
	drawRandomGaussCircle

drawRandomCircle :: Pio ()	
drawRandomCircle = do	
	noStroke
	fill =<< randomCola
	rad <- random2 (10, 40)
	circle rad =<< randomP2

drawRandomGaussCircle = do	
	noStroke
	fill =<< randomCola
	rad <- random2 (10, 20)
	x <- randomGaussian
	y <- randomGaussian
	circle rad  (50 * (x, y) + center)

