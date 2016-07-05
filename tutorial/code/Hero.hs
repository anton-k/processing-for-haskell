
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400

center = (0.5 * width, 0.5 * height)

setup = do
	noStroke
	size (width, height)	

draw () = do
	background (grey 230)
	local $ do
	 	translate center
	 	scale (2, 2)
	 	rotate 0.2
		drawHero
	local $ do 
		translate (100, 320)
		scale (0.5, 0.5)
		drawHero

drawHero = do
	drawHead
	drawBody
	drawLegs
	drawArms

drawHead = do	
	fill (grey 165)	
	circle 10 (0, -20)

drawBody = do
	fill (grey 68)
	rect (-10, -10) (20, 40)

drawLegs = do
	fill (grey 125)	
	rect (-7, 30) (3, 27)
	rect (4, 30) (3, 27)
	
drawArms = do
	fill (grey 125)	
	strokeWeight 2
	linePath [(-10, -10), (-20, 7), (-15, 15)]
	linePath [(10, -10), (20, -27), (15, -35)]