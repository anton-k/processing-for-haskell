import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

setup = do
	size (400, 400)	

draw () = do
	noStroke
	fill (rgb 255 145 23)
	ellipse (50, 50) (40, 70)
	
	noStroke
	fill (grey 74)
	rect (150, 50) (60, 90)

	strokeWeight 6
	stroke (rgb 24 146 75)
	line (50, 200) (350, 200)

	noStroke
	fill (greya 34 140)
	rect (100, 300) (75, 75)

	noStroke
	fill (rgba 230 53 38 100)
	triangle (50, 350) (100, 250) (150, 350)

	strokeWeight 2
	stroke purple
	bezier (250, 250) (350, 200) (400, 350) (367, 80)

	fill green
	stroke navy
	quad (220, 300) (270, 350) (320, 300) (260, 260)

	stroke black
	point (350, 230)

	linePath [(10, 10), (10, 390), (390, 390)]


