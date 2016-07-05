import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width  = 400
height = 400

setup = do
	size (width, height)

draw () = do
	background (grey 12)	
	m <- mouse
	circle 15 m