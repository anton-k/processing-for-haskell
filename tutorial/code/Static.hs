import Graphics.Proc

width  = 400
height = 400
center = (width / 2, height / 2)

setup = do
	size (width, height)
	return ()

draw () = do
	background (grey 0)
	fill (grey 255)
	circle 17 center	

update x = return x

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }