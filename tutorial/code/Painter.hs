import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procMousePressed = mousePressed }

width = 400
height = 400

setup = do
	size (width, height)
	strokeWeight 2
	stroke (grey 255)
	return []

draw ps = do
	background (grey 0)
	case ps of
		[] -> return ()
		_  -> do
			m <- mouse
			linePath (m : ps)

mousePressed ps = do
	mb <- mouseButton
	case mb of
		Just LeftButton -> do
			m <- mouse
			return (m : ps)
		Just RightButton -> do
			if null ps 
				then return []
				else return (tail ps)
		_ -> return ps
