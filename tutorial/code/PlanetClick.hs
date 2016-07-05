import Graphics.Proc

width  = 400
height = 400
center = (width / 2, height / 2)

setup = do
	size (width, height)
	return (1, initPlanets)

draw (_, xs) = do
	background (grey 0)
	drawSun
	mapM_ drawPlanet xs

update (orient, xs) = return $ (orient, fmap (updatePlanet orient) xs)

-------------------------------------
-- sun

drawSun = do
	fill (grey 255)
	circle 17 center	

-------------------------------------
-- planets

data Planet = Planet 
	{ planetColor 	:: Col
	, planetRadius :: Float
	, planetAngle 	:: Float
	, planetSpeed 	:: Float
	, planetDistanceToSun :: Float }

drawPlanet p = local $ do
	translate center
	rotate (planetAngle p)
	noStroke
	fill (planetColor p)
	circle (planetRadius p) (0, planetDistanceToSun p)

updatePlanet orient p = p { planetAngle = angle + orient * speed }
	where
		angle = planetAngle p
		speed = planetSpeed p

initPlanets = [Planet green 10 0 0.0013 85, Planet red 8 0 0.001 155]

-------------------------------------

mousePressed (orient, ps) = return (negate orient, ps)

-------------------------------------

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update
	, procMousePressed = mousePressed }