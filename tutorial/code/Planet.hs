import Graphics.Proc

width  = 400
height = 400
center = (width / 2, height / 2)

setup = do
	size (width, height)
	return initPlanets

draw xs = do
	background (grey 0)
	fill (grey 255)
	circle 17 center	
	mapM_ drawPlanet xs

update xs = return $ fmap updatePlanet xs

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

updatePlanet p = p { planetAngle = angle + speed }
	where
		angle = planetAngle p
		speed = planetSpeed p

initPlanets = [Planet green 10 0 0.0013 85, Planet red 8 0 0.001 155]

-------------------------------------

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }