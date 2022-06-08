import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

setup = do
  size (P2 600 600)
  pure 0

{- simple processing code

// Re-creates the default perspective
size(400, 400, P3D);
noFill();
float fov = PI/3.0;
float cameraZ = (height/2.0) / tan(fov/2.0);
perspective(fov, float(width)/float(height),
            cameraZ/10.0, cameraZ*10.0);
translate(200, 200, 0);
rotateX(-PI/6);
rotateY(PI/3);
box(180);

-}

update t = pure (t + 0.1)

draw t = do
  background white
  let fov = pi / 15
      cameraZ = 600 / tan (fov / 2)
  camera (P3 0 0 0) (P3 0 0 (negate cameraZ)) (P3 0 1 0)
  perspective fov 1 (cameraZ / 10) (cameraZ * 300)
  translate (P3 150 150 (negate 300))
  scale (P2 0.75 0.75)
  strokeWeight 2
  strokeFill black
  box 5
  strokeFill red
  local $ do
    rotateX (- ((t / 200) * pi) / 3)
    box 90
  local $ do
    rotateY (- ((t / 150) * pi) / 3)
    strokeFill green
    box 50
  strokeFill blue
  local $ do
    rotateZ (- ((t / 100) * pi) / 3)
    box 30
  strokeWeight 2
  strokeFill black
  line (P3 0 0 0) (P3 0 0 (negate cameraZ))
  line (P3 (-200) 0 0) (P3 200 0 0)
  line (P3 0 (-200) 0) (P3 0 200 0)

