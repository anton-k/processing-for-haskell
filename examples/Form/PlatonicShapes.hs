import Graphics.Proc3

main = runProc $ def
  { procSetup = setup
  , procDraw = draw
  , procUpdate = update
  }

setup = do
  size (P2 800 630)
  pure 0

update t = pure (t + 0.1)

-- | We can also try for shape
--
-- * tetrahedron
-- * cube / box
-- * octahedron
-- * dodecahedron
-- * icosahedron
draw = drawBy 2
  ( reverse $
   zip3 [icosahedron, octahedron, tetrahedron, dodecahedron, cube]
        [black, yellow, blue, green, purple]
        [10, 22, 45, 70, 120]
  )

drawBy speed params t = do
  background white
  let fov = pi / 15
      cameraZ = 500 / tan (fov / 2)
  camera (P3 0 0 0) (P3 0 0 (negate cameraZ)) (P3 0 1 0)
  perspective fov 1 (cameraZ / 10) (cameraZ * 400)
  translate (P3 150 150 (negate 300))
  scale 1.2
  strokeWeight 2
  zipWithM_ go (cycle [rotateX, rotateY, rotateZ, rotateX . negate, rotateY . negate, rotateZ . negate]) params
  where
    go tfm (shape, col, rad) =
      local $ do
        strokeWeight (7 * rad / 100)
        strokeFill col
        tfm $ 0.3 * speed * t / rad
        shape rad


