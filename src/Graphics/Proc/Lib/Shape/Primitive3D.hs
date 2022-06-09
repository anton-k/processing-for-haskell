module Graphics.Proc.Lib.Shape.Primitive3D(
  SphereRes(..),
  sphereDetail,
  sphere,
  box,
  tetrahedron,
  cube,
  octahedron,
  dodecahedron,
  icosahedron,
) where

import Graphics.Proc.Core
import Graphics.Proc.Lib.Shape.Primitive2D (linePath, line)
import qualified Graphics.Rendering.OpenGL.GLU.Quadrics as G

-- | Controls the detail used to render a sphere by adjusting the number of vertices of the sphere mesh.
-- The default resolution is 30, which creates a fairly detailed sphere definition with vertices
-- every 360/30 = 12 degrees. If you're going to render a great number of spheres per frame,
-- it is advised to reduce the level of detail using this function.
-- The setting stays active until sphereDetail() is called again with a new parameter
-- and so should not be called prior to every sphere() statement, unless you wish to render
-- spheres with different settings, e.g. using less detail for smaller spheres or ones
-- further away from the camera. To control the detail of the horizontal and vertical resolution
-- independently, use the version of the functions with two parameters.
sphereDetail :: SphereRes -> Pio ()
sphereDetail sphereRes = modify' $ \st -> st { globalSphereDetail = toRawSphereRes sphereRes }

sphere :: Float -> Draw
sphere rad = do
  RawSphereRes slices stacks <- gets globalSphereDetail
  liftIO $ G.renderQuadric (G.QuadricStyle Nothing G.NoTextureCoordinates G.Outside G.FillStyle) (G.Sphere (f2d rad) slices stacks)

-- | Draw a box (width, height, depth) with center at 0.
box :: P3 -> Draw
box (P3 h w d) = do
  sq d2
  sq (-d2)
  rib (-h2) (-w2)
  rib h2 (-w2)
  rib (-h2) w2
  rib h2 w2
  where
    sq z = linePath [P3 (-h2) (-w2) z, P3 (-h2) w2 z, P3 h2 w2 z, P3 h2 (-w2) z, P3 (-h2) (-w2) z]
    rib x y = line (P3 x y d2) (P3 x y (-d2))

    h2 = 0.5 * h
    w2 = 0.5 * w
    d2 = 0.5 * d

-- | Draw a tetrahedron with given radius of the sphere
tetrahedron :: Float -> Pio ()
tetrahedron rad = do
  side v1 v2 v3
  side v1 v2 v4
  side v1 v4 v3
  side v4 v2 v3
  where
    side a b c = linePath [a, b, c, a]

    a1 = sqrt 2 / 3
    a2 = sqrt (2 / 3)
    a3 = negate (1/3)

    v1 = rad *^ P3 (sqrt 8 / 3) 0 a3
    v2 = rad *^ P3 (negate a1) a2 a3
    v3 = rad *^ P3 (negate a1) (negate a2) a3
    v4 = rad *^ P3 0 0 1

cube :: Float -> Pio ()
cube rad = box (P3 x x x)
  where
    x = 2 * rad / sqrt 3

octahedron :: Float -> Pio ()
octahedron rad = do
  side v1 v3 v5
  side v1 v3 v6
  side v1 v4 v5
  side v1 v4 v6
  side v2 v3 v5
  side v2 v3 v6
  side v2 v4 v5
  side v2 v4 v6
  where
    side a b c = linePath [a, b, c, a]

    rad' = negate rad

    v1 = P3 rad  0 0
    v2 = P3 rad' 0 0

    v3 = P3 0 rad  0
    v4 = P3 0 rad' 0

    v5 = P3 0 0 rad
    v6 = P3 0 0 rad'

dodecahedron :: Float -> Pio ()
dodecahedron rad = do
  side v15 v13 v2 v11 v6
  side v13 v1 v17 v18 v2
  side v11 v6 v20 v8 v12
  side v11 v12 v4 v18 v2
  side v8 v12 v4 v14 v16
  side v4 v18 v17 v3 v14
  side v15 v13 v1 v9 v5
  side v15 v6 v20 v19 v5
  side v20 v19 v7 v16 v8
  side v15 v13 v1 v9 v5
  side v9 v10 v7 v19 v5
  side v9 v10 v3 v17 v1
  where
    side a1 a2 a3 a4 a5 = linePath [a1, a2, a3, a4, a5, a1]
    k = rad / sqrt 3

    phi = (1 + sqrt 5) / 2
    invPhi = recip phi
    p x y z = k *^ P3 x y z

    v1 = p 1 1 1
    v2 = p 1 (-1) 1
    v3 = p 1 1 (-1)
    v4 = p 1 (-1) (-1)

    v5 = p (-1) 1 1
    v6 = p (-1) (-1) 1
    v7 = p (-1) 1 (-1)
    v8 = p (-1) (-1) (-1)

    v9  = p 0 phi invPhi
    v10 = p 0 phi (negate invPhi)
    v11 = p 0 (negate phi) invPhi
    v12 = p 0 (negate phi) (negate invPhi)

    v13 = p invPhi 0 phi
    v14 = p invPhi 0 (negate phi)
    v15 = p (negate invPhi) 0 phi
    v16 = p (negate invPhi) 0 (negate phi)

    v17  = p phi invPhi 0
    v18 = p phi (negate invPhi) 0
    v19 = p (negate phi) invPhi 0
    v20 = p (negate phi) (negate invPhi) 0

-- | Draw icosahedron with given radius
icosahedron :: Float -> Pio ()
icosahedron rad = do
  side v1 v5 v7
  side v2 v5 v7
  side v1 v5 v9
  side v1 v9 v3
  side v1 v10 v3
  side v7 v10 v12
  side v8 v10 v12
  side v8 v10 v3
  side v8 v6 v3
  side v8 v6 v4
  side v11 v6 v4
  side v11 v2 v4
  side v11 v2 v5
  side v11 v9 v5
  side v11 v9 v6
  side v3 v9 v6
  side v4 v12 v8
  side v4 v12 v2
  side v1 v10 v7
  side v2 v7 v12
  where
    side a b c = linePath [a, b, c, a]
    phi = (1 + sqrt 5) / 2
    k = rad / sqrt (phi * phi + 1)
    p x y z = k *^ P3 x y z

    v1 = p 0 1 phi
    v2 = p 0 1 (negate phi)
    v3 = p 0 (-1) phi
    v4 = p 0 (-1) (negate phi)

    v5 = p 1 phi 0
    v6 = p 1 (negate phi) 0
    v7 = p (-1) phi 0
    v8 = p (-1) (negate phi) 0

    v9 = p phi 0 1
    v10 = p (negate phi) 0 1
    v11 = p phi 0 (-1)
    v12 = p (negate phi) 0 (-1)

