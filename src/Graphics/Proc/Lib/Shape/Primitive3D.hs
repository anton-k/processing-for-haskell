module Graphics.Proc.Lib.Shape.Primitive3D(
  SphereRes(..),
  sphereDetail,
  sphere,
  box,
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
    sq z = linePath [P3 (-h2) (-w2) z, P3 (-h2) w2 z, P3 h2 w2 z, P3 h2 (-w2) z]
    rib x y = line (P3 x y d2) (P3 x y (-d2))

    h2 = 0.5 * h
    w2 = 0.5 * w
    d2 = 0.5 * d

