module Graphics.Proc.Lib.Transform(
	translate, 
	rotate, rotateX, rotateY, rotateZ, 
	scale, 
	resetMatrix, 
	local, 
	applyMatrix, 
	shearX, shearY
) where

import Control.Monad.Trans.State.Strict	

import Graphics.Rendering.OpenGL hiding (scale, translate, rotate)
import qualified Graphics.Rendering.OpenGL as G

import Graphics.Proc.Core

translate :: P2 -> Draw 
translate p = liftIO $ G.translate $ p2v p

rotateBy :: Vector3 GLfloat -> Float -> Draw
rotateBy v x = liftIO $ G.rotate (x * 360) v 

rotate, rotateX, rotateY, rotateZ :: Float -> Draw

rotate = rotateZ

rotateX = rotateBy $ Vector3 (1 :: GLfloat) 0 0
rotateY = rotateBy $ Vector3 0 (1 :: GLfloat) 0
rotateZ = rotateBy $ Vector3 0 0 (1 :: GLfloat)

scale :: P2 -> Draw
scale (x, y) = liftIO $ G.scale x y 1

resetMatrix :: Draw
resetMatrix = liftIO $ loadIdentity

local :: Draw -> Draw
local (Pio a) = Pio $ StateT $ \s -> do
    preservingMatrix $ do
        runStateT a s

applyMatrix :: [Float] -> Draw
applyMatrix as@[a11, a12, a21, a22] = 
    applyMatrix 
        [ a11, a12,   0, 0
        , a21, a22,   0, 0
        ,   0,   0,   1, 0
        ,   0,   0,   0, 1]
applyMatrix as@[a11, a12, a13, a21, a22, a23, a31, a32, a33] = 
    applyMatrix 
        [ a11, a12, a13, 0
        , a21, a22, a23, 0
        , a31, a32, a33, 0
        ,   0,   0,   0, 1]
applyMatrix as@[a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44] = liftIO $ do
    m <- newMatrix RowMajor (fmap f2d as)
    multMatrix (m :: GLmatrix GLdouble)
applyMatrix _ = error "Wrong matrix size. The list should contain 4, 9 or 16 elements"

shearX :: Float -> Draw
shearX x = applyMatrix [1, x, 0, 1]

shearY :: Float -> Draw
shearY x = applyMatrix [1, 0, x, 1]

printMatrix :: Draw
printMatrix = undefined	
