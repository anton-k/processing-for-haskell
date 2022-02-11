-- original code: https://processing.org/examples/noisewave.html

-- Noise Wave by Daniel Shiffman.
--
-- Using Perlin Noise to generate a wave-like pattern.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

setup = do
  size (P2 width height)
  return 0

draw yoff = do
	background (grey 51)
	stroke (grey 255)

	ps <- forM (zip [0, 10 .. width] [0, 0.05 ..]) $ \(x, xoff) -> do
		y <- fmap (remap (0, 1) (200, 300)) $ noise2 (P2 xoff yoff)
		return (P2 x y)

	linePath ps

update yoff = return (yoff + 0.01)


--------------------------------
-- Side note
--
-- Original code uses polygones to draw shapes but in Hskell
-- drawing of polygons is not available right now.

{-
float yoff = 0.0;        // 2nd dimension of perlin noise


void draw() {
  background(51);

  fill(255);
  // We are going to draw a polygon out of the wave points
  beginShape();

  float xoff = 0;       // Option #1: 2D Noise
  // float xoff = yoff; // Option #2: 1D Noise

  // Iterate over horizontal pixels
  for (float x = 0; x <= width; x += 10) {
    // Calculate a y value according to noise, map to
    float y = map(noise(xoff, yoff), 0, 1, 200,300); // Option #1: 2D Noise
    // float y = map(noise(xoff), 0, 1, 200,300);    // Option #2: 1D Noise

    // Set the vertex
    vertex(x, y);
    // Increment x dimension for noise
    xoff += 0.05;
  }
  // increment y dimension for noise
  yoff += 0.01;
  vertex(width, height);
  vertex(0, height);
  endShape(CLOSE);
}
-}
