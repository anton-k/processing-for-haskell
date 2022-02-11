-- original code: https://processing.org/examples/incrementdecrement.html

-- Increment Decrement.
--
-- Writing "a++" is equivalent to "a = a + 1". Writing "a--" is equivalent to "a = a - 1".
-- but in Haskell  there is no such operator we can just update values.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

setup = do
  size (P2 width height)
  -- not implemented
  -- colorMode(RGB, width)
  frameRate 30
  return (a, b, direction)
  where
    a = 0
    b = width
    direction = True

stroke' n = stroke (grey $ 255 * n / width)

draw (a, b, direction) = do
  if direction
    then stroke' a
    else stroke' (width - a)
  line (P2 a 0) (P2 a (height / 2))

  if direction
    then stroke' (width - b)
    else stroke' b
  line (P2 b (height/2+1)) (P2 b height)

update (a, b, direction) = return (a1, b1, direction1)
  where
    a1 = if (a > width) then 0 else a + 1
    b1 = if (b < 0)     then width else b - 1
    direction1 = if (a > width) then not direction else direction
