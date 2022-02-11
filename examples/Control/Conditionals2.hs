-- Conditionals 2.
--
-- We extend the language of conditionals from the previous example by adding
-- the keyword "else". This allows conditionals to ask two or more sequential
-- questions, each with a different action.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (P2 width height)

draw ()  = do
  background (grey 0)
  forM_ [2, 4 .. width - 2] $ \i -> do
    -- If 'i' divides by 20 with no remainder
    if ((int i `mod` 20) == 0)
      then do
        stroke (grey 255)
        line (P2 i 80) (P2 i (height/2))
      else do
        -- If 'i' divides by 10 with no remainder
        if ((int i `mod` 10) == 0)
          then do
            stroke (grey 153)
            line (P2 i 20) (P2 i 180)
          -- If neither of the above two conditions are met
          -- then draw this line
          else do
            stroke (grey 102)
            line (P2 i (height/2)) (P2 i (height-20))

--------------------------------------------------------
-- Side note
--
-- In Haskell we don't have if-statments with multiple branches.
-- It's always a ternary operator. But we can achieve the same
-- result with nesting `if-then-else`s.
