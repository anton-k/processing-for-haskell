import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

setup = size (P2 400 400)

draw _ = do
  background white
  stroke black
  translate (P3 232 192 0)
  rotateY 0.5
  noFill
  box 160

