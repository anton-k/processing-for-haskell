import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

setup = size (P2 200 200)

draw _ = do
  background (grey 200)
  stroke black
  translate (P3 50 50 0)
  rotateX . (* 0.05) =<< mouseY
  rotateY . (* 0.05) =<< mouseX
  sphereDetail . (\x -> SphereRes x x) . int . (* 0.25) =<< mouseX
  sphere 40

