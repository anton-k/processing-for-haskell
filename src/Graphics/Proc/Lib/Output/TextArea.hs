module Graphics.Proc.Lib.Output.TextArea(
	println
) where

import Graphics.Proc.Core

println :: Show a => a -> Pio ()
println = liftIO . print
