module Graphics.Proc.Lib.Output.TextArea(
	println
) where

import Graphics.Proc.Core

-- | Prints values on the console.
println :: Show a => a -> Pio ()
println = liftIO . print
