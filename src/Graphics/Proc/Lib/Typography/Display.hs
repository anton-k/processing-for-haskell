module Graphics.Proc.Lib.Typography.Display(
	loadFont, text, textFont,
	getCurrentFont, onFont, setFontSize
) where

import Graphics.Rendering.FTGL
import Codec.Binary.UTF8.String

import Graphics.Proc.Core
import Graphics.Proc.Lib.Transform

loadFont :: String -> Pio Font
loadFont fontName = liftIO $ do
	font <- createTextureFont fontName
	fsetFontCharMap font (marshalCharMap EncodingUnicode)    
	return font

text :: String -> P2 -> Pio ()
text str p = do
	onFont $ \fontSpec -> 
				local $ do				
					translate p
					local $ do
						scale (fontSizeFactor fontSpec *^ (1, -1))					
						liftIO $ renderFont (fontCurrent fontSpec) (encodeString str) Side

fontSizeFactor fontSpec = fromIntegral (fontSize fontSpec) / fromIntegral (fontInitSize fontSpec)

textFont :: Font -> Int -> Pio ()
textFont font size = do
    modifyStatePio $ \st -> st { globalFont = Just (FontSpec font size size) }
    onFont $ \fontSpec -> liftIO $ do
        _ <- setFontFaceSize (fontCurrent fontSpec) size size
        return ()

-----------------------------------------------
-- Raw Fonts

getCurrentFont :: Pio (Maybe FontSpec)
getCurrentFont = readStatePio globalFont

onFont :: (FontSpec -> Pio ()) -> Pio ()
onFont act = do
	mfont <- getCurrentFont
	case mfont of
		Just font -> act font
		Nothing -> liftIO $ print "Font is not set"

setFontSize :: FontSpec -> Int -> Pio ()
setFontSize fontSpec size = modifyStatePio $ \st -> 
    	let font = globalFont st
            font' = fmap (\x -> x { fontSize = size }) font
        in  st { globalFont = font' }
