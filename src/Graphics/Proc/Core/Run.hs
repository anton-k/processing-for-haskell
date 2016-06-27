{-# Language FlexibleContexts #-}
module Graphics.Proc.Core.Run(
	Proc(..), runProc, Draw, TimeInterval
) where

import Control.Monad.IO.Class

import Data.Default
import Data.IORef

import qualified Graphics.Rendering.OpenGL as G
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Time.Clock
import Data.Time.Calendar

import Graphics.Proc.Core.Pio
import Graphics.Proc.Core.GLBridge

type Update s = s -> Pio s
type TimeInterval = Float

type Draw = Pio ()

data Proc s = Proc 
    { procSetup :: Pio s
    , procUpdate :: Update s
    , procUpdateTime :: TimeInterval -> Update s 
    , procDraw  :: s -> Draw

    -- mouse callbacks
    , procMousePressed  :: Update s    
    , procMouseReleased :: Update s
    , procMouseClicked  :: Update s
    , procMouseDragged  :: Update s
    , procMouseMoved    :: Update s

    -- keyboard callbacks
    , procKeyPressed   :: Update s
    , procKeyReleased  :: Update s
    , procKeyTyped     :: Update s
    }

instance Default s => Default (Proc s) where
    def = Proc
        { procSetup = return def
        , procUpdate = return
        , procUpdateTime = const return
        , procDraw = const (return ()) 
        -- mouse
        , procMousePressed  = return
        , procMouseReleased = return
        , procMouseClicked  = return
        , procMouseDragged  = return
        , procMouseMoved    = return
        -- keyboard
        , procKeyPressed    = return
        , procKeyReleased   = return
        , procKeyTyped      = return
        }


runProc :: Proc s -> IO ()
runProc p = do
  setupWindow

  inputSt <- newIORef (def :: InputState)
  globalStRef <- newIORef =<< (liftIO . defGlobalState =<< G.get inputSt)

  (initSt, globalSt) <- runPio (procSetup p) =<< G.get globalStRef
  globalStRef $= globalSt
  st <- newIORef initSt

  displayCallback $= display st globalStRef inputSt
  idleCallback $= Just (idle st globalStRef inputSt)
  keyboardMouseCallback $= Just (keyMouse st globalStRef inputSt)
  motionCallback $= Just (mouseMotion inputSt)
  passiveMotionCallback $= Just (passiveMouseMotion inputSt)

  mainLoop
  where   
    display st globalStRef inputStRef = do      
      loadIdentity
      updateState (\s -> procDraw p s >> return s) st globalStRef inputStRef         
      swapBuffers
      updateFrameCount globalStRef

    idle st globalStRef inputStRef = do
      updateState (procUpdate p) st globalStRef inputStRef
      updateStateWithTime (procUpdateTime p) st globalStRef inputStRef
      postRedisplay Nothing

    updateState f st globalStRef inputStRef = do
      s <- G.get st
      inputSt <- G.get inputStRef
      globalSt <- G.get globalStRef
      (s1, globalSt1) <- runPio (f s) (globalSt { globalInputState = inputSt })
      globalStRef $= globalSt1
      st $=! s1

    updateStateWithTime f st globalStRef inputStRef = do
      s <- G.get st
      inputSt <- G.get inputStRef
      globalSt <- G.get globalStRef 

      let prevTime = globalLastTime globalSt
      now <- getCurrentTime
      let dt = fromRational $ toRational $ diffUTCTime now prevTime
          globalSt1 = globalSt { globalLastTime = now }
      (s1, globalSt2) <- runPio (f dt s) (globalSt1 { globalInputState = inputSt })
      globalStRef $= globalSt2
      st $=! s1

    saveKeyPress key inputStRef = do
      inputSt <- G.get inputStRef
      inputStRef $= inputSt { lastPressedKey = key }

    saveMouseButton mb inputStRef = do
      inputSt <- G.get inputStRef
      inputStRef $= inputSt { pressedButton = mb }

    keyMouse st globalStRef inputStRef key keyState modifiers pos = do
      inputSt <- G.get inputStRef
      let inputSt1 = inputSt { lastPressedKey = key, pressedModifiers = modifiers, mousePosition = fromPosition pos }
      case keyState of 
        Down -> do
          case key of
            Char ch -> do
              saveKeyPress (Char ch) inputStRef
              updateState (procKeyPressed p) st globalStRef inputStRef
            SpecialKey sk -> do
              saveKeyPress (SpecialKey sk) inputStRef 
              updateState (procKeyPressed p) st globalStRef inputStRef
            MouseButton mb -> do
              saveMouseButton (Just mb) inputStRef
              updateState (procMousePressed p) st globalStRef inputStRef
        Up   -> 
          case key of
            Char ch -> updateState (procKeyReleased p) st globalStRef inputStRef
            SpecialKey sk -> return ()
            MouseButton mb -> do
              saveMouseButton Nothing inputStRef
              updateState (procMouseReleased p) st globalStRef inputStRef
      inputStRef $=! inputSt1

    mouseMotion inputStRef pos = do
      writePosition inputStRef pos

    passiveMouseMotion inputStRef pos = do
      writePosition inputStRef pos

    fromPosition (Position x y) = (fromEnum x, fromEnum y)

    writePosition ref pos = do
      inputSt <- G.get ref
      ref $=! inputSt { mousePosition = fromPosition pos }      

    updateFrameCount globalStRef = do
      globalSt <- G.get globalStRef
      globalStRef $= globalSt { globalFrameCount = globalFrameCount globalSt + 1 }
