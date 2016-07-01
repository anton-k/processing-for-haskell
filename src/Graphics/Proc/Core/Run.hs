{-# Language FlexibleContexts #-}
module Graphics.Proc.Core.Run(
	Proc(..), runProc, Draw
) where

import Control.Monad.IO.Class

import Data.Default
import Data.IORef

import qualified Graphics.Rendering.OpenGL as G
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.Proc.Core.State
import Graphics.Proc.Core.GLBridge


type Update s = s -> Pio s
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

instance Default (Proc s) where
    def = Proc
        { procSetup = return $ error "No setup is defined. Please define the procSetup value."
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

data St s = St
  { stUser   :: s
  , stGlobal :: GlobalState }

initSt :: Proc s -> IO (St s)
initSt p = do
  (user, global) <- runPio (procSetup p) =<< defGlobalState
  return $ St user global

updateSt :: IORef (St s) -> Update s -> IO ()
updateSt ref f = do
  st <- get ref
  (user, global) <- runPio (f (stUser st)) (stGlobal st)
  ref $= St user global

passSt :: IORef (St s) -> Pio () -> IO ()
passSt ref p = updateSt ref $ \s -> p >> return s

runProc :: Proc s -> IO ()
runProc p = do
  setupWindow
  ref <- newIORef =<< initSt p

  nextFrame ref
  displayCallback       $= display ref  
  keyboardMouseCallback $= Just (keyMouse ref)
  motionCallback        $= Just (mouseMotion ref)
  passiveMotionCallback $= Just (passiveMouseMotion ref)

  mainLoop
  where   
    display ref = updateSt ref $ \s -> do      
      liftIO $ loadIdentity
      procDraw p s      
      liftIO $ swapBuffers
      updateFrameCount
      return s

    idle ref = do
      updateSt ref $ \s -> do      
        s1 <- procUpdate p s
        dt <- getDuration
        s2 <- procUpdateTime p dt s1
        liftIO $ postRedisplay Nothing
        return s2
      nextFrame ref

    nextFrame ref = do
      timeOut <- getTimeoutInterval ref
      addTimerCallback timeOut (idle ref)

    keyMouse ref key keyState modifiers pos = updateSt ref $ \s -> do
      putPosition pos
      case keyState of 
        Down -> do
          case key of
            MouseButton mb -> do
              putMouseButton (Just mb)
              procMousePressed p s
            keyPress -> do
              putKeyPress keyPress
              procKeyPressed p s
        Up   -> 
          case key of
            Char ch -> procKeyReleased p s
            SpecialKey sk -> return s
            MouseButton mb -> do
              putMouseButton Nothing
              procMouseReleased p s     

    mouseMotion ref pos = passSt ref $ putPosition pos
    passiveMouseMotion ref pos = passSt ref $ putPosition pos

getTimeoutInterval ref = do
  st <- fmap stGlobal $ get ref
  fmap fst $ runPio getter st
  where getter = fmap (round . (1000 * ) . recip) getFrameRate
