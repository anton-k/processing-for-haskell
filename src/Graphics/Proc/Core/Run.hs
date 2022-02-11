{-# Language FlexibleContexts #-}
module Graphics.Proc.Core.Run(
  Proc(..), runProc, Draw, Update, TimeInterval
) where

import Control.Monad.IO.Class

import Data.Default
import Data.IORef

import qualified Graphics.Rendering.OpenGL as G
import qualified Graphics.UI.GLUT as G
import Graphics.UI.GLUT (($=))

import Graphics.Proc.Core.State
import Graphics.Proc.Core.GLBridge


-- | A alias for value update inside processing IO-monad.
type Update s = s -> Pio s

-- | An alias for processing procedures.
type Draw = Pio ()

-- | It holds all processing standard callbacks.
-- With it we can set the setup, draw, and update functions.
-- Here we can specify how to react on user-input.
--
-- All functions update the program state. They take it in as an argument and produce as result.
-- In Haskell we can not manipulate global variables with such ease as Processing provides.
-- So we have to find out another way to update the state. The natural way for Haskell is to keep
-- the things as explicit as possible. That leads to the following decisions:
--
-- * @setup@ returns the initial state.
--
-- * @draw@ takes the state as an argument and draws it.
--
-- * @update@ should take in the current state and return back the next state.
--
-- * All input processing functions also manipulate the state explicitly by passing arguments.
--
-- Notice that the processing function draw is split on two functions: draw and update.
-- The draw is only for drawing the program state and update is for state update.
--
-- There is a useful function procUpdateTime that provides a time interval that has passed since
-- the previous update of the state. It can be useful for physics engines.
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
  st <- G.get ref
  (user, global) <- runPio (f (stUser st)) (stGlobal st)
  ref $= St user global

passSt :: IORef (St s) -> Pio () -> IO ()
passSt ref p = updateSt ref $ \s -> p >> return s

-- | The main function for rendering processing actions.
-- It sets the scene and starts the rendering of animation.
runProc :: Proc s -> IO ()
runProc p = do
  setupWindow
  ref <- newIORef =<< initSt p

  nextFrame ref
  G.displayCallback       $= display ref
  G.keyboardMouseCallback $= Just (keyMouse ref)
  G.motionCallback        $= Just (mouseMotion ref)
  G.passiveMotionCallback $= Just (passiveMouseMotion ref)

  G.mainLoop
  where
    display ref = updateSt ref $ \s -> do
      liftIO $ G.loadIdentity
      procDraw p s
      liftIO $ G.swapBuffers
      updateFrameCount
      return s

    idle ref = do
      loopInfo <- getLoopInfo ref
      case loopInfo of
        Loop   -> updateLoopState ref
        NoLoop -> return ()
        Redraw -> updateLoopState ref >> passSt ref (putLoopMode NoLoop)
      nextFrame ref

    updateLoopState ref =  updateSt ref $ \s -> do
        s1 <- procUpdate p s
        dt <- getDuration
        s2 <- procUpdateTime p dt s1
        liftIO $ G.postRedisplay Nothing
        return s2

    nextFrame ref = do
      timeOut <- getTimeoutInterval ref
      G.addTimerCallback timeOut (idle ref)

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

getTimeoutInterval ref = readRef getter ref
  where getter = fmap (round . (1000 * ) . recip) getFrameRate

getLoopInfo ref = readRef getLoopMode ref

readRef getter ref = do
  st <- fmap stGlobal $ G.get ref
  fmap fst $ runPio getter st

