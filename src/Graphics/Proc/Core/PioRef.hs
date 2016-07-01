module Graphics.Proc.Core.PioRef(
	PioRef, newPioRef, readPioRef, writePioRef, modifyPioRef
) where

import Data.IORef

import Control.Monad.IO.Class

import Graphics.Proc.Core.State

newtype PioRef a = PioRef { unPioRef :: IORef a }
	deriving (Eq)

newPioRef :: a -> Pio (PioRef a)
newPioRef a = liftIO $ fmap PioRef $ newIORef a

readPioRef :: PioRef a -> Pio a
readPioRef (PioRef ref) = liftIO $ readIORef ref

writePioRef :: PioRef a -> a -> Pio ()
writePioRef (PioRef ref) value = liftIO $ writeIORef ref value

modifyPioRef :: PioRef a -> (a -> a) -> Pio ()
modifyPioRef (PioRef ref) f = liftIO $ modifyIORef ref f
