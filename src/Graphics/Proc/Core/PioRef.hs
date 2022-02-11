module Graphics.Proc.Core.PioRef(
  PioRef, newPioRef, readPioRef, writePioRef, modifyPioRef
) where

import Data.IORef

import Control.Monad.IO.Class

import Graphics.Proc.Core.State

-- | Datatyp for mutable variables. We can create a reference
-- and then manipulate the value with functions @readPioRef@ and @writePioRef@.
-- The API is the same as in the case of @IORef@s. It's standard way to work with mutables in haskell.
newtype PioRef a = PioRef { unPioRef :: IORef a }
  deriving (Eq)

-- | Creates a reference for a mutable value. The argument is an initial value assigned to the variable.
newPioRef :: a -> Pio (PioRef a)
newPioRef a = liftIO $ fmap PioRef $ newIORef a

-- | Reads the value from the reference.
readPioRef :: PioRef a -> Pio a
readPioRef (PioRef ref) = liftIO $ readIORef ref

-- | Writes the value to reference.
writePioRef :: PioRef a -> a -> Pio ()
writePioRef (PioRef ref) value = liftIO $ writeIORef ref value

-- | Modifies a value iside the reference with a function.
modifyPioRef :: PioRef a -> (a -> a) -> Pio ()
modifyPioRef (PioRef ref) f = liftIO $ modifyIORef ref f
