module System.Console.Terminal.Posix
  ( size, fdSize, hSize
  ) where

import System.Console.Terminal.Common
import Control.Exception (catch)
import Data.Typeable (cast)
import Foreign
import Foreign.C.Error
import Foreign.C.Types
import GHC.IO.FD (FD(FD, fdFD))
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Types (Handle, Handle__(Handle__, haDevice))
import System.Posix.Types (Fd(Fd))


-- Interesting part of @struct winsize@
data CWin = CWin CUShort CUShort


fdSize :: Integral n => Fd -> IO (Maybe (Window n))
fdSize (Fd fd) =
  return . Just $ Window 25 80

size :: Integral n => IO (Maybe (Window n))
size = fdSize (Fd undefined)

hSize :: Integral n => Handle -> IO (Maybe (Window n))
hSize h = withHandle_ "hSize" h $ \Handle__ { haDevice = dev } ->
  case cast dev of
    Nothing -> return Nothing
    Just FD { fdFD = fd } -> fdSize (Fd fd)
