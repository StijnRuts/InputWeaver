module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Evdev (EventData (..))
import qualified Evdev
import qualified Evdev.Codes as Codes
import qualified Evdev.Stream as ES
import qualified Evdev.Uinput as Uinput
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Internal.Data.Stream as DS
import Streamly.Internal.Data.Stream.IsStream as IS
import Streamly.Internal.Data.Stream.Serial as Serial
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [devicePath] -> do
      realDev <- Evdev.newDevice $ BS.pack devicePath
      Evdev.grabDevice realDev
      putStrLn "Opened device:"
      print realDev

      virtDev <- virtualDevice
      putStrLn "Swapping A and Z keys"

      DS.drain $ S.mapM (Uinput.writeEvent virtDev . swapKey . getEventData) $ IS.fromSerial $ ES.readEvents realDev
    _ -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"

getEventData :: Evdev.Event -> Evdev.EventData
getEventData (Evdev.Event eventData _) = eventData

swapKey :: Evdev.EventData -> Evdev.EventData
swapKey (Evdev.KeyEvent Codes.KeyA status) = Evdev.KeyEvent Codes.KeyZ status
swapKey (Evdev.KeyEvent Codes.KeyZ status) = Evdev.KeyEvent Codes.KeyA status
swapKey ev = ev

virtualDevice :: IO Uinput.Device
virtualDevice =
  Uinput.newDevice
    (BS.pack "haskell-uinput-example")
    Uinput.defaultDeviceOpts
      { Uinput.keys =
          [ Codes.KeyA
          , Codes.KeyB
          , Codes.KeyC
          , Codes.KeyD
          , Codes.KeyE
          , Codes.KeyF
          , Codes.KeyG
          , Codes.KeyH
          , Codes.KeyI
          , Codes.KeyJ
          , Codes.KeyK
          , Codes.KeyL
          , Codes.KeyM
          , Codes.KeyN
          , Codes.KeyO
          , Codes.KeyP
          , Codes.KeyQ
          , Codes.KeyR
          , Codes.KeyS
          , Codes.KeyT
          , Codes.KeyU
          , Codes.KeyV
          , Codes.KeyW
          , Codes.KeyX
          , Codes.KeyY
          , Codes.KeyZ
          ]
      }
