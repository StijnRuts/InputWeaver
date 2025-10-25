module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import Evdev (EventData (..), KeyEvent (..))
import qualified Evdev
import qualified Evdev.Codes as Codes
import qualified Evdev.Uinput as Uinput
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

      eventChan <- newChan
      forkIO $ forever $ do
        ev <- readChan eventChan
        -- threadDelay (1 * 1000000)
        Uinput.writeEvent virtDev $ mapKeyCode swapKey $ getEventData ev

      forever $ do
        evdevEvent <- Evdev.nextEvent realDev
        writeChan eventChan evdevEvent

    _ -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"

getEventData :: Evdev.Event -> Evdev.EventData
getEventData (Evdev.Event eventData _) = eventData

mapKeyCode :: (Codes.Key -> Codes.Key) -> Evdev.EventData -> Evdev.EventData
mapKeyCode f (Evdev.KeyEvent code val) = Evdev.KeyEvent (f code) val
mapKeyCode _ eventData = eventData

swapKey :: Codes.Key -> Codes.Key
swapKey kc
  | kc == Codes.KeyA = Codes.KeyZ
  | kc == Codes.KeyZ = Codes.KeyA
  | otherwise = kc

delayExample :: IO ()
delayExample = do
  putStrLn "Main thread starts"
  forkIO $ do
    threadDelay (5 * 1000000)
    putStrLn "Delayed IO action executed!"
  putStrLn "Main thread is free to do other work."
  forever $ do
    threadDelay (1 * 1000000)
    putStrLn "Tick"

virtualDevice :: IO Uinput.Device
virtualDevice =
  Uinput.newDevice
    (BS.pack "haskell-uinput-example")
    Uinput.defaultDeviceOpts
      { Uinput.keys =
          [ Codes.KeyA,
            Codes.KeyB,
            Codes.KeyC,
            Codes.KeyD,
            Codes.KeyE,
            Codes.KeyF,
            Codes.KeyG,
            Codes.KeyH,
            Codes.KeyI,
            Codes.KeyJ,
            Codes.KeyK,
            Codes.KeyL,
            Codes.KeyM,
            Codes.KeyN,
            Codes.KeyO,
            Codes.KeyP,
            Codes.KeyQ,
            Codes.KeyR,
            Codes.KeyS,
            Codes.KeyT,
            Codes.KeyU,
            Codes.KeyV,
            Codes.KeyW,
            Codes.KeyX,
            Codes.KeyY,
            Codes.KeyZ
          ]
      }
