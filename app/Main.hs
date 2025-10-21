module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import Evdev (EventData (..), KeyEvent (..))
import qualified Evdev
import qualified Evdev.Codes as Codes
import qualified Evdev.Uinput as Uinput
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as BF
import System.Environment (getArgs)

main :: IO ()
main = frpExample

readExample :: IO ()
readExample = do
  args <- getArgs
  case args of
    [devicePath] -> do
      realDev <- Evdev.newDevice $ BS.pack devicePath
      putStrLn "Opened device:"
      print realDev
      forever $ do
        ev <- Evdev.nextEvent realDev
        print ev
    _ -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"

writeExample :: IO ()
writeExample = do
  virtDev <- virtualDevice
  putStrLn "Pressing A:"
  Uinput.writeEvent virtDev $ KeyEvent Codes.KeyA Pressed
  Uinput.writeEvent virtDev $ SyncEvent Codes.SynReport
  Uinput.writeEvent virtDev $ KeyEvent Codes.KeyA Released
  Uinput.writeEvent virtDev $ SyncEvent Codes.SynReport
  putStrLn "Done"

swapExample :: IO ()
swapExample = do
  realDev <- Evdev.newDevice $ BS.pack "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
  Evdev.grabDevice realDev
  virtDev <- virtualDevice
  putStrLn "Swapping A and Z keys"
  forever $ do
    (Evdev.Event eventData eventTime) <- Evdev.nextEvent realDev
    case eventData of
      (Evdev.KeyEvent code val) -> Uinput.writeEvent virtDev $ KeyEvent (swapKey code) val
      _ -> Uinput.writeEvent virtDev eventData

swapKey :: Codes.Key -> Codes.Key
swapKey kc
  | kc == Codes.KeyA = Codes.KeyZ
  | kc == Codes.KeyZ = Codes.KeyA
  | otherwise = kc

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

frpExample :: IO ()
frpExample = do
  (addHandler, fire) <- BF.newAddHandler
  network <- BF.compile $ networkDescription addHandler
  BF.actuate network
  fire ()
  fire ()
  fire ()

networkDescription :: BF.AddHandler () -> BF.MomentIO ()
networkDescription addHandler = do
  eInput <- BF.fromAddHandler addHandler
  bCount <- B.accumB (0 :: Int) ((+ 1) <$ eInput)
  eCount <- BF.changes bCount
  let eIO = fmap print <$> eCount
  BF.reactimate' eIO
