module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import Evdev (EventData (..))
import qualified Evdev
import qualified Evdev.Codes as Codes
import qualified Evdev.Uinput as Uinput
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as BF
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
      let sendOutput :: Evdev.EventData -> IO ()
          sendOutput = Uinput.writeEvent virtDev
      (addHandler, fire) <- BF.newAddHandler
      network <- BF.compile $ networkDescription addHandler sendOutput
      BF.actuate network
      forever $ do
        (Evdev.Event eventData _) <- Evdev.nextEvent realDev
        fire eventData
    _ -> putStrLn "Usage: Provide a device path such as /dev/input/eventX"

networkDescription :: BF.AddHandler Evdev.EventData -> (Evdev.EventData -> IO ()) -> BF.MomentIO ()
networkDescription addHandler sendOutput = do
  eInput <- BF.fromAddHandler addHandler
  let eOutput = swapKey <$> eInput
  let eIO = sendOutput <$> eOutput
  BF.reactimate eIO

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
