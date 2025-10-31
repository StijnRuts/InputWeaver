{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import Control.Applicative (liftA2)
import qualified GI.Gtk as Gtk
import qualified Reflex.GI.Gtk as RGtk
import Reflex.GI.Gtk (ReactiveAttrOp(..))
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified Reflex as R
import qualified Reflex.Spider.Internal as R
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Evdev (EventData (..))
import qualified Evdev
import qualified Evdev.Codes as Codes
import qualified Evdev.Uinput as Uinput

main :: IO ()
main = do
  argv <- (:) <$> Env.getProgName <*> Env.getArgs

  Just gtkApplication <- Gtk.applicationNew (Just "be.stijnruts.InputWeaver") []
  
  realDev <- Evdev.newDevice $ BS.pack "/dev/input/by-id/usb-Dell_Dell_USB_Entry_Keyboard-event-kbd"
  Evdev.grabDevice realDev
  putStrLn "Opened device:"
  print realDev

  virtDev <- virtualDevice
  putStrLn "Swapping A and Z keys"

  rc <- RGtk.runReflexGtk gtkApplication (Just argv) $ do
    reactiveGtk gtkApplication
    reactiveEvdev realDev virtDev

  case rc of
    0 -> Exit.exitSuccess
    n -> Exit.exitWith $ Exit.ExitFailure $ fromIntegral n

reactiveGtk :: R.HasSpiderTimeline t => Gtk.Application -> RGtk.ReflexGtk t ()
reactiveGtk gtkApplication = do
  window <- RGtk.runGtk $ Gtk.applicationWindowNew gtkApplication

  box <- RGtk.runGtk $ Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.containerAdd window box

  input1 <- RGtk.runGtk Gtk.entryNew
  input2 <- RGtk.runGtk Gtk.entryNew
  output <- RGtk.runGtk $ Gtk.labelNew Nothing
  RGtk.runGtk $ Gtk.boxPackStart box input1 False False 0
  RGtk.runGtk $ Gtk.boxPackStart box input2 False False 0
  RGtk.runGtk $ Gtk.boxPackStart box output False False 0

  text1 <- RGtk.dynamicOnSignal "" input1 #changed $ \fire -> Gtk.entryGetText input1 >>= fire
  text2 <- RGtk.dynamicOnAttribute input2 #text
  let combinedText = liftA2 (<>) text1 text2
  RGtk.sink output [#label :== combinedText]

  _ <- gtkApplication `Gtk.on` #activate $ Gtk.widgetShowAll window
  pure ()

reactiveEvdev :: R.HasSpiderTimeline t => Evdev.Device -> Uinput.Device -> RGtk.ReflexGtk t ()
reactiveEvdev realDev virtDev = do
  (myEvent, myTrigger) <- R.newTriggerEvent
  delayedEvent <- R.delay 1.0 myEvent
  _ <- R.performEvent_ $ liftIO . Uinput.writeEvent virtDev . swapKey <$> delayedEvent

  _ <- liftIO $ forkIO $ forever $ do
    (Evdev.Event eventData _) <- Evdev.nextEvent realDev
    myTrigger eventData

  pure ()

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

