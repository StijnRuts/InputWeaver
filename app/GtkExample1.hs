{-# LANGUAGE OverloadedStrings, OverloadedLabels, FlexibleContexts #-}

import Control.Applicative (liftA2)
import GI.Gtk hiding (main)
import Reflex.GI.Gtk
import System.Environment
import System.Exit

GtkExample1 :: IO ()
GtkExample1 = do
  argv <- (:) <$> getProgName <*> getArgs
  Just gtkApplication <- applicationNew (Just "org.example.MyAwesomeApp") []
  rc <- runReflexGtk gtkApplication (Just argv) $ myReactiveCode gtkApplication
  case rc of
    0 -> exitSuccess
    n -> exitWith $ ExitFailure $ fromIntegral n

myReactiveCode :: (MonadReflexGtk t m) => Application -> m ()
myReactiveCode gtkApplication = do
  window <- runGtk $ applicationWindowNew gtkApplication
  box <- runGtk $ boxNew OrientationVertical 0
  containerAdd window box
  input1 <- runGtk entryNew
  input2 <- runGtk entryNew
  output <- runGtk $ labelNew Nothing
  runGtk $ boxPackStart box input1 False False 0
  runGtk $ boxPackStart box input2 False False 0
  runGtk $ boxPackStart box output False False 0

  text1 <- dynamicOnSignal "" input1 #changed $ \fire -> entryGetText input1 >>= fire
  text2 <- dynamicOnAttribute input2 #text
  let combinedText = liftA2 (<>) text1 text2
  sink output [#label :== combinedText]

  _ <- gtkApplication `on` #activate $ widgetShowAll window
  pure ()

