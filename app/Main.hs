module Main where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win $ T.pack "Introduction"

  Gtk.windowSetDefaultSize win 640 480
  Gtk.windowSetResizable win False
  -- Gtk.windowMaximize win
  -- Gtk.windowFullscreen win

  -- Gtk.windowSetDecorated win False
  -- Gtk.windowSetModal win True

  box <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.containerAdd win box

  msg <- Gtk.labelNew $ Just $ T.pack "Hello"
  Gtk.boxPackStart box msg True False 10

  btn <- Gtk.buttonNewWithLabel $ T.pack "Click me!"
  Gtk.boxPackStart box btn False False 10
  _ <- Gtk.onButtonClicked btn $ Gtk.labelSetLabel msg $ T.pack "Clicked!"

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit
  Gtk.widgetShowAll win

  Gtk.main
