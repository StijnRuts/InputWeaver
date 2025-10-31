module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  -- win <- Gtk.windowNew Gtk.WindowTypePopup
  Gtk.windowSetTitle win $ T.pack "Introduction"

  -- Gtk.widgetSetAppPaintable win True

  Gtk.windowSetDefaultSize win 640 480
  Gtk.windowSetResizable win False
  -- Gtk.windowMaximize win
  -- Gtk.windowFullscreen win

  -- Gtk.windowSetDecorated win False
  -- Gtk.windowSetModal win True


  -- header <- Gtk.headerBarNew
  -- Gtk.headerBarSetShowCloseButton header False
  -- Gtk.headerBarSetTitle header $ Just $ T.pack "My App"
  -- Gtk.windowSetTitlebar win $ Just header

  box <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.containerAdd win box

  msg <- Gtk.labelNew $ Just $ T.pack "Hello"
  Gtk.boxPackStart box msg True False 10

  canvas <- Gtk.drawingAreaNew
  Gtk.boxPackStart box canvas True True 0

  btn <- Gtk.buttonNewWithLabel $ T.pack "Click me!"
  Gtk.boxPackStart box btn False False 10
  _ <- Gtk.onButtonClicked btn $ Gtk.labelSetLabel msg $ T.pack "Clicked!"

  css <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData css $ BS.pack
    "window { background-color: rgba(0,0,0,0); }"

  screen <- Gdk.screenGetDefault
  case screen of
    Just scr -> Gtk.styleContextAddProviderForScreen scr css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
    Nothing -> putStrLn "No screen found!"

  -- winContext <- Gtk.widgetGetStyleContext win
  -- Gtk.styleContextAddProvider winContext css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)


  -- width  <- liftIO $ Gtk.widgetGetAllocatedWidth canvas
  -- height <- liftIO $ Gtk.widgetGetAllocatedHeight canvas

  -- Gtk.onWidgetDraw canvas $ renderWithContext (drawCanvasHandler canvas)
  _ <- Gtk.onWidgetDraw canvas $ Cairo.renderWithContext $ do
    Cairo.setSourceRGB 1 0 0
    Cairo.arc 200 150 50 0 (2 * pi)
    Cairo.fill
    return True


  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit
  Gtk.widgetShowAll win

  Gtk.main
