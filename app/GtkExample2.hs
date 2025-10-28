{-# LANGUAGE OverloadedLabels, FlexibleContexts, OverloadedStrings, RecursiveDo, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module GtkExample2 where

import qualified Data.Map.Lazy as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Reflex ( Dynamic
              , Event
              , (<@>)
              , current
              , holdDyn
              , list
              , listHoldWithKey
              , mergeList
              , performEvent_
              , switch
              , joinDynThroughMap
              )
import Reflex.GI.Gtk ( ReactiveAttrOp((:==))
                     , MonadReflexGtk
                     , runGtk
                     , runReflexGtk
                     , eventOnSignal
                     , eventOnSignal0
                     , sink
                     , sinkBoxUniform
                     )
import System.Exit ( ExitCode(ExitFailure)
                   , die
                   , exitSuccess
                   , exitWith
                   )
import System.Environment ( getArgs
                          , getProgName
                          )

stringInput :: (MonadReflexGtk t m)
            => m (Gtk.Widget, Dynamic t T.Text, Event t ())
stringInput = do
  ( input
    , deleteButton
    , inputW
    ) <- runGtk $ do
    box <- Gtk.boxNew Gtk.OrientationHorizontal 0
    input <- Gtk.entryNew
    deleteButton <- Gtk.buttonNewFromIconName (Just "list-remove") $
      fromIntegral $ fromEnum Gtk.IconSizeButton
    #packStart box input True True 0
    #packStart box deleteButton False False 0
    #showAll box
    inputW <- Gtk.toWidget box
    pure (input, deleteButton, inputW)
  newTextE <- eventOnSignal input #changed (Gtk.get input #text >>=)
  textDyn <- holdDyn T.empty newTextE
  delete <- eventOnSignal0 deleteButton #clicked
  pure (inputW, textDyn, delete)

main :: IO ()
main = do
  argv <- liftA2 (:) getProgName getArgs
  Gtk.applicationNew (Just "de.weltraumschlangen.reflex-test") []
    >>= maybe
    (die "Failed to initialize GTK")
    ( \application -> do
        ret <- runReflexGtk application (Just argv) $ do
          mainWindow <- runGtk $ Gtk.applicationWindowNew application
          activate <- eventOnSignal0 application #activate

          outerBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 2
          inputBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 0
          outputBox <- Gtk.boxNew Gtk.OrientationVertical 5
          addInputButton <- runGtk $ Gtk.buttonNewFromIconName (Just "list-add") $
            fromIntegral $ fromEnum Gtk.IconSizeButton
          Gtk.set addInputButton [#label Gtk.:= "Add"]
          runGtk $ do
            #add mainWindow outerBox
            #packStart outerBox inputBox False False 0
            #packStart outerBox addInputButton False False 0
            #packStart outerBox outputBox False False 0

          addInput <- eventOnSignal0 addInputButton #clicked

          rec
            let freeKey = maybe (minBound :: Word) (succ . fst) . M.lookupMax
                          <$> current inputWidgets
                inputWidgetUpdates = mconcat
                                     [ (\k -> M.singleton k . Just) <$> freeKey <@> addInput
                                     , M.fromList . map (,Nothing) . NE.toList <$> delete
                                     ]
            inputWidgets <-
              listHoldWithKey (M.singleton 0 ()) inputWidgetUpdates $ \k () ->
              (\(widget, text, delete') -> (widget, text, k <$ delete')) <$> stringInput

            let delete = switch $ mergeList . map (\(_, _, d) -> d) . M.elems
                         <$> current inputWidgets

          sinkBoxUniform inputBox (M.map (\(w, _, _) -> w) <$> inputWidgets)
            False False 0 Gtk.PackTypeStart

          let inputTexts = joinDynThroughMap (M.map (\(_, t, _) -> t) <$> inputWidgets)

          outputWidgets <- list inputTexts $ \textDyn -> do
            label <- runGtk $ Gtk.labelNew Nothing
            sink label [#label :== textDyn]
            #show label
            pure label
          sinkBoxUniform outputBox outputWidgets True True 10 Gtk.PackTypeStart

          performEvent_ $ runGtk (#showAll mainWindow) <$ activate

        case ret of
          0 -> exitSuccess
          n -> exitWith $ ExitFailure $ fromIntegral n
    )


