{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main,
  )
where

import Control.Concurrent.STM (TChan, atomically, tryReadTChan)
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QTimer as QTimer
import Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import Graphics.UI.Qtah.Widgets.QTextEdit (QTextEdit)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import RON.Data (evalObjectState)
import qualified RON.Data.RGA as RGA
import RON.Data.RGA (RgaString)
import RON.Storage.Backend
  ( CollectionName,
    DocId (DocId),
    Document (Document, objectFrame),
    RawDocId,
  )
import qualified RON.Storage.FS as Storage
import RON.Storage.FS (Collection, loadDocument, modify, runStorage)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

theDoc :: DocId RgaString
theDoc = DocId "B3QCFGMHK1QJS-2005CRP400492"

dataDir :: FilePath
dataDir = "demo/data"

main :: IO ()
main = do
  progName <- getProgName
  storage <- Storage.newHandle dataDir
  changedDocs <- Storage.subscribe storage
  withApp $ \_ -> do
    UI {window, editor} <- setupUI progName storage
    QWidget.show window
    updateTheText storage editor
    whenUIIsIdle $ checkDBChange storage editor changedDocs
    QCoreApplication.exec
  where
    withApp = withScopedPtr $ do
      args <- getArgs
      QApplication.new args

checkDBChange
  :: Storage.Handle -> QTextEdit -> TChan (CollectionName, RawDocId) -> IO ()
checkDBChange storage editor changedDocs =
  atomically (tryReadTChan changedDocs) >>= \case
    Just (collection, docid) -> update storage editor collection docid
    Nothing -> pure ()

update :: Storage.Handle -> QTextEdit -> CollectionName -> RawDocId -> IO ()
update storage editor collection docid = case collection of
  "text"
    | DocId docid == theDoc -> updateTheText storage editor
    | otherwise ->
      hPutStrLn stderr $ "update: unknown document id " <> show docid
  _ -> hPutStrLn stderr $ "update: unknown document type " <> show docid

updateTheText :: Storage.Handle -> QTextEdit -> IO ()
updateTheText storage editor = do
  text <-
    runStorage storage $ do
      Document {objectFrame} <- loadDocument theDoc
      evalObjectState objectFrame RGA.getList
  QTextEdit.setPlainText editor text

setupUI :: String -> Storage.Handle -> IO UI
setupUI progName storage = do
  window <- QMainWindow.new
  QWidget.setWindowTitle window progName
  editor <- QTextEdit.new
  QMainWindow.setCentralWidget window editor
  connect_ editor QTextEdit.textChangedSignal $ saveTheText storage editor
  pure UI {window, editor}

instance Collection RgaString where
  collectionName = "text"

saveTheText :: Storage.Handle -> QTextEdit -> IO ()
saveTheText storage editor = do
  text <- QTextEdit.toPlainText editor
  runStorage storage $ modify theDoc $ RGA.edit text

data UI
  = UI
      { window :: QMainWindow,
        editor :: QTextEdit
      }

whenUIIsIdle :: IO () -> IO ()
whenUIIsIdle action = do
  t <- QTimer.new
  connect_ t QTimer.timeoutSignal action
  QTimer.start t 0
