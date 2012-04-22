module Main where

import System.Environment
import System.Console.GetOpt
import Hierarchy
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as ModelView
import Data.Tree


myTreeViewNew :: TreeStore (String) -> (Forest String) -> IO (TreeView)
myTreeViewNew contents tree = do
   treeview <- ModelView.treeViewNewWithModel contents
   ModelView.treeViewSetHeadersVisible treeview True

   col <- ModelView.treeViewColumnNew
   ModelView.treeViewColumnSetTitle col "Tree"
   ModelView.treeViewAppendColumn treeview col

   renderer <- ModelView.cellRendererTextNew
   ModelView.cellLayoutPackStart col renderer False
   ModelView.cellLayoutSetAttributes col renderer contents
           $ \ind -> [ModelView.cellText := ind]

   ModelView.treeViewExpandAll treeview

   return treeview

update :: TreeStore (String) -> TreeView -> (Forest String) -> IO ()
update ts tv newForest = do 
    treeStoreClear ts 
    treeStoreInsertForest ts [] 0 newForest
    treeViewExpandAll tv

performSearch :: TreeStore (String) -> TreeView -> TreeStore (String) -> (Forest String) -> Entry -> IO ()
performSearch searchResultStore searchResultView bottomView fullModel e = do
   txt <- entryGetText e
   let filteredModel = filterModelForest fullModel txt
   update searchResultStore searchResultView filteredModel
   treeStoreClear bottomView

onMainSelectionChanged :: Forest String -> TreeStore String -> ModelView.TreeSelection -> TreeStore String -> TreeView -> IO ()
onMainSelectionChanged fullForest treeStore treeSelection bottomStore bottomView = do
   selectedRows <- ModelView.treeSelectionGetSelectedRows treeSelection
   selectedRowText <- ModelView.treeStoreGetValue treeStore (head selectedRows)
   update bottomStore bottomView (filterModelForest fullForest selectedRowText)

scrolled windowtoscroll = do
   scrolledWindow <- scrolledWindowNew Nothing Nothing
   scrolledWindowAddWithViewport scrolledWindow windowtoscroll
   return scrolledWindow

main :: IO ()
main = do
   args <- getArgs
   let ( actions, nonOpts, msgs ) = getOpt RequireOrder [ ] args
   let filename = head nonOpts 

   file <- readFile filename;
   let forest = parseForest(file)

   initGUI

   treeStore1 <- treeStoreNew forest
   treeview1 <- myTreeViewNew treeStore1 forest 
   treeStore2 <- treeStoreNew forest
   treeview2 <- myTreeViewNew treeStore2 forest 
   search <- entryNew
   textBuffer <- textBufferNew Nothing
   textBufferSetText textBuffer file
   text <- textViewNewWithBuffer textBuffer

   onEntryActivate search (performSearch treeStore1 treeview1 treeStore2 forest search)
   tree <- ModelView.treeViewGetSelection treeview1
   ModelView.treeSelectionSetMode tree SelectionSingle
   ModelView.onSelectionChanged tree (onMainSelectionChanged forest treeStore1 tree treeStore2 treeview2)

   leftbox <- vBoxNew False 0
   tv1s <- scrolled treeview1
   boxPackStart leftbox tv1s PackGrow 0
   boxPackStart leftbox search PackNatural 0

{-
   upperpaned <- hPanedNew
   panedAdd1 upperpaned leftbox 
   scrolledtext <- scrolled text
   panedAdd2 upperpaned scrolledtext
   panedSetPosition upperpaned 1000
-}

   outerpaned <- vPanedNew 
   panedAdd1 outerpaned leftbox
   tv2s <- scrolled treeview2
   panedAdd2 outerpaned tv2s
   panedSetPosition outerpaned 1000

   window <- windowNew
   set window [ windowDefaultWidth := 100
               , windowDefaultHeight := 200
               , containerChild := outerpaned
              ]

   onDestroy window mainQuit
   widgetShowAll window
   mainGUI
   return ()


