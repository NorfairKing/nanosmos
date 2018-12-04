{-# LANGUAGE OverloadedStrings #-}

module NanoSmos
    ( nanoSmos
    ) where

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Exit

import Cursor.TextField

import Brick as Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

nanoSmos :: IO ()
nanoSmos = do
    (file:_) <- getArgs
    exists <- doesFileExist file
    contents <-
        if exists
            then T.readFile file
            else pure T.empty
    let tc = makeTextFieldCursor contents
    tc' <- Brick.defaultMain nanoSmosApp tc
    T.writeFile file $ rebuildTextFieldCursor tc'

nanoSmosApp :: App TextFieldCursor e Text
nanoSmosApp =
    App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap Vty.defAttr []
        }

draw :: TextFieldCursor -> [Widget Text]
draw tc =
    [ centerLayer $
      border $
      padAll 1 $
      let (y, x) = textFieldCursorSelection tc
       in showCursor "cursor" (Location (x, y)) $
          txtWrap (rebuildTextFieldCursor tc)
    ]

handleEvent ::
       TextFieldCursor
    -> BrickEvent Text e
    -> EventM Text (Next TextFieldCursor)
handleEvent tc e =
    case e of
        VtyEvent ve ->
            case ve of
                EvKey key mods ->
                    let mDo func = continue . fromMaybe tc $ func tc
                     in case key of
                            KChar c -> continue $ textFieldCursorInsertChar c tc
                            KLeft -> mDo textFieldCursorSelectPrevChar
                            KRight -> mDo textFieldCursorSelectNextChar
                            KUp -> mDo textFieldCursorSelectPrevLine
                            KDown -> mDo textFieldCursorSelectNextLine
                            KBS -> mDo textFieldCursorRemove
                            KHome ->
                                continue $ textFieldCursorSelectStartOfLine tc
                            KEnd -> continue $ textFieldCursorSelectEndOfLine tc
                            KDel -> mDo textFieldCursorDelete
                            KEsc -> halt tc
                            KEnter -> halt tc
                            _ -> continue tc
                _ -> continue tc
        _ -> continue tc
