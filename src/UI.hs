{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UI where

import RIO
import RIO.Partial (toEnum)
import RIO.Char
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import Brick (App(..), AttrMap, AttrName, (<+>), (<=>))
import Brick.Types
import qualified Brick as B
import qualified Brick.Widgets.Edit as BE
import Brick.Widgets.Edit (Editor(..), DecodeUtf8, handleEditorEvent)
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as B
import qualified Brick.AttrMap as BA
import qualified Graphics.Vty as V
import Graphics.Vty (Event(..), Key(..))
import qualified Data.Text.Zipper as Z
import Types
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)
import qualified RIO.Map as M
import qualified Data.CaseInsensitive as CI



app :: App BrickState Requested Name
app = App
  { appDraw = drawUI
  , appChooseCursor = BF.focusRingCursor $ view bsFocus
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const theMap
  }

drawUI :: BrickState -> [Widget Name]
drawUI st = [B.hLimitPercent 60 requestedWidget <+> responseWidget <=> infoTxt]
  where
    responseWidget = B.vLimitPercent 50
      ( B.vBox [ statusWidget
               , delayWidget
               , headersWidget
               ]
      )
     <=> bodyWidget
    infoTxt = B.withAttr infoAttr $ B.txt editingInfo
    editor ed
      = BE.renderEditor (B.txt . T.unlines) (foc $ B.getName ed) ed
    editorWithLabel l fAttr ed = B.borderWithLabel (B.txt l) $ fAttr $ editor ed
    statusWidget = editorWithLabel "status"  id $ st ^. bsEditorStatus
    delayWidget = editorWithLabel "Delay (ms)" id $ st ^. bsEditorDelay
    headersWidget = editorWithLabel "Headers" editingHeader $  st ^. bsEditorHeaders
    bodyWidget = editorWithLabel "Body"  validateBody $ st ^. bsEditorBody
    foc n = BF.focusGetCurrent (st ^. bsFocus) == Just n
    requestedWidget = B.borderWithLabel (B.txt $ reqLabelTxt $ st ^. bsRequested)
                    $ B.viewport NameRequest B.Both $ requestedRender $ st ^. bsRequested
    reqLabelTxt reqz = case reqzLength reqz of
      0 -> "request"
      _ -> "request (" <> textDisplay (reqzCurPos reqz)
       <> "/" <> textDisplay (reqzLength  reqz) <> ")"
    savedHeader = st ^. bsRawHeader == BE.getEditContents (st ^. bsEditorHeaders)
    editingInfo = if savedHeader
                then ""
                else "Header is editing. Press Ctrl + a to save Header"
    editingHeader = if savedHeader
                    then id
                    else B.forceAttr editingAttr
    validateBody = if st ^. bsValidateBody
                   then id
                   else B.forceAttr invalidBodyAttr
 

-- | Main even handler for brick events
--
handleEvent ::  BrickState -> BrickEvent Name Requested -> EventM Name (Next BrickState)
handleEvent st ev = case ev of
  AppEvent (req :: Requested) -> B.continue $ st & over bsRequested (insertReqz req)
  MouseDown n _ _ _ -> B.continue $ st & over bsFocus (BF.focusSetCurrent n)
                                       & set bsValidateBody
                                         (isValidateBody ( st ^. bsBodyJsonMode )
                                                         $ T.intercalate "\n" $ BE.getEditContents $ st ^. bsEditorBody)
  MouseUp n _ _  -> B.continue $ st & over bsFocus (BF.focusSetCurrent n)
                                    & set bsValidateBody
                                      (isValidateBody ( st ^. bsBodyJsonMode)
                                                      $ T.intercalate  "\n" $ BE.getEditContents $ st ^. bsEditorBody)
  VtyEvent ve@(V.EvKey k ms) ->
    case (k, ms) of
      (V.KEsc, []) -> B.halt st
      (V.KChar '\t', _) -> B.continue $ st & over bsFocus BF.focusNext
      (V.KBackTab, []) -> B.continue $ st & over bsFocus BF.focusPrev
      (V.KPageDown, []) -> B.continue $ st & over bsRequested moveBelowReqz
      (V.KPageUp, []) -> B.continue $ st & over bsRequested moveAboveReqz
      (V.KChar 'a', [V.MCtrl]) -> do --  save Header
        let rawHeaders = BE.getEditContents $ st ^. bsEditorHeaders
            withFast f (a, b) = (f a, b)
            headerTxts = M.filterWithKey (\key a -> not (T.null $ CI.original key) && not (T.null a))
                                        $ M.fromList $ map (withFast CI.mk . fmap (T.strip . T.dropPrefix ":") . T.breakOn ":" ) rawHeaders
            lookupContentType = M.lookup "Content-Type"  headerTxts
            jsonMode = lookupContentType == Just "application/json"
        atomically $ modifyTVar (st ^. bsResData) $ set resHeaders headerTxts
        B.continue $ st & set bsRawHeader rawHeaders
                        & set bsBodyJsonMode jsonMode
                        & set bsValidateBody (isValidateBody
                                                jsonMode
                                                $ T.intercalate "\n" $ BE.getEditContents $ st ^. bsEditorBody)
      _ -> case BF.focusGetCurrent $ st ^. bsFocus of
        Just NameStatus -> do
          r <- handleNumEditorEvent 3 ve $ st ^. bsEditorStatus
          let stCode = fromMaybe 0 $ readMaybe @Int $ T.unpack $ T.concat $ BE.getEditContents r
          atomically $ modifyTVar (st ^. bsResData) $ set resStatusCode $ toEnum stCode
          B.continue $ st & set bsEditorStatus r
        Just NameDelay -> do
          r <- handleNumEditorEvent 9 ve $ st ^. bsEditorDelay
          let delay = fromMaybe 0 $ readMaybe @Int $ T.unpack $ T.concat $ BE.getEditContents r
          atomically $ modifyTVar (st ^. bsResData) $ set resDelay delay
          B.continue $ st & set bsEditorDelay r
        Just NameHeaders -> do --  edit header without save value
          r <- BE.handleEditorEvent ve $ st ^. bsEditorHeaders
          B.continue $ st & set bsEditorHeaders r
        Just NameBody -> do --  edit header without save value
          r <- BE.handleEditorEvent ve $ st ^. bsEditorBody
          let rawBody = T.intercalate "\n" $ BE.getEditContents r
          atomically $ modifyTVar (st ^. bsResData) $ set resBody rawBody
          B.continue $ st & set bsEditorBody r
                          & set bsValidateBody  (isValidateBody (st ^. bsBodyJsonMode) rawBody )
        _ -> B.continue st
  _ -> B.continue st
  where
    isValidateBody jsonMode txt = not jsonMode
                               || isJust (Aeson.decodeStrict @Value (T.encodeUtf8 txt))
  
  

handleNumEditorEvent :: (DecodeUtf8 t, Eq t, Monoid t) => Int -> Event -> Editor t n -> EventM n (Editor t n)
handleNumEditorEvent limit ev ed = case ev of
  EvKey (KChar c) []
    | not $ isDigit c -> pure ed
    | sum (Z.lineLengths $ ed ^. BE.editContentsL) >= limit -> pure ed
    | otherwise -> handleEditorEvent ev ed
  _ -> handleEditorEvent ev ed
  
  
keyAttr :: AttrName
keyAttr = "key"

valAttr :: AttrName
valAttr = "val"

reqAttr :: AttrName
reqAttr = "req"

editingAttr :: AttrName
editingAttr = "editing"

invalidBodyAttr :: AttrName
invalidBodyAttr = "invalidBody"

infoAttr :: AttrName
infoAttr = "infoEdit"

requestedTxt :: RequestedData -> Text
requestedTxt reqz = case reqzCurrent reqz of
  Nothing -> ""
  Just req
    ->  req ^. reqMethod <> " "
    <> req ^. reqPathInfo <> " "
    <> "HTTP/"
    <> T.pack (show $ req ^. reqMajor) <> "."
    <> T.pack (show $ req ^. reqMinor)
    <> T.intercalate "\n" (map (\(a, b) -> a <> ": " <> b) $ req ^. reqHeaders)

requestedRender :: RequestedData -> Widget Name
requestedRender reqz = case reqzCurrent reqz of
  Nothing -> B.txt ""
  Just req
    -> B.withAttr reqAttr (B.txt $ req ^. reqMethod <> " ")
   <+> B.txt (req ^. reqPathInfo <> " ")
   <+> B.withAttr reqAttr (B.txt "HTTP") <+> B.txt "/"
   <+> B.str (show $ req ^. reqMajor) <+> B.str "."
   <+> B.str (show $ req ^. reqMinor)
   <=> B.vBox (map toHeader $ req ^. reqHeaders )
 where
  toHeader (a, b) =  B.withAttr keyAttr (B.txt a) <+> B.txt ": "
                 <+> B.withAttr valAttr (B.txt b)
 


theMap :: AttrMap
theMap = BA.attrMap V.defAttr
       [ (BE.editAttr, V.black `B.on` V.cyan)
       , (BE.editFocusedAttr, V.black `B.on` V.yellow)
       , (B.borderAttr, B.fg V.cyan)
       , (editingAttr, V.black `B.on` V.white)
       , (invalidBodyAttr, V.black `B.on` V.red)
       , (keyAttr, B.fg V.brightRed)
       , (valAttr, B.fg V.brightGreen)
       , (reqAttr, B.fg V.brightBlue)
       , (infoAttr, B.fg V.green)
       ]
    



