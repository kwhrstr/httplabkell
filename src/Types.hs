{-# LANGUAGE FlexibleInstances #-}

module Types where

import RIO
import RIO.List
import Network.HTTP.Types
import Brick.Widgets.Edit (Editor, editor)
import Brick.Focus (FocusRing, focusRing)
import qualified RIO.Text as T
import qualified RIO.Map as M
import Network.Wai
import qualified Data.CaseInsensitive as CI 


type HeaderTxt = (Text, Text)
type RequestedData = RequestedZipper Requested
type HeaderTxts = Map (CI.CI Text) Text

data CmdArgs = CmdArgs
  { argAutoUpdate :: Bool
  , argBody :: Text
  , argConfig :: FilePath
  , argCorsEnabled :: Bool
  , argCorsDisplay :: Bool
  , argDelay :: Int
  , argHeaders :: HeaderTxts
  , argPort :: Int
  , argStatus :: Status
  }

data ResponseData = ResponseData
  { _resHeaders :: HeaderTxts
  , _resStatusCode :: Status
  , _resBody :: Text
  , _resDelay :: Int
  } deriving Show
  
resHeaders :: Lens' ResponseData HeaderTxts
resHeaders = lens _resHeaders $ \s t -> s { _resHeaders = t }

resStatusCode :: Lens' ResponseData Status
resStatusCode = lens _resStatusCode $ \s t -> s { _resStatusCode = t }

resBody :: Lens' ResponseData Text
resBody = lens _resBody $ \s t -> s { _resBody = t }

resDelay :: Lens' ResponseData Int
resDelay = lens _resDelay $ \s t -> s { _resDelay = t }

class HasResponseData a where
  getResData :: a -> TVar ResponseData

instance HasResponseData (TVar ResponseData) where
  getResData = id

data Name = NameStatus
          | NameDelay
          | NameHeaders
          | NameBody
          | NameRequest
          deriving (Show, Eq, Ord, Enum, Bounded)
  
data BrickState = BrickState
  { _bsEditorStatus :: Editor Text Name
  , _bsEditorDelay :: Editor Text Name
  , _bsEditorHeaders :: Editor Text Name
  , _bsEditorBody :: Editor Text Name
  , _bsEditorRequest :: Editor Text Name
  , _bsFocus :: FocusRing Name
  , _bsResData :: TVar ResponseData
  , _bsRequested :: RequestedData
  , _bsRawHeader :: [Text]
  , _bsValidateBody :: Bool
  , _bsBodyJsonMode :: Bool
  }
bsEditorStatus :: Lens' BrickState (Editor Text Name)
bsEditorStatus = lens _bsEditorStatus $ \s t -> s {_bsEditorStatus = t}

bsEditorDelay :: Lens' BrickState (Editor Text Name)
bsEditorDelay = lens _bsEditorDelay $ \s t -> s {_bsEditorDelay = t}

bsEditorHeaders :: Lens' BrickState (Editor Text Name)
bsEditorHeaders = lens _bsEditorHeaders $ \s t -> s {_bsEditorHeaders = t}

bsEditorBody :: Lens' BrickState (Editor Text Name)
bsEditorBody = lens _bsEditorBody $ \s t -> s {_bsEditorBody = t}

bsEditorRequest :: Lens' BrickState (Editor Text Name)
bsEditorRequest = lens _bsEditorRequest $ \s t -> s {_bsEditorRequest = t}

bsFocus :: Lens' BrickState (FocusRing Name)
bsFocus = lens _bsFocus $ \s t -> s { _bsFocus = t }

bsResData :: Lens' BrickState (TVar ResponseData)
bsResData = lens _bsResData $ \s t -> s { _bsResData = t }

bsRequested :: Lens' BrickState RequestedData
bsRequested = lens _bsRequested $ \s t -> s { _bsRequested = t }

bsRawHeader :: Lens' BrickState [Text]
bsRawHeader = lens _bsRawHeader $ \s t -> s { _bsRawHeader = t }

bsValidateBody :: Lens' BrickState Bool
bsValidateBody = lens _bsValidateBody $ \s t -> s { _bsValidateBody = t }

bsBodyJsonMode :: Lens' BrickState Bool
bsBodyJsonMode = lens _bsBodyJsonMode $ \s t -> s { _bsBodyJsonMode = t }

initBrickState :: CmdArgs -> TVar ResponseData -> BrickState
initBrickState args tvar = BrickState
  { _bsEditorStatus = editor NameStatus (Just 1) $ T.pack $ show $ fromEnum $ argStatus args
  , _bsEditorDelay = editor NameDelay (Just 1) $ T.pack $ show $ argDelay args
  , _bsEditorHeaders = editor NameHeaders Nothing $ T.intercalate "\n" $ M.foldrWithKey (\k a  -> (resHeaderToUI k a :)) [] $ argHeaders args
  , _bsEditorBody = editor NameBody Nothing $ argBody args
  , _bsEditorRequest = editor NameRequest Nothing ""
  , _bsFocus = focusRing [NameStatus .. NameBody]
  , _bsResData = tvar
  , _bsRequested = initReqZipper
  , _bsRawHeader = M.foldrWithKey (\k a  -> (resHeaderToUI k a :)) [] $ argHeaders args
  , _bsValidateBody = True
  , _bsBodyJsonMode = False
  }
  where
    resHeaderToUI a b = CI.original a <> ": " <> b
    
instance HasResponseData BrickState where
  getResData = view bsResData
  
data Requested = Requested
  { _reqMethod :: Text
  , _reqPathInfo :: Text
  , _reqMajor :: Int
  , _reqMinor :: Int
  , _reqHeaders :: [HeaderTxt]
  }
reqMethod :: Lens' Requested Text
reqMethod = lens _reqMethod $ \s t -> s { _reqMethod = t }

reqPathInfo :: Lens' Requested Text
reqPathInfo = lens _reqPathInfo $ \s t -> s { _reqPathInfo = t }

reqMajor :: Lens' Requested Int
reqMajor = lens _reqMajor $ \s t -> s { _reqMajor = t }

reqMinor :: Lens' Requested Int
reqMinor = lens _reqMinor $ \s t -> s { _reqMinor = t }

reqHeaders :: Lens' Requested [HeaderTxt]
reqHeaders = lens _reqHeaders $ \s t -> s { _reqHeaders = t }

toRequested :: Request -> Requested
toRequested req = 
  let m = either (const "GET") id $  T.decodeUtf8' $ requestMethod req
      u = mappend "/" $ T.intercalate "/" $ pathInfo req
      major = httpMajor $ httpVersion req
      minor = httpMinor $ httpVersion req
      key = either (const "") id . T.decodeUtf8' . CI.original . fst 
      val = either (const "") id . T.decodeUtf8' . snd
      hs = filter (not . T.null . fst)  $ map (\h -> ( key h, val h)) $ sortOn fst $ requestHeaders req
  in Requested
      { _reqMethod = m
      , _reqPathInfo = u
      , _reqMajor = major
      , _reqMinor = minor
      , _reqHeaders = hs
      }

data RequestedZipper a = RZ
  { _reqzAbove :: [a]
  , _reqzBelow :: [a]
  , _reqzCurrent :: Maybe a
  , _reqzCurPos :: Int
  , _reqzLength :: Int
  }
reqzLength :: RequestedZipper a -> Int
reqzLength = _reqzLength

reqzCurPos :: RequestedZipper a -> Int
reqzCurPos = _reqzCurPos

reqzCurrent :: RequestedZipper a -> Maybe a
reqzCurrent = _reqzCurrent

initReqZipper :: RequestedZipper Requested
initReqZipper = RZ
  { _reqzAbove = []
  , _reqzBelow = []
  , _reqzCurrent = Nothing
  , _reqzCurPos = 0
  , _reqzLength = 0
  }
  
moveAboveReqz :: RequestedZipper a -> RequestedZipper a
moveAboveReqz z@RZ{_reqzAbove = []} = z
moveAboveReqz z@RZ{_reqzCurrent = Nothing} = z
moveAboveReqz z@RZ{_reqzAbove = b:bs, _reqzCurrent = Just c} =
  z { _reqzAbove = bs
    , _reqzCurrent = Just b
    , _reqzBelow = c:_reqzBelow z
    , _reqzCurPos = _reqzCurPos z - 1
    }

moveBelowReqz :: RequestedZipper a -> RequestedZipper a
moveBelowReqz z@RZ{_reqzBelow = []} = z
moveBelowReqz z@RZ{_reqzCurrent = Nothing} = z
moveBelowReqz z@RZ{_reqzBelow = b:bs, _reqzCurrent = Just c} = z
  { _reqzBelow = bs
  , _reqzCurrent = Just b
  , _reqzAbove = c:_reqzAbove z
  , _reqzCurPos = _reqzCurPos z + 1
  }

insertReqz :: a -> RequestedZipper a -> RequestedZipper a
insertReqz a z@RZ{_reqzCurrent = Nothing} = z
  { _reqzCurrent = Just a
  , _reqzCurPos = _reqzCurPos z + 1
  , _reqzLength = _reqzLength z + 1
  }
insertReqz a z@RZ{_reqzCurrent = Just c, _reqzBelow = []} = z
  { _reqzCurrent = Just a
  , _reqzAbove = c : _reqzAbove z
  , _reqzCurPos = _reqzCurPos z + 1
  , _reqzLength = _reqzLength z + 1
  }
insertReqz a z = z
  { _reqzBelow = _reqzBelow z <> [a]
  , _reqzLength = _reqzLength z + 1
  }
  