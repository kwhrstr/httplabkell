{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleInstances #-}

module App where

import RIO
import Brick.BChan
import Options.Applicative
import Network.HTTP.Types
import RIO.Partial (toEnum)
import qualified RIO.Map as M 
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)
import qualified Data.CaseInsensitive as CI 
import qualified Network.Wai.Handler.Warp as WAI
import qualified Network.Wai.Handler.WarpTLS as WAI 
import Network.Wai
import qualified Data.ByteString.Builder as B
import qualified Graphics.Vty as V
import Types
import Brick (customMain)
import UI
import Network.Wai.Middleware.Cors

respHandler :: HasResponseData d => d -> BChan Requested -> Application
respHandler hasTvar bchan req send = do
  dt <- readTVarIO $ getResData hasTvar
  threadDelay $ dt ^. resDelay
  send $ responseBuilder
    (dt ^. resStatusCode)
    (M.foldrWithKey' (\k a -> (toResHeader k a :)) [] $ dt ^. resHeaders)
    $ B.byteString $ T.encodeUtf8 $ dt ^. resBody
  <* writeBChan bchan (toRequested req)
  where
    toResHeader a b = (CI.mk $ T.encodeUtf8 $ CI.foldedCase a, T.encodeUtf8 b)

middleware :: Bool -> Middleware
middleware True = simpleCors
middleware _ = id 
  
runApp :: IO ()
runApp = do
  cmd <- execParser opts
  let initialResData = ResponseData (argHeaders cmd) (argStatus cmd) (argBody cmd) (argDelay cmd)
  tvar <- newTVarIO initialResData
  chan <- newBChan 5
  vty <- buildVty
  let state = initBrickState cmd tvar
  race_ (run cmd $ middleware (argCorsEnabled cmd) $ respHandler state chan)
         $ customMain vty buildVty (Just chan) app state
  where
   opts = info (parseCmdArgs <**> helper) fullDesc
   buildVty = do
     v <- V.mkVty =<< V.standardIOConfig
     V.setMode (V.outputIface v) V.Mouse True
     pure v
   run cmd  = if argTLSEnabled cmd 
            then WAI.runTLS WAI.defaultTlsSettings $ WAI.setPort (argPort cmd) WAI.defaultSettings
            else WAI.run $ argPort cmd 


parseAutoUpdate :: Parser Bool
parseAutoUpdate = flag True False
  $ long "auto-update"
 <> short 'a'
 <> showDefault
 <> help "Auto-updates response when fields change."
 
parseBody :: Parser Text
parseBody = strOption
  $ long "body"
 <> short 'b'  
 <> value "Hello, World"
 <> help "Specifies the initial response body."

parseConfig :: Parser FilePath
parseConfig = strOption
  $ long "config"
 <> short 'c'
 <> value ""
 <> metavar "FilePath"
 <> help "Specifies custom config path."
 
parseCorsEnabled :: Parser Bool
parseCorsEnabled = switch
  $ long "cors"
 <> help "Enable CORS."
  
parseTLSEnabled :: Parser Bool
parseTLSEnabled = switch
  $ long "tls"
 <> short 't'
 <> help "Enable TLS Server"

parseCorsDisplay :: Parser Bool
parseCorsDisplay = flag True False
  $ long "cors-display"
 <> help "Display CORS requests."

parseDelay :: Parser Int
parseDelay = option (maybeReader readMaybe)
  $ long "delay"
 <> short 'd'
 <> value 0
 <> help "Specifies the initial response delay in ms."

parseHeaders :: Parser HeaderTxts
parseHeaders =  M.fromList . filter validHeader <$> some (option (maybeReader $ readHeader . T.pack)
  $ long "headers" 
  <> short 'H'
  <> help  "Specifies the initial response delay in ms."
  <> metavar "HeaderName:Value"
  ) <|> pure (M.singleton "X-Server" "HTTPLab")
  where
    readHeader txt = 
      let (key, val) = T.dropPrefix ":" <$> T.breakOn ":" txt
      in Just (CI.mk key , val)
    validHeader ("", _) = False
    validHeader (_, "") = False
    validHeader _ = True


parsePort :: Parser Int
parsePort = option (maybeReader readMaybe )
  $ long "port"
 <> short 'p'
 <> value 10080
 <> help "Specifies the port where HTTPLab will bind to."

parseStatus :: Parser Status
parseStatus = option (maybeReader $ fmap toEnum . readMaybe)
  $ long "status"
 <> short 's'
 <> value status200
 <> help "Specifies the initial response status."
 <> metavar "StatusCode"


parseCmdArgs :: Parser CmdArgs
parseCmdArgs = CmdArgs <$> parseAutoUpdate
                       <*> parseBody
                       <*> parseConfig
                       <*> parseCorsEnabled
                       <*> parseCorsDisplay
                       <*> parseDelay
                       <*> parseHeaders
                       <*> parsePort
                       <*> parseStatus
                       <*> parseTLSEnabled
                       
                       