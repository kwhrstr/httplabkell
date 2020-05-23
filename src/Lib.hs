module Lib where

import RIO hiding (SimpleApp)
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import qualified RIO.ByteString as B
import Data.ByteString.Builder (byteString)
import Control.Monad.Trans.Resource
import System.IO (openBinaryFile, print)
import Data.Aeson


--type SimpleApp = Request -> IO Response
--
--unSimpleApp :: SimpleApp -> Application
--unSimpleApp sapp r f = do
-- res <- sapp r
-- f res




--exec :: IO ()
--exec = run 3000 $ \req send -> do   
-- print req
-- case pathInfo req of
--  [] -> send $ responseBuilder
--   status303
--   [("Location", "/home")]
--   "Redirecting"
--  ["home"] -> send $ responseBuilder
--   status200
--   [("Content-Type", "text/plain")]
--   "This is the home route"

--exec :: IO ()
--exec = run 3000 $ \req send -> send $ responseBuilder
--  status200
--  ( case lookup "marco" $ requestHeaders req of
--      Nothing -> []
--      Just val -> [("Polo", val)] 
--  )
--  "Hello WAI!"


files :: [FilePath]
files = ["file1.txt", "file2.txt"]

withBinaryFiles :: [FilePath] -> IOMode -> ([Handle] -> IO a) -> IO a
withBinaryFiles fps mode inner =
  loop fps id
  where
    loop [] front = inner $ front []
    loop (x:xs) front =
      withBinaryFile x mode $ \h ->
        loop xs (front . (h:))

--main :: IO ()
--main = run 3000 $ \_req send -> withBinaryFiles files ReadMode $ \hs ->
--  send $ responseStream
--    status200
--    [("Content-Type", "text/plain")]
--    $ \chunk _flush -> forM_ hs $ \h -> fix $ \loop -> do
--      bs <- B.hGetSome h 4096
--      unless (B.null bs) $ do
--        chunk $ byteString bs
--        loop

main :: IO ()
main = run 3000 $ \req send ->
  bracket createInternalState closeInternalState $ \is -> do
  print req
  send $ responseStream
    status200
    [("Content-Type", "texp/plain")]
    $ \chunk _flush -> runInternalState (forM_ files $ \file -> do
      (releaseKey, h) <- allocate
        (openBinaryFile file ReadMode)
        hClose
      lift $ fix $ \loop -> do
        bs <- B.hGetSome h 4096
        unless (B.null bs) $ do
          chunk $ byteString bs
          loop
      release releaseKey) is


--main :: IO ()
--main = do
--  let req = setRequestMethod "POST"
--          $ setRequestBodyJSON (object ["hello" .= (1 :: Int)])
--            "http://localhost:3000"
--  res <- httpJSON req
--  print (res :: Response Value) 

