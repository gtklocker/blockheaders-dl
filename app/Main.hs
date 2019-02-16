{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types     hiding (Error)
import           Data.Conduit.Network
import           Data.Scientific
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
import           Network.JSONRPC
import           UnliftIO.Concurrent

type TxSize = Int

type Fee = Scientific

data Req =
  EstimateFee TxSize
  deriving (Show, Eq)

{-
instance FromRequest Req where
  parseParams "blockchain.block.header" = Just $ const $ return Header
  parseParams _                         = Nothing
-}
instance ToRequest Req where
  requestMethod (EstimateFee _) = "blockchain.estimatefee"
  requestIsNotif = const False

instance ToJSON Req where
  toJSON (EstimateFee txSize) = toJSON [txSize]

data Res = Fee
  { getFee :: Fee
  } deriving (Show, Eq)

instance FromResponse Res where
  parseResult "blockchain.estimatefee" =
    return $ withScientific "fee" $ \fee -> return $ Fee fee
  parseResult _ = Nothing

instance ToJSON Res where
  toJSON (Fee fee) = toJSON fee

handleResponse :: Maybe (Either ErrorObj Res) -> Res
handleResponse t =
  case t of
    Nothing        -> error "could not receive or parse response"
    Just (Left e)  -> error $ fromError e
    Just (Right r) -> r

req :: MonadLoggerIO m => JSONRPCT m Res
req = do
  tEM <- sendRequest $ EstimateFee 6
  $(logDebug) "sending estimate fee request"
  return $ handleResponse tEM

{-
reqBatch :: MonadLoggerIO m => JSONRPCT m [Res]
reqBatch = do
  $(logDebug) "sending pings"
  tEMs <- sendBatchRequest $ replicate 2 Ping
  return $ map handleResponse tEMs
-}
main :: IO ()
main =
  runStderrLoggingT $
  jsonrpcTCPClient V2 True (clientSettings 50001 "electrum-server.ninja") $ do
    req >>= $(logDebug) . T.pack . ("response: " ++) . show
    -- $(logDebug) "sending two time requests one second apart"
    -- replicateM_ 2 $ do
      -- liftIO (threadDelay 1000000)
    -- $(logDebug) "sending two pings in a batch"
    -- reqBatch >>= $(logDebug) . T.pack . ("response: " ++) . show
