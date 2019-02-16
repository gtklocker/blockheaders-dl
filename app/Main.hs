{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types     hiding (Error)
import           Data.Conduit.Network
import           Data.List
import           Data.Scientific
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
import           Network.JSONRPC
import           UnliftIO.Concurrent

type TxSize = Int

type Fee = Scientific

type Height = Int

type Count = Int

data Req
  = EstimateFee TxSize
  | GetBlockHeaders Height
                    Count
                    Height
  deriving (Show, Eq)

instance ToRequest Req where
  requestMethod (EstimateFee _)         = "blockchain.estimatefee"
  requestMethod (GetBlockHeaders _ _ _) = "blockchain.block.headers"
  requestIsNotif = const False

instance ToJSON Req where
  toJSON (EstimateFee txSize) = toJSON [txSize]
  toJSON (GetBlockHeaders startHeight count cpHeight) =
    toJSON [startHeight, count] --, cpHeight]

data Res
  = Fee { getFee :: Fee }
  | BlockHeaders { getHex   :: String
                 , getCount :: Count
                 , getMax   :: Int }
  deriving (Show, Eq)

instance FromResponse Res where
  parseResult "blockchain.estimatefee" =
    return $ withScientific "fee" $ \fee -> return $ Fee fee
  parseResult "blockchain.block.headers" =
    return $
    withObject "result" $ \res ->
      let hex = res .: "hex"
          count = res .: "count"
          max = res .: "max"
       in BlockHeaders <$> hex <*> count <*> max
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

req' :: MonadLoggerIO m => JSONRPCT m Res
req' = do
  tEM <- sendRequest $ GetBlockHeaders 0 1 0
  $(logDebug) "sending get block headers request"
  return $ handleResponse tEM

reqBatch :: MonadLoggerIO m => [Req] -> JSONRPCT m [Res]
reqBatch reqs = do
  $(logDebug) "sending reqs"
  tEMs <- sendBatchRequest reqs
  return $ map handleResponse tEMs

getBlockHeaderReqs :: Int -> Int -> Int -> [Req]
getBlockHeaderReqs maxPerGroup start end =
  let rest =
        if start + maxPerGroup <= end
          then getBlockHeaderReqs maxPerGroup (start + maxPerGroup) end
          else []
   in GetBlockHeaders start maxPerGroup 0 : rest

main :: IO ()
main =
  runStderrLoggingT $
  jsonrpcTCPClient V2 True (clientSettings 50001 "electrum-server.ninja") $
  reqBatch (getBlockHeaderReqs 5 0 20) >>= \xs ->
    $(logDebug) $ T.pack $ "response: " ++ intercalate "\n" (map getHex xs)
