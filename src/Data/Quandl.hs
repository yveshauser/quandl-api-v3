{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Data.Quandl
  where

import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Proxy
import qualified Data.Vector as V
import           Data.Time.Calendar
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client

-- | TimeSeriesAPI for quandl, see documentation at https://www.quandl.com/tools/api
type TimeSeriesApi =
     Capture "database_code" String
  :> Capture "dataset_code" String
  :> QueryParam "limit" Int
  :> QueryParam "column_index" Int
  :> QueryParam "start_date" Day
  :> QueryParam "end_date" Day
  :> QueryParam "order" Order
  :> QueryParam "collapse" Collapse
  :> QueryParam "transform" Transform
  :> QueryParam "api_key" String
  :> Get '[JSON] QuandlResponse

-- | The quandl base URL
quandlUrl :: BaseUrl
quandlUrl = BaseUrl Https "quandl.com" 443 "/api/v3/datasets"

-- | Servant quandl API
quandlApi :: Proxy TimeSeriesApi
quandlApi = Proxy

-- | Load data from quandl
quandlLoad :: Parameter -> IO (Either ClientError QuandlResponse)
quandlLoad p = flip mkClientEnv quandlUrl <$> newManager tlsManagerSettings
  >>= quandlClient p

-- | Load a multiple data from quandl
quandlLoads :: [Parameter] -> IO (Either ClientError [QuandlResponse])
quandlLoads p = flip mkClientEnv quandlUrl <$> newManager tlsManagerSettings
  >>= \e -> sequence <$> mapConcurrently (flip quandlClient e) p

-- | Client
quandlClient :: Parameter -> ClientEnv -> IO (Either ClientError QuandlResponse)
quandlClient Parameter{..} =
  runClientM (client quandlApi
    database_code
    dataset_code
    limit
    column_index
    start_date
    end_date
    order
    collapse
    transform
    api_key)

-- Types

data Order =
    Asc
  | Desc
  deriving (Show)

data Collapse =
    Daily
  | Weekly
  | Monthly
  | Quarterly
  | Annual
  deriving (Show)

data Transform =
    Diff
  | Rdiff
  | Rdiff_from
  | Cumul
  | Normalize
  deriving (Show)

data Parameter = Parameter {
    database_code :: String
  , dataset_code  :: String
  , limit         :: Maybe Int
  , column_index  :: Maybe Int
  , start_date    :: Maybe Day
  , end_date      :: Maybe Day
  , order         :: Maybe Order
  , collapse      :: Maybe Collapse
  , transform     :: Maybe Transform
  , api_key       :: Maybe String
  }

data QuandlResponse = QuandlResponse DataSet
  deriving (Show)

data Data =
    Data1 [(Day, Double)]
  | Data2 [(Day, Double, Double)]
  | Data3 [(Day, Double, Double, Double)]
  | Data4 [(Day, Double, Double, Double, Double)]
  | Data5 [(Day, Double, Double, Double, Double, Double)]
  | Data6 [(Day, Double, Double, Double, Double, Double, Double)]
  | Data7 [(Day, Double, Double, Double, Double, Double, Double, Double)]
  | Data8 [(Day, Double, Double, Double, Double, Double, Double, Double, Double)]
  | Data9 [(Day, Double, Double, Double, Double, Double, Double, Double, Double, Double)]
  deriving (Show)

data DataSet = DataSet {
    name                  :: String
  , database_code         :: String
  , database_id           :: Int
  , description           :: String
  , dataset_code          :: String
  , column_names          :: [String]
  , dataset_data          :: Data
  , newest_available_date :: Maybe Day
  , oldest_available_date :: Maybe Day
  , limit                 :: Maybe Int
  , column_index          :: Maybe Int
  , start_date            :: Maybe Day
  , end_date              :: Maybe Day
  , order                 :: Maybe Order
  , collapse              :: Maybe Collapse
  , transform             :: Maybe Transform
} deriving (Show)

-- ToHttpApiData instances

instance ToHttpApiData Order where
  toQueryParam Asc  = "asc"
  toQueryParam Desc = "desc"

instance ToHttpApiData Collapse where
  toQueryParam Daily     = "daily"
  toQueryParam Weekly    = "weekly"
  toQueryParam Monthly   = "monthly"
  toQueryParam Quarterly = "quarterly"
  toQueryParam Annual    = "annual"

instance ToHttpApiData Transform where
  toQueryParam Diff       = "diff"
  toQueryParam Rdiff      = "rdiff"
  toQueryParam Rdiff_from = "rdiff_from"
  toQueryParam Cumul      = "cumul"
  toQueryParam Normalize  = "normalize"

-- JSON instances

instance FromJSON Order where
  parseJSON (String "asc")  = pure Asc
  parseJSON (String "desc") = pure Desc
  parseJSON _               = fail "unexpected value"

instance FromJSON Collapse where
  parseJSON (String "daily")     = pure Daily
  parseJSON (String "weekly")    = pure Weekly
  parseJSON (String "monthly")   = pure Monthly
  parseJSON (String "quarterly") = pure Quarterly
  parseJSON (String "annual")    = pure Annual
  parseJSON _                    = fail "unexpected value"

instance FromJSON Transform where
  parseJSON (String "diff")       = pure Diff
  parseJSON (String "rdiff")      = pure Rdiff
  parseJSON (String "rdiff_from") = pure Rdiff_from
  parseJSON (String "cumul")      = pure Cumul
  parseJSON (String "normalize")  = pure Normalize
  parseJSON _                     = fail "unexpected value"

instance FromJSON QuandlResponse where
  parseJSON (Object v) =
    QuandlResponse <$> v .: "dataset"
  parseJSON _          = fail "expected object"

instance FromJSON DataSet where
  parseJSON (Object v) = do
      name                  <- v .: "name"
      database_code         <- v .: "database_code"
      database_id           <- v .: "database_id"
      description           <- v .: "description"
      dataset_code          <- v .: "dataset_code"
      column_names          <- v .: "column_names"
      newest_available_date <- v .: "newest_available_date"
      oldest_available_date <- v .: "oldest_available_date"
      limit                 <- v .: "limit"
      column_index          <- v .: "column_index"
      start_date            <- v .: "start_date"
      end_date              <- v .: "end_date"
      order                 <- v .: "order"
      collapse              <- v .: "collapse"
      transform             <- v .: "transform"

      let l              = length column_names
      let (Just dataset) = HM.lookup "data" v

      data_ <- parseArray l dataset

      return $ DataSet
                  name
                  database_code
                  database_id
                  description
                  dataset_code
                  column_names
                  data_
                  newest_available_date
                  oldest_available_date
                  limit
                  column_index
                  start_date
                  end_date
                  order
                  collapse
                  transform

  parseJSON _ = fail "expected object"

parseArray :: Int -> Value -> Parser Data
parseArray 2  (Array a) = Data1 <$> parseArgs a
parseArray 3  (Array a) = Data2 <$> parseArgs a
parseArray 4  (Array a) = Data3 <$> parseArgs a
parseArray 5  (Array a) = Data4 <$> parseArgs a
parseArray 6  (Array a) = Data5 <$> parseArgs a
parseArray 7  (Array a) = Data6 <$> parseArgs a
parseArray 8  (Array a) = Data7 <$> parseArgs a
parseArray 9  (Array a) = Data8 <$> parseArgs a
parseArray 10 (Array a) = Data9 <$> parseArgs a
parseArray _ _          = fail "expected array"

parseArgs :: FromJSON a => V.Vector Value -> Parser [a]
parseArgs = mapM parseJSON . V.toList
