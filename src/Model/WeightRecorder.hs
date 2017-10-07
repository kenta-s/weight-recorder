{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Model.WeightRecorder
    ( NewWRecord(NewWRecord, nwrUserId, nwrTime, nwrWeight)
    , nwrUserId'
    , nwrTime'
    , nwrWeight'
    , insertNewWRecord
    , selectWRecord
    )
  where

import Control.Exception (catch)
import qualified Data.Time.LocalTime as TM
import Crypto.BCrypt
    ( hashPasswordUsingPolicy
    , slowerBcryptHashingPolicy
    , validatePassword
    )
import qualified Data.ByteString as BS
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.HDBC
    ( IConnection
    , SqlError
    , withTransaction
    )
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import qualified Database.HDBC.Record as DHR
import qualified Database.Relational.Query as HRR
import qualified Entity.WeightRecord as WRecord
import System.IO (hPrint, stderr)

data NewWRecord = NewWRecord
    { nwrUserId :: !Int
    , nwrTime   :: !TM.LocalTime
    , nwrWeight :: !Double
    }

makeRecordPersistableDefault ''NewWRecord

insertNewWRecord :: IConnection c => NewWRecord -> c -> IO Integer
insertNewWRecord wr conn = do
    let ins = HRR.typedInsert WRecord.tableOfWeightRecord piNewRecord
    withTransaction conn $ 
      \conn' ->
          DHR.runInsert conn' ins wr `catch`
          \e -> do hPrint stderr (e :: SqlError)
                   return 0
              
piNewRecord :: HRR.Pi WRecord.WeightRecord NewWRecord
piNewRecord =
    NewWRecord HRR.|$| WRecord.userId' HRR.|*| WRecord.time' HRR.|*|
    WRecord.weight'

selectWRecord :: IConnection c => Int -> c -> IO [WRecord.WeightRecord]
selectWRecord uid conn = DHR.runQuery conn q uid
    where
        q :: HRR.Query Int WRecord.WeightRecord
        q =
            HRR.relationalQuery . HRR.relation' . HRR.placeholder $
            \ph ->
                do a <- HRR.query WRecord.weightRecord
                   HRR.wheres $ a HRR.! WRecord.userId' HRR..=. ph
                   HRR.desc $ a HRR.! WRecord.time'
                   return a
