module Game.Panacea.Strings ( StringRecord(..) ) where

import Control.Monad ( mzero )
import Data.Csv ( (.!) )
import qualified Data.Csv as CSV
import Data.Text ( Text )

data StringRecord = StringRecord {
      stringId   :: Int
    , stringText :: Text
} deriving (Show)

instance CSV.FromRecord StringRecord where
    parseRecord :: CSV.Record -> CSV.Parser StringRecord
    parseRecord v
        | length v == 2 = StringRecord <$> v .! 0 <*> v .! 1
        | otherwise     = mzero

instance CSV.ToRecord StringRecord where
    toRecord :: StringRecord -> CSV.Record
    toRecord (StringRecord sId sText) = CSV.record [ CSV.toField sId, CSV.toField sText ]