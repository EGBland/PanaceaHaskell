{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Game.Panacea.Strings.Get ( StringRecord(..), getStrings ) where

import Control.Monad ( replicateM )
import Data.Binary.Get ( Get, getByteString, getWord8, getWord32le )
import Data.Bits ( (.&.), shiftL )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf16LE )
import Game.Panacea.Strings ( StringRecord(..) )

getStrings :: Get [StringRecord]
getStrings = do
    numStrings <- fromEnum <$> getWord32le
    replicateM numStrings getString

getString :: Get StringRecord
getString = do
    stringId  <- getStringId
    theString <- getString'
    return $ StringRecord stringId theString

getStringId :: Get Int
getStringId = fromEnum <$> getWord32le

getString' :: Get Text
getString' = decodeUtf16LE <$> (getStringLen >>= getByteString . (*2))

getStringLen :: Get Int
getStringLen = do
    byte1 <- fromIntegral <$> getWord8 :: Get Int
    case byte1 .&. 128 of
        0   -> return byte1
        128 -> do
            byte2 <- fromIntegral <$> getWord8 :: Get Int
            return $ (byte1 .&. 127) + (byte2 `shiftL` 7)