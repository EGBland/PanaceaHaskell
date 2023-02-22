module Game.Panacea.Strings.Put ( putStrings ) where

import Data.Binary.Put ( Put, putWord8, putWord32le, putByteString )
import Data.Bits ( (.&.), (.|.), complement, shiftR )
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf16LE )
import Data.Word ( Word8, Word16 )
import Game.Panacea.Strings ( StringRecord(..) )

putStrings :: [StringRecord] -> Put
putStrings records = do
    putNumStrings records
    mapM_ putString records


putNumStrings :: [StringRecord] -> Put
putNumStrings = putWord32le . fromIntegral . length

putString :: StringRecord -> Put
putString (StringRecord strId strText) = do
    putStringId strId
    putStringLen strText
    putText strText

putStringId :: Int -> Put
putStringId = putWord32le . fromIntegral

putStringLen :: T.Text -> Put
putStringLen text
    | n < 128   = putOneByteLen n
    | otherwise = putTwoByteLen n
    where
        n = T.length text

putOneByteLen :: Int -> Put
putOneByteLen = putWord8 . fromIntegral

putTwoByteLen :: Int -> Put
putTwoByteLen n =
    let
        n16 = fromIntegral n :: Word16
        part1 = (n16 .&. 127) .|. (if n16 > 127 then 128 else 0) :: Word16
        part2 = (n16 .&. (complement 127 :: Word16)) `shiftR` 7 :: Word16
        byte1 = fromIntegral . fromEnum $ part1 .&. 255 :: Word8
        byte2 = fromIntegral . fromEnum $ part2 .&. 127 :: Word8
    in putWord8 byte1 >> putWord8 byte2

putText :: T.Text -> Put
putText = putByteString . encodeUtf16LE