module Util (
    filesizeStr
)
where

import Text.Printf

fstl :: Int -> String
fstl 0 = "B"
fstl 1 = "KB"
fstl 2 = "MB"
fstl 3 = "GB"
fstl _ = "TB"

filesizeStr :: (Enum a) => a -> String
filesizeStr x = filesizeStr' (fromIntegral . fromEnum x) 0

filesizeStr' :: Float -> Int -> String
filesizeStr' b e
    | b >= 1024 = printf "%.2f%s" b $ fstl e
    | e > 3 = printf "%.2f%s" b $ fstl e
    | otherwise = filesizeStr' (b / 1024) (e + 1)