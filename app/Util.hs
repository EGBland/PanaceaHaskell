module Util (
    split,
    filesizeStr
)
where

import Text.Printf

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split c s
    | head s == c = split c $ tail s
    | otherwise = [sBefore]++(split c sNext)
    where sBefore = takeWhile (/=c) s
          sNext = drop (length sBefore) s

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