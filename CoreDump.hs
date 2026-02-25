module CoreDump where

import qualified Data.PVector.Back as P

benchFoldl :: P.Vector Int -> Int
benchFoldl = P.foldl' (+) (0 :: Int)

benchMap :: P.Vector Int -> P.Vector Int
benchMap = P.map (+1)

benchFoldr :: P.Vector Int -> [Int]
benchFoldr = P.toList
