module AOC2015.Day4 (run) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Utils (Result (..))

process :: (Show a) => ByteString -> a -> ByteString
process key = encode . hash . mergedByteStrings
  where
    mergedByteStrings x = key <> toByteString x
    toByteString = pack . show

part1 :: T.Text -> Int
part1 a = until (checkPrefix . encoded) (+ 1) 1
  where
    checkPrefix = isPrefixOf (pack "00000")
    encoded = process (TE.encodeUtf8 a)

part2 :: T.Text -> Int
part2 a = until (checkPrefix . encoded) (+ 1) 1
  where
    checkPrefix = isPrefixOf (pack "000000")
    encoded = process (TE.encodeUtf8 a)

run :: T.Text -> Result
run a = Result result1 result2
  where
    result1 = part1 a
    result2 = part2 a