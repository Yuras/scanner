{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified Scanner

import qualified Redis.Reply as Redis
import qualified Redis.Atto
import qualified Redis.Scanner

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Serialize.Get as Cereal

import Criterion
import Criterion.Main

main :: IO ()
main = do
  let smallStringInput = "+OK\r\n"
      longStringInput = "+11111111111111111111111111122222222222222222222233333333333333333333333444444444444444444445555555555555555555555666666666666666666677777777777777777777888888888888888888888999999999999999999000000000000000000\r\n"
      intInput = ":123\r\n"
      bulkInput = "$10\r\n0123456789\r\n"
      multiInput = "*3\r\n+A\r\n+B\r\n+C\r\n"
      binaryInput = ByteString.pack [5, 65, 66, 67, 68, 69]
  print (redisByteStringReply smallStringInput)
  print (redisAttoReply smallStringInput)
  print (redisScannerReply smallStringInput)
  print (redisAttoReply intInput)
  print (redisScannerReply intInput)
  print (redisAttoReply bulkInput)
  print (redisScannerReply bulkInput)
  print (redisAttoReply multiInput)
  print (redisScannerReply multiInput)
  defaultMain
    [ bgroup "small string"
      [ bench "Atto" $ whnf redisAttoReply smallStringInput
      , bench "Scanner" $ whnf redisScannerReply smallStringInput
      , bench "ByteString" $ whnf redisByteStringReply smallStringInput
      ]

    , bgroup "long string"
      [ bench "Atto" $ whnf redisAttoReply longStringInput
      , bench "Scanner" $ whnf redisScannerReply longStringInput
      , bench "ByteString" $ whnf redisByteStringReply longStringInput
      ]

    , bgroup "integer"
      [ bench "Atto" $ whnf redisAttoReply intInput
      , bench "Scanner" $ whnf redisScannerReply intInput
      ]

    , bgroup "bulk"
      [ bench "Atto" $ whnf redisAttoReply bulkInput
      , bench "Scanner" $ whnf redisScannerReply bulkInput
      ]

    , bgroup "multi"
      [ bench "Atto" $ whnf redisAttoReply multiInput
      , bench "Scanner" $ whnf redisScannerReply multiInput
      ]

    , bgroup "cereal"
      [ bench "Scanner" $ whnf binaryScanner binaryInput
      , bench "Cereal" $ whnf binaryCereal binaryInput
      ]
    ]

{-# NOINLINE redisAttoReply #-}
redisAttoReply :: ByteString -> Either String Redis.Reply
redisAttoReply bs = case Atto.parse Redis.Atto.reply bs of
  Atto.Done _ r -> Right r
  Atto.Fail _ _ err -> Left err
  Atto.Partial _ -> Left "Not enough input"

{-# NOINLINE redisScannerReply #-}
redisScannerReply :: ByteString -> Either String Redis.Reply
redisScannerReply bs = case Scanner.scan Redis.Scanner.reply bs of
  Scanner.Done _ r -> Right r
  Scanner.Fail _ err -> Left err
  Scanner.More _ -> Left "Not enought input"

{-# NOINLINE redisByteStringReply #-}
redisByteStringReply :: ByteString -> Either String Redis.Reply
redisByteStringReply bs = case ByteString.uncons bs of
  Just (c, bs') -> case c of
    43 -> let (l, r) = ByteString.span (/= 13) bs'
          in case ByteString.uncons r of
            Just (c', bs'') -> case c' of
              13 -> case ByteString.uncons bs'' of
                Just (c'', _) -> case c'' of
                  10 -> Right (Redis.String l)
                  _ -> Left "Unexpected input"
                Nothing -> Left "Not enough input"
              _ -> Left "Unexpected input"
            Nothing -> Left "Not enought input"
    _ -> Left "Unknown type"
  Nothing -> Left "Not enought input"

binaryScanner :: ByteString -> Either String ByteString
binaryScanner bs = case Scanner.scan p bs of
  Scanner.Done _ r -> Right r
  Scanner.Fail _ err -> Left err
  Scanner.More _ -> Left "Not enought input"
  where
  p = do
    n <- fromIntegral <$> Scanner.anyWord8
    Scanner.take n

binaryCereal :: ByteString -> Either String ByteString
binaryCereal bs = Cereal.runGet g bs
  where
  g = do
    n <- fromIntegral <$> Cereal.getWord8
    Cereal.getByteString n
