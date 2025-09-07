{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Lazy_
( ByteString
, toChunks
, fromChunks
) where

#ifdef __MHS__
import qualified Data.ByteString as Strict

newtype ByteString = ByteString [Strict.ByteString]

toChunks :: ByteString -> [Strict.ByteString]
toChunks (ByteString chunks) = chunks

fromChunks :: [Strict.ByteString] -> ByteString
fromChunks = ByteString
#else
import "bytestring" Data.ByteString.Lazy
#endif
