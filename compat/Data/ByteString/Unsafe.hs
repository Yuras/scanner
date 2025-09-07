{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Unsafe
( unsafeDrop
) where

#ifdef __MHS__
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
unsafeDrop :: Int -> ByteString -> ByteString
unsafeDrop = ByteString.drop
#else
import "bytestring" Data.ByteString.Unsafe
#endif
