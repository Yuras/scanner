{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module GHC.Base
( unsafeChr
) where

#ifdef __MHS__
import Data.Char
unsafeChr :: Int -> Char
unsafeChr = chr
#else
import "base" GHC.Base
#endif
