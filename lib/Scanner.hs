
-- | Fast not-backtracking incremental scanner for bytestrings
--
-- Unlike attoparsec or most of other parser combinator libraries,
-- scanner doesn't support backtracking. But you probably don't need it
-- anyway, at least if you need fast parser.
--
-- Scanner processes input incrementally. When more input is needed, scanner
-- returns `More` continuation. All the already processed input is discarded.

module Scanner
( Scanner
, Result (..)
, scan
, scanOnly
, scanLazy
, scanWith
, anyWord8
, anyChar8
, word8
, char8
, take
, takeWhile
, takeWhileChar8
, string
, skipWhile
, skipSpace
, lookAhead
, lookAheadChar8
)
where

import Scanner.Internal

import Prelude hiding (take, takeWhile)
import Data.Word
import qualified Data.Char as Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Control.Monad
import GHC.Base (unsafeChr)

-- | Scan the complete input, without resupplying
scanOnly :: Scanner a -> ByteString -> Either String a
scanOnly s bs = go (scan s bs)
  where
  go res = case res of
    Done _ r -> Right r
    Fail _ err -> Left err
    More more -> go (more ByteString.empty)

-- | Scan lazy bytestring by resupplying scanner with chunks
scanLazy :: Scanner a -> Lazy.ByteString -> Either String a
scanLazy s lbs = go (scan s) (Lazy.ByteString.toChunks lbs)
  where
  go more chunks =
    let (chunk, chunks') = case chunks of
          [] -> (ByteString.empty, [])
          (c:cs) -> (c, cs)
    in case more chunk of
      Done _ r -> Right r
      Fail _ err -> Left err
      More more' -> go more' chunks'

-- | Scan with the provided resupply action
scanWith :: Monad m => m ByteString -> Scanner a -> ByteString -> m (Result a)
scanWith more s input = go input (scan s)
  where
  go bs next = case next bs of
    More next' -> do
      bs' <- more
      go bs' next'
    res -> return res

-- | Consume the next 8-bit char
--
-- It fails if end of input
{-# INLINE anyChar8 #-}
anyChar8 :: Scanner Char
anyChar8 = w2c <$> anyWord8

-- | Consume the specified word or fail
{-# INLINE word8 #-}
word8 :: Word8 -> Scanner ()
word8 w = do
  w' <- anyWord8
  unless (w' == w) $
    fail "unexpected word"

-- | Consume the specified 8-bit char or fail
{-# INLINE char8 #-}
char8 :: Char -> Scanner ()
char8 = word8 . c2w

-- | Take input while the predicate is `True`
{-# INLINE takeWhileChar8 #-}
takeWhileChar8 :: (Char -> Bool) -> Scanner ByteString
takeWhileChar8 p = takeWhile (p . w2c)

-- | Return the next byte, if any, without consuming it
{-# INLINE lookAheadChar8 #-}
lookAheadChar8 :: Scanner (Maybe Char)
lookAheadChar8 = fmap w2c <$> lookAhead

-- | Skip any input while the preducate is `True`
{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Scanner ()
skipWhile = void . takeWhile

-- | Skip space
{-# INLINE skipSpace #-}
skipSpace :: Scanner ()
skipSpace = skipWhile isSpaceWord8

{-# INLINE isSpaceWord8 #-}
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w = w == 32 || w <= 13

{-# INLINE w2c #-}
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral

{-# INLINE c2w #-}
c2w :: Char -> Word8
c2w = fromIntegral . Char.ord
