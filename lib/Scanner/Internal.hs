{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Scanner implementation

module Scanner.Internal
where

import Prelude hiding (takeWhile)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Control.Monad

-- | CPS scanner without backtracking
data Scanner a = Scanner
  { run :: forall r. ByteString -> Next a r -> Result r
  }

-- | Scanner continuation
type Next a r = ByteString -> a -> Result r

-- | Scanner result
data Result r
  -- | Successful result with the rest of input
  = Done ByteString r

  -- | Scanner failed with rest of input and error message
  | Fail ByteString String

  -- | Need more input
  | More (ByteString -> Result r)

-- | Run scanner with the input
scan :: Scanner r -> ByteString -> Result r
scan s bs = run s bs Done

instance Functor Scanner where
  {-# INLINE fmap #-}
  fmap f (Scanner s) = Scanner $ \bs next ->
    s bs $ \bs' a ->
      next bs' (f a)

instance Applicative Scanner where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Scanner where
  {-# INLINE return #-}
  return a = Scanner $ \bs next ->
    next bs a

  {-# INLINE (>>=) #-}
  s1 >>= s2 = Scanner $ \bs next ->
    run s1 bs $ \bs' a ->
      run (s2 a) bs' next

-- | Consume the next word
--
-- It fails if end of input
{-# INLINE anyWord8 #-}
anyWord8 :: Scanner Word8
anyWord8 = Scanner $ \bs next ->
  case ByteString.uncons bs of
    Just (c, bs') -> next bs' c
    _ -> More $ \bs' -> slowPath bs' next
  where
  slowPath bs next =
    case ByteString.uncons bs of
      Just (c, bs') -> next bs' c
      _ -> Fail ByteString.empty "No more input"

-- | Take input while the predicate is `True`
{-# INLINE takeWhile #-}
takeWhile :: (Word8 -> Bool) -> Scanner ByteString
takeWhile p = Scanner $ \bs next ->
  let (l, r) = ByteString.span p bs
  in if ByteString.null r
    then More $ \bs' ->
      if ByteString.null bs'
        then next ByteString.empty l
        else run (slowPath l) bs' next
    else next r l
  where
  slowPath l = go [l]
  go res = do
    chunk <- takeChunk
    done <- endOfInput
    if done || ByteString.null chunk
      then return . ByteString.concat . reverse $ (chunk : res)
      else go (chunk : res)
  takeChunk = Scanner $ \bs next ->
    let (l, r) = ByteString.span p bs
    in next r l

-- | Take the specified number of bytes
{-# INLINE take #-}
take :: Int -> Scanner ByteString
take n = Scanner $ \bs next ->
  let len = ByteString.length bs
  in if len >= n
    then let (l, r) = ByteString.splitAt n bs
         in next r l
    else More $ \bs' ->
      if ByteString.null bs'
        then Fail ByteString.empty "No more input"
        else run (slowPath bs len) bs' next
  where
  slowPath bs len = go [bs] (n - len)
  go res 0 = return . ByteString.concat . reverse $ res
  go res i = Scanner $ \bs next ->
    let len = ByteString.length bs
    in if len >= i
      then let (l, r) = ByteString.splitAt i bs
           in next r (ByteString.concat . reverse $ (l : res))
      else More $ \bs' ->
        if ByteString.null bs'
          then Fail ByteString.empty "No more input"
          else run (go (bs : res) (i - len)) bs' next

-- | Returns `True` when there is no more input
{-# INLINE endOfInput #-}
endOfInput :: Scanner Bool
endOfInput = Scanner $ \bs next ->
  if ByteString.null bs
    then More $ \bs' -> next bs' (ByteString.null bs')
    else next bs False

-- | Consume the specified string
{-# INLINE string #-}
string :: ByteString -> Scanner ()
string str = Scanner $ \bs next ->
  let bsL = ByteString.length bs
      strL = ByteString.length str
  in if ByteString.isPrefixOf str bs
    then next (ByteString.drop strL bs) ()
    else if ByteString.isPrefixOf bs str
      then More $ \bs' -> run (string (ByteString.drop bsL str)) bs' next
      else Fail bs "unexpected input"

-- | Return the next byte, if any, without consuming it
{-# INLINE lookAhead #-}
lookAhead :: Scanner (Maybe Word8)
lookAhead = Scanner $ \bs next ->
  case ByteString.uncons bs of
    Just (c, _) -> next bs (Just c)
    _ -> More $ \bs' -> slowPath bs' next
  where
  slowPath bs next =
    case ByteString.uncons bs of
      Just (c, _) -> next bs (Just c)
      _ -> next ByteString.empty Nothing
