{-# LANGUAGE RankNTypes, BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Scanner implementation

module Scanner.Internal
where

import Prelude hiding (take, takeWhile)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString (unsafeDrop)
import qualified Scanner.OctetPredicates as OctetPredicates
import Control.Monad

-- | CPS scanner without backtracking
newtype Scanner a = Scanner
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

  {-# INLINE (*>) #-}
  (*>) = (>>)

  {-# INLINE (<*) #-}
  s1 <* s2 = s1 >>= \a -> s2 >> return a

instance Monad Scanner where
  {-# INLINE return #-}
  return a = Scanner $ \bs next ->
    next bs a

  {-# INLINE (>>=) #-}
  s1 >>= s2 = Scanner $ \bs next ->
    run s1 bs $ \bs' a ->
      run (s2 a) bs' next

  {-# INLINE  fail #-}
  fail err = Scanner $ \bs _ ->
    Fail bs err

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
--
-- Warning: it is not optimized yet, so for for small string it is better
-- to consume it byte-by-byte using `Scanner.word8`
{-# INLINE string #-}
string :: ByteString -> Scanner ()
string str = Scanner $ \bs next ->
  let strL = ByteString.length str
  in if ByteString.isPrefixOf str bs
    then next (ByteString.unsafeDrop strL bs) ()
    else run slowPath bs next
  where
  slowPath = do
    bs <- take (ByteString.length str)
    if bs == str
      then return ()
      else fail "Unexpected input"

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

{-| Fold over the octets, which satisfy the predicate -}
{-# INLINE foldlWhile #-}
foldlWhile :: (Word8 -> Bool) -> (a -> Word8 -> a) -> a -> Scanner a
foldlWhile p step init = Scanner $ \ bs next -> let
  (l, r) = ByteString.span p bs
  state = ByteString.foldl' step init l
  in if ByteString.null r
    then More $ \ bs -> if ByteString.null bs
      then next ByteString.empty state
      else run (loop state) bs next
    else next r state
  where
    loop state = do
      chunk <- takeChunk state
      if ByteString.null chunk
        then return state
        else do
          done <- endOfInput
          if done
            then return state
            else loop (ByteString.foldl' step state chunk)
    takeChunk state = Scanner $ \ bs next ->
      let (l, r) = ByteString.span p bs
      in next r l

{-| Fold over the octets, which satisfy the predicate, ensuring that there's at least one -}
{-# INLINE foldlWhile1 #-}
foldlWhile1 :: (Word8 -> Bool) -> (a -> Word8 -> a) -> a -> Scanner a
foldlWhile1 predicate step init = do
  head <- satisfy predicate
  foldlWhile predicate step (step init head)

{-| Consume a single octet which satisfies the predicate and fail if it does not -}
{-# INLINE satisfy #-}
satisfy :: (Word8 -> Bool) -> Scanner Word8
satisfy predicate = Scanner $ \ chunk next -> case ByteString.uncons chunk of
  Just (word8, remainder) -> handleHeadAndTail word8 remainder next chunk
  Nothing -> More $ \ chunk -> case ByteString.uncons chunk of
    Just (word8, remainder) -> handleHeadAndTail word8 remainder next chunk
    Nothing -> Fail chunk "No more input"
  where
    handleHeadAndTail :: Word8 -> ByteString -> (ByteString -> Word8 -> Result r) -> ByteString -> Result r
    handleHeadAndTail word8 remainder next chunk = if predicate word8
      then if ByteString.null remainder
        then More $ \ chunk -> next chunk word8
        else next remainder word8
      else Fail chunk "Octet doesn't satisfy the predicate"

{-| Consume a single octet in case it satisfies the predicate -}
{-# INLINE satisfyMaybe #-}
satisfyMaybe :: (Word8 -> Bool) -> Scanner (Maybe Word8)
satisfyMaybe predicate = Scanner $ \ chunk next -> case ByteString.uncons chunk of
  Just (word8, remainder) -> handleHeadAndTail word8 remainder next chunk
  Nothing -> More $ \ chunk -> case ByteString.uncons chunk of
    Just (word8, remainder) -> handleHeadAndTail word8 remainder next chunk
    Nothing -> next ByteString.empty Nothing
  where
    handleHeadAndTail :: Word8 -> ByteString -> (ByteString -> Maybe Word8 -> Result r) -> ByteString -> Result r
    handleHeadAndTail word8 remainder next chunk = if predicate word8
      then if ByteString.null remainder
        then More $ \ chunk -> next chunk (Just word8)
        else next remainder (Just word8)
      else next chunk Nothing

{-| Parse a non-negative decimal number in ASCII -}
{-# INLINE decimal #-}
decimal :: Integral n => Scanner n
decimal = foldlWhile1 OctetPredicates.isDigit step 0 where
  step a w = a * 10 + fromIntegral (w - 48)
