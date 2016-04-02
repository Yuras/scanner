{-# LANGUAGE OverloadedStrings #-}

module Redis.Scanner
( reply
)
where

import Scanner (Scanner)
import qualified Scanner

import Redis.Reply

import Prelude hiding (error)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import Control.Monad

{-# INLINE reply #-}
reply :: Scanner Reply
reply = do
  c <- Scanner.anyChar8
  case c of
    '+' -> string
    '-' -> error
    ':' -> integer
    '$' -> bulk
    '*' -> multi
    _ -> fail "Unknown reply type"

{-# INLINE string #-}
string :: Scanner Reply
string = String <$> line

{-# INLINE error #-}
error :: Scanner Reply
error = Error <$> line

{-# INLINE integer #-}
integer :: Scanner Reply
integer = Integer <$> integral

{-# INLINE bulk #-}
bulk :: Scanner Reply
bulk = Bulk <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> Scanner.take len <* eol

-- don't inline it to break the circle between reply and multi
{-# NOINLINE multi #-}
multi :: Scanner Reply
multi = Multi <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> replicateM len reply

{-# INLINE integral #-}
integral :: Integral i => Scanner i
integral = do
  str <- line
  case Text.signed Text.decimal (Text.decodeUtf8 str) of
    Left err -> fail (show err)
    Right (l, _) -> return l

{-# INLINE line #-}
line :: Scanner ByteString
line = Scanner.takeWhileChar8 (/= '\r') <* eol

{-# INLINE eol #-}
eol :: Scanner ()
eol = do
  Scanner.char8 '\r'
  Scanner.char8 '\n'
