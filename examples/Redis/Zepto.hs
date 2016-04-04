{-# LANGUAGE OverloadedStrings #-}

module Redis.Zepto
( reply
)
where

import Redis.Reply

import Prelude hiding (error)
import Data.ByteString (ByteString)
import Data.Attoparsec.Zepto (Parser)
import qualified Data.Attoparsec.Zepto as Zepto
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import Control.Monad

{-# INLINE reply #-}
reply :: Parser Reply
reply = do
  c <- Zepto.take 1
  case c of
    "+" -> string
    "-" -> error
    ":" -> integer
    "$" -> bulk
    "*" -> multi
    _ -> fail "Unknown reply type"

{-# INLINE string #-}
string :: Parser Reply
string = String <$> line

{-# INLINE error #-}
error :: Parser Reply
error = Error <$> line

{-# INLINE integer #-}
integer :: Parser Reply
integer = Integer <$> integral

{-# INLINE integral #-}
integral :: Integral i => Parser i
integral = do
  str <- line
  case Text.signed Text.decimal (Text.decodeUtf8 str) of
    Left err -> fail (show err)
    Right (l, _) -> return l

{-# INLINE bulk #-}
bulk :: Parser Reply
bulk = Bulk <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> Zepto.take len <* eol

-- don't inline it to break the circle between reply and multi
{-# NOINLINE multi #-}
multi :: Parser Reply
multi = Multi <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> replicateM len reply

{-# INLINE line #-}
line :: Parser ByteString
line = Zepto.takeWhile (/= 13) <* eol

{-# INLINE eol #-}
eol :: Parser ()
eol = Zepto.string "\r\n"
