{-# LANGUAGE OverloadedStrings #-}

module Redis.Atto
( reply
)
where

import Redis.Reply

import Prelude hiding (error)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Atto (takeTill)
import qualified Data.Attoparsec.ByteString.Char8 as Atto hiding (takeTill)
import Control.Monad

{-# INLINE reply #-}
reply :: Parser Reply
reply = do
  c <- Atto.anyChar
  case c of
    '+' -> string
    '-' -> error
    ':' -> integer
    '$' -> bulk
    '*' -> multi
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

{-# INLINE bulk #-}
bulk :: Parser Reply
bulk = Bulk <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> Atto.take len <* eol

-- don't inline it to break the circle between reply and multi
{-# NOINLINE multi #-}
multi :: Parser Reply
multi = Multi <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> Atto.count len reply

{-# INLINE integral #-}
integral :: Integral i => Parser i
integral = Atto.signed Atto.decimal <* eol

{-# INLINE line #-}
line :: Parser ByteString
line = Atto.takeTill (== 13) <* eol

{-# INLINE eol #-}
eol :: Parser ()
eol = void $ Atto.string "\r\n"
