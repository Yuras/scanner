
module Redis.Reply
( Reply (..)
)
where

import Data.Int
import Data.ByteString (ByteString)

data Reply
  = String ByteString
  | Error ByteString
  | Integer Int64
  | Bulk (Maybe ByteString)
  | Multi (Maybe [Reply])
  deriving (Show, Eq)
