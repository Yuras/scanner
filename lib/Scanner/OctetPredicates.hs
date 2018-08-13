module Scanner.OctetPredicates
where

import Prelude
import Data.Word


isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
