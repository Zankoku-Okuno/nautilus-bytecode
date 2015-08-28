module Language.Nautilus.Bytecode.FromNBC where

import Data.Int
import Data.Word
import Language.Nautilus.Bytecode.Abstract

class FromNBC a where
    fromNBC :: Data -> a

    fromNBC1 :: [Data] -> a
    fromNBC1 [x] = fromNBC x
    fromNBC1 _ = error "FromNBC1 of non-singleton list"

instance FromNBC Word8 where
    fromNBC (U8 x) = x

instance FromNBC Integer where
    fromNBC (U8 x) = fromIntegral x
    -- TODO the rest of the integer types