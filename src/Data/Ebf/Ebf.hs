{-# OPTIONS -XFlexibleContexts #-}
module Data.Ebf.Ebf where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Word
import Data.Monoid
import Data.Iteratee
import qualified Data.Iteratee.ListLike as I
import Data.ListLike
import Data.Word
import Data.ByteString

class Ebf a where
  encode :: a -> Builder
  decode :: (ListLike s Word8, Nullable s, Monad m) => Iteratee s m a

class Typehash a where
  typehashCRC32 :: a -> Word32

