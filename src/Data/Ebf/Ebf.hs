{-# OPTIONS -XFlexibleContexts #-}
module Data.Ebf.Ebf where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Word
import Data.Monoid
import Data.Iteratee
import qualified Data.Iteratee.ListLike as I
import Data.ListLike
import Data.Word
import Data.ByteString as BS
import Control.DeepSeq

class (NFData a) => Ebf a where
  encode :: a -> Builder
  decode :: Monad m => Iteratee BS.ByteString m a

class Typehash a where
  typehashCRC32 :: a -> Word32

