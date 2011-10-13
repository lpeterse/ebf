{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -XFlexibleContexts -XOverloadedStrings #-}
module Data.Ebf where
                 
import Blaze.ByteString.Builder
import Data.Iteratee
import qualified Data.Iteratee.ListLike as I
import Data.ListLike
import Data.Word
import Data.ByteString

import Control.Applicative

import Data.Ebf.Ebf
import Data.Ebf.Derive

{--

main = (print :: (Maybe Bool) -> IO ()) =<< run (idone 0 (Chunk (pack [2,1,2])) >> decode) 

instance Ebf Bool where
  encode False = fromWrite $ writeWord8 0
  encode True  = fromWrite $ writeWord8 1
  decode       = I.head >>= \x-> case x of
                  0 -> return False 
                  1 -> return True
                  x -> fail $ ""

$(deriveEbf ''Bool)
$(deriveEbf ''Maybe)
 
--}
