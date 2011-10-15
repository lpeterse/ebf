{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -XFlexibleContexts -XOverloadedStrings #-}
module Data.Ebf
         ( Data.Ebf.Derive.deriveEbf
         , Ebf (..)
         ) where
                 
import Blaze.ByteString.Builder
import Data.Iteratee
import qualified Data.Iteratee.ListLike as I
import Data.ListLike
import Data.Word
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Text.Encoding as TE
import Data.Int
import Data.Word
import Data.Monoid
import Data.Foldable

import Control.Monad
import Control.Applicative
import Control.DeepSeq

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

--}

instance Ebf Int where
  encode = encode . (fromIntegral :: Int -> Word64)
  decode = decode >>= \x-> return $! (fromIntegral :: Word64 -> Int) x

instance Ebf Word where
  encode = encode . (fromIntegral :: Word -> Word64)
  decode = decode >>= \x-> return $! (fromIntegral :: Word64 -> Word) x

instance Ebf Word64 where
  encode = fromWord64be 
  decode = do a <- I.head
              b <- I.head
              c <- I.head
              d <- I.head
              e <- I.head
              f <- I.head
              g <- I.head
              h <- I.head
              let x = (fromIntegral a) * 256*256*256*256*256*256*256
                    + (fromIntegral b) * 256*256*256*256*256*256
                    + (fromIntegral c) * 256*256*256*256*256
                    + (fromIntegral d) * 256*256*256*256
                    + (fromIntegral e) * 256*256*256
                    + (fromIntegral f) * 256*256
                    + (fromIntegral g) * 256
                    + (fromIntegral h)
              x `deepseq` return x

instance Ebf Word16 where
  encode = fromWord16be
  decode = do a <- I.head
              b <- I.head
              let x = (fromIntegral a) * 256
                    + (fromIntegral b)
              x `deepseq` return x


instance Ebf Word32 where
  encode = fromWord32be
  decode = do a <- I.head
              b <- I.head
              c <- I.head
              d <- I.head
              let x = (fromIntegral a) * 256*256*256
                    + (fromIntegral b) * 256*256
                    + (fromIntegral c) * 256
                    + (fromIntegral d) 
              x `deepseq` return x

instance Ebf T.Text where
  encode = encode . TE.encodeUtf8
  decode = decode >>= \x-> return $! TE.decodeUtf8 x

instance Ebf BS.ByteString where
  encode x = fromWord32be (fromIntegral $ BS.length x) `mappend` fromByteString x
  decode   = do i  <- decode
                bs <- joinI $ I.take (fromIntegral (i :: Word32)) (I.stream2stream)
                return $! bs

{--instance (Ebf a) => Ebf [a] where
  encode []     = fromWord8 1
  encode (x:xs) = fromWord8 2 `mappend` encode x `mappend` encode xs
  decode        = do i <- I.head
                     case i of
                       0 -> fail "read 0 while decoding []"
                       1 -> return []
                       2 -> do a  <- decode
                               as <- decode
                               return $! a:as
                       _ -> fail "error while decoding []"
--}
$(deriveEbf ''Bool)
$(deriveEbf ''Maybe)
$(deriveEbf ''[])

