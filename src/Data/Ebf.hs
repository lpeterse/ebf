{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS -XFlexibleContexts -XOverloadedStrings #-}
module Data.Ebf
         ( Data.Ebf.Derive.deriveEbf
         , Data.Ebf.Derive.deriveTypehash
         , Ebf (..)
         , Typehash (..)
         , typehashCRC32
         , readEbfV1
         , writeEbfV1
         ) where
                 
import System.FilePath
import System.IO

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
import Data.Digest.CRC32

import Control.Monad
import Control.Applicative
import Control.DeepSeq

import Data.Ebf.Ebf
import Data.Ebf.Derive

ebfV1Signature :: BS.ByteString
ebfV1Signature  = BS.pack [0x1b, 0x5b, 0x31, 0x3b,
                           0x33, 0x35, 0x6d, 0x45,
                           0x42, 0x46, 0x30, 0x31,
                           0x0d, 0x0a, 0x1a, 0x0a]

typehashCRC32 :: (Typehash a) => a -> Word32
typehashCRC32  = const 0 -- crc32 . toLazyByteString . fingerprint

writeEbfV1    :: (Typehash a, Ebf a) => FilePath -> a -> IO ()
writeEbfV1 p a = do file  <- openFile p WriteMode
                    toByteStringIO 
                      (BS.hPutStr file)
                      (fromByteString ebfV1Signature `mappend` encode (typehashCRC32 a) `mappend` encode a)
                    hClose file

readEbfV1     :: forall a . (Typehash a, Ebf a) => FilePath -> IO a
readEbfV1 p    = fileDriverVBuf 65536 (iter :: Iteratee BS.ByteString IO a) p
               where
                 iter = do sig <- joinI $ I.take 16 stream2stream
                           if sig /= ebfV1Signature
                             then fail "This is not an Ebf version 1 file"
                             else do fp <- decode
                                     if typehashCRC32 (undefined :: a) /=  fp
                                       then fail "typestructure mismatch"
                                       else decode



{--

instance (Typehash a) => Typehash (Maybe a) where
  fingerprint x = fromWord64be 2468236482 `mappend` -- seed
                  fromWord16be 0          `mappend` -- first constructor  (0 fields)
                  fromWord16be 1          `mappend` -- second constructor (1 field)
                    fingerprint a
    where
      Just a = undefined `asTypeOf` x

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

instance Typehash BS.ByteString where
  fingerprint _ = fromWord64be 7283497247

instance Typehash T.Text        where
  fingerprint _ = fromWord64be 7476483117

instance Typehash Int           where
  fingerprint _ = fromWord64be 7023639375

instance Typehash Word          where
  fingerprint _ = fromWord64be 3050242152

instance Typehash Word16        where
  fingerprint _ = fromWord64be 7202776180

instance Typehash Word32        where
  fingerprint _ = fromWord64be 1544731753 

instance Typehash Word64        where
  fingerprint _ = fromWord64be 1144710436

$(deriveEbf ''Bool)
$(deriveEbf ''Maybe)
$(deriveEbf ''[])

$(deriveTypehash ''Bool  73945397534)
$(deriveTypehash ''Maybe 24349741019)
$(deriveTypehash ''[]    83558151499)

