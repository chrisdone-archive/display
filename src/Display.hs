{-# LANGUAGE FlexibleInstances #-}

-- | Display human-readable values to the user.

module Display where

import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int64, Int32, Int16, Int8)
import           Data.Word (Word64, Word32, Word16, Word8)

class Display a where
  display :: a -> B.Builder
  -- ^ Display a value in a readable format.

instance Display S.ByteString where
  display = B.byteString

instance Display L.ByteString where
  display = B.lazyByteString

instance Display [Char] where
  display = B.stringUtf8

instance Display Char where
  display = B.charUtf8

instance Display Int8 where
  display = B.int8Dec

instance Display Int16 where
  display = B.int16Dec

instance Display Int32 where
  display = B.int32Dec

instance Display Int64 where
  display = B.int64Dec

instance Display Int where
  display = B.intDec

instance Display Integer where
  display = B.integerDec

instance Display Word8 where
  display = B.word8Dec

instance Display Word16 where
  display = B.word16Dec

instance Display Word32 where
  display = B.word32Dec

instance Display Word64 where
  display = B.word64Dec

instance Display Word where
  display = B.wordDec

instance Display Float where
  display = B.floatDec

instance Display Double where
  display = B.doubleDec
