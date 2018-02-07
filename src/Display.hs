{-# LANGUAGE FlexibleInstances #-}

-- | Display human-readable values to the user efficiently. See the
-- 'display' method for documentation.
--
-- Use the OverloadedStrings language extension when using this module
-- to produce 'Builder' values conveniently e.g. @\"Hello\" <> display name@.

module Display
  (
  -- * Printing method
  display
  -- * Conversions
  , displayByteString
  , displayLazyByteString
  , displayString
  , displayText
  , displayLazyText
  -- * The class
  , Display
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int64, Int32, Int16, Int8)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Word (Word64, Word32, Word16, Word8)

--------------------------------------------------------------------------------
-- Top-level entry points

-- | Display to a 'ByteString' value.
displayByteString :: Display a => a -> ByteString
displayByteString = L.toStrict . B.toLazyByteString . display

-- | Display to a 'ByteString' value.
displayLazyByteString :: Display a => a -> ByteString
displayLazyByteString = L.toStrict . B.toLazyByteString . display

-- | Display to a 'Text' value. Inefficient. Only use when you have to.
displayText :: Display a => a -> Text
displayText = LT.toStrict . LT.decodeUtf8 . B.toLazyByteString . display

-- | Display to a 'Text' value. Inefficient. Only use when you have to.
displayLazyText :: Display a => a -> Text
displayLazyText = LT.toStrict . LT.decodeUtf8 . B.toLazyByteString . display

-- | Display to a 'String' value. Very inefficient. Only use when you
-- are forced to by another API.
displayString :: Display a => a -> String
displayString = LT.unpack . LT.decodeUtf8 . B.toLazyByteString . display

--------------------------------------------------------------------------------
-- Class and instances

-- | Display a value in a human-readable format. This is the opposite
-- of the 'Show' class which is intended for programmers to read, and
-- can be used alongside 'Show'.
--
-- For example, consider: @Maybe String@
--
-- >>> show (Just "abc")
-- Just "abc"
-- >>> show Nothing
-- Nothing
--
-- whereas 'display' is meant for printing to users, so you might
-- write this:
--
-- >>> display (Just "abc")
-- abc
-- >>> display Nothing
-- ""
--
-- You can safely use newtype deriving with this type, e.g.
--
-- @
-- newtype Name = Name Text deriving (Show, Display)
-- @
--
-- Instances for exceptions can be written like this:
--
-- @
-- data MyException = SomeProblem deriving (Show, Typeable)
-- instance Exception MyException where displayException = displayString
-- @
--
class Display a where
  display :: a -> B.Builder
  -- ^ Display a value in a human-readable format. This is the
  -- opposite of the 'Show' class which is intended for programmers to
  -- read. See 'Display' for how to use this for your own types.
  --
  -- * Writing to file or output: The 'B.Builder' can be written
  -- directly to a handle with 'B.hPutBuilder' very efficiently as
  -- UTF-8. This is the preferred method when writing to stdout or a
  -- file.
  --
  -- * Writing to a string: Use one of the functions in Conversions
  --   like 'displayByteString' or 'displayString' (when needed).
  --
  -- Use the functions in "Data.ByteString.Builder" for hex encodings
  -- of numbers and strings.
  --
  -- To append 'Builder' you can use the 'Monoid' instance, for
  -- example to print things comma-separated, you can use
  -- 'Data.List.intersperse' and 'mconcat':
  --
  -- @
  -- mconcat (intersperse ", " (map display [1, 4, 5]))
  -- @
  --
  -- This example requires the OverloadedStrings language extension.

instance Display a => Display (Maybe a) where
  display = maybe mempty display

instance (Display a, Display b) => Display (Either a b) where
  display = either display display

instance Display Text where
  display = B.byteString . T.encodeUtf8

instance Display LT.Text where
  display = B.lazyByteString . LT.encodeUtf8

instance Display B.Builder where
  display = id

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
