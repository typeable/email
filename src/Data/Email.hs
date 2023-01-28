{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Email
  ( Email(..)
  , _Email
  , _EmailAddress
  , toEmail
  , _EmailEmailAddress
  ) where

import           Control.Lens              hiding (elements)
import           Data.Aeson                as JSON
import           Data.ByteString
import           Data.ByteString.Lens
import           Data.String
import           Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Text.Lens
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen       (elements)
import           Text.Email.Extra          (validateEmail)
import           Text.Email.Parser

newtype Email
  = Email
  { unEmail :: Text
  } deriving (Eq, Show, FromJSON, ToJSON, IsString)

makePrisms ''Email

-- | "Orphaned" prism.
_EmailAddress :: Prism' ByteString EmailAddress
_EmailAddress = prism' toByteString (preview _Right . validateEmail)

-- | Can be used like this:
--
-- >>> _EmailEmailAddress # (fromJust $ emailAddress "foo@bar.com")
-- Email {unEmail = "foo@bar.com"}
--
-- >>> Email "foo@bar.com" ^? _EmailEmailAddress
-- Just "foo@bar.com"
--
_EmailEmailAddress :: Prism' Email EmailAddress
_EmailEmailAddress = prism'
  toEmail
  (^? _Email . unpacked . packedChars . _EmailAddress)

instance Arbitrary Email where
  arbitrary = do
    user <- elements ["user", "foo", "bar" :: Text]
    host <- elements ["example.com", "gmail.com" :: Text]
    return $ Email $ user <> host

toEmail :: EmailAddress -> Email
toEmail = Email . decodeUtf8 . toByteString
