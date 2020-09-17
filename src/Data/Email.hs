{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Email
  ( Email(..)
  , _Email
  , _EmailAddress
  , toEmail
  , _EmailEmailAddress
  ) where

import Control.Lens hiding (elements)
import Control.Monad
import Data.Aeson as JSON
import Data.ByteString
import Data.ByteString.Lens
import Data.String
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lens
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (elements)
import Text.Email.Parser
import Text.Email.Validate (EmailAddress, emailAddress)

newtype Email
  = Email
  { unEmail :: Text
  } deriving (Eq, Show, FromJSON, ToJSON, IsString)

makePrisms ''Email

-- | "Orphaned" prism.
_EmailAddress :: Prism' ByteString EmailAddress
_EmailAddress = prism' toByteString emailAddress

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

instance ToJSON EmailAddress where
  toJSON = toJSON . view (re _EmailEmailAddress . _Email)

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ \s -> do
    let ea = s ^? re _Email . _EmailEmailAddress
    case ea of
      Just x  -> pure x
      Nothing -> mzero

toEmail :: EmailAddress -> Email
toEmail = Email . decodeUtf8 . toByteString
