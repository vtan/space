module App.HashedText where

import App.Prelude

import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.String (IsString, fromString)

data HashedText = HashedText Text Int

new :: Text -> HashedText
new text = HashedText text (hash text)

toText :: HashedText -> Text
toText (HashedText text _) = text

instance Show HashedText where
  show (HashedText text _) = show text

instance Eq HashedText where
  (HashedText text1 hash1) == (HashedText text2 hash2) =
    hash1 == hash2 && text1 == text2

  (HashedText text1 hash1) /= (HashedText text2 hash2) =
    hash1 /= hash2 || text1 /= text2

instance Hashable HashedText where
  hashWithSalt salt (HashedText text _) =
    hashWithSalt salt text

  hash (HashedText _ preHash) = preHash

instance IsString HashedText where
  fromString = fromString >>> new
