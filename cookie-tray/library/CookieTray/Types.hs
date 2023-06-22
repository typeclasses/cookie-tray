module CookieTray.Types where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString)
import Data.Time (DiffTime, UTCTime)
import Prelude (Eq, Functor, Ord, Show)

newtype Name = Name {nameByteString :: BS.ByteString}
  deriving newtype (Eq, Ord, Show, IsString)

data Named a = Named {name :: Name, value :: a}
  deriving stock (Eq, Ord, Show)

newtype Value = Value {valueByteString :: BS.ByteString}
  deriving newtype (Eq, Ord, Show, IsString)

-- | The raw content of a set-cookie header
newtype BinaryCommand = BinaryCommand {binaryCommandByteStringLazy :: LBS.ByteString}
  deriving newtype (Eq, Ord, Show, IsString)

data JavascriptAccess
  = -- | HttpOnly; javascript cannot access the cookie
    HiddenFromJavascript
  | -- | Cookie will be accessible from javascript,
    -- see https://developer.mozilla.org/en-US/docs/web/api/document/cookie
    AccessibleFromJavascript
  deriving (Eq, Ord, Show)

data TransportEncryption
  = -- | The browser only sends cookies over HTTP, not HTTP
    RequireEncryptedTransport
  | -- | The browser sends cookies regardless of transport encryption
    AllowUnencryptedTransport
  deriving (Eq, Ord, Show)

data Origin
  = SameSite SameSiteOptions
  | -- | Instruct the client to send the cookie for all requests,
    -- even those originating from a third party.
    -- This option implies 'RequireEncryptedTransport'.
    CrossSite
  deriving (Eq, Ord, Show)

data SameSiteOptions = SameSiteOptions
  { sameSiteStrictness :: SameSiteStrictness,
    transportEncryption :: TransportEncryption
  }
  deriving (Eq, Ord, Show)

data SameSiteStrictness
  = -- | Instruct the client not to send the cookie for requests originating
    -- from another site, e.g. if a user clicked a link from another site to
    -- yours. This setting offers the strongest protection against cross-site
    -- request forgery, but does not make sense in a lot of cases. (If somebody
    -- follows a link to your site, should they appear to be logged out?)
    SameSiteStrict
  | -- | The client will send the cookie whenever your domain is what appears
    -- in the navigation bar
    SameSiteLax
  deriving (Eq, Ord, Show)

data Security = Security
  { jsAccess :: JavascriptAccess,
    origin :: Origin
  }
  deriving (Eq, Ord, Show)

data Secured a = Secured
  { security :: Security,
    secured :: a
  }
  deriving (Eq, Ord, Show, Functor)

data Expiry = ExpiryTime UTCTime | ExpiryAge DiffTime
  deriving (Eq, Ord, Show)

data Expiring a = Expiring {expiry :: Expiry, expiring :: a}
  deriving (Eq, Ord, Show, Functor)

-- | The host to which the cookie will be sent
data Domain = Domain BS.ByteString | CurrentHostExcludingSubdomains
  deriving (Eq, Ord, Show)

data Path = Path BS.ByteString | CurrentPath
  deriving (Eq, Ord, Show)

data Scope = Scope {domain :: Domain, path :: Path}
  deriving (Eq, Ord, Show)

data Meta = Meta {metaScope :: Scope, metaSecurity :: Security}
  deriving (Eq, Ord, Show)

data Action a = Delete | Put a
  deriving (Eq, Ord, Show, Functor)
