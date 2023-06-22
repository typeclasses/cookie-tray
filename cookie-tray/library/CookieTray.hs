module CookieTray
  ( -- * Command
    render,
    renderLBS,
    ToCommandList (..),
    Command,
    Action (..),
    renderCommand,
    BinaryCommand,
    binaryCommandByteStringLazy,

    -- * Tray
    Tray (..),
    parse,
    lookup,
    fromList,
    toList,

    -- * Name
    Name (..),
    Named (..),

    -- * Value
    Value (..),

    -- * Expiry
    Expiry (..),
    Expiring (..),

    -- * Meta

    -- ** Security
    Security (..),
    Secured (..),
    Origin (..),
    TransportEncryption (..),
    SameSiteOptions (..),
    SameSiteStrictness (..),
    JavascriptAccess (..),

    -- ** Scope
    Scope (..),
    Domain (..),
    Path (..),
    Meta (..),
  )
where

import CookieTray.Command (Command, ToCommandList (..))
import CookieTray.Command qualified as Command
import CookieTray.Types
import Data.Binary.Builder qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Functor (Functor, fmap, (<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Endo (..), Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Time.Clock.POSIX qualified as Time
import GHC.Exts (IsList, Item)
import GHC.Exts qualified as IsList (IsList (..))
import Web.Cookie qualified as Web
import Prelude (Bool (..), Eq, Maybe (..), Ord, Show, ($), (.))

---  Tray  ---

newtype Tray a = Tray (Map Name a)
  deriving (Eq, Ord, Show, Functor)

-- | Left-biased map union
instance Semigroup (Tray a) where
  Tray x <> Tray y = Tray (x <> y)

instance Monoid (Tray a) where
  mempty = Tray mempty

instance IsList (Tray a) where
  type Item (Tray a) = Named a
  fromList = fromList
  toList = toList

parse :: BS.ByteString -> Tray Value
parse =
  fromList
    . fmap (\(a, b) -> Named {name = Name a, value = Value b})
    . Web.parseCookies

toList :: Tray a -> [Named a]
toList (Tray m) =
  Map.toList m <&> \(a, b) ->
    Named {name = a, value = b}

fromList :: [Named a] -> Tray a
fromList = Tray . Map.fromList . fmap (\x -> (name x, value x))

lookup :: Name -> Tray a -> Maybe a
lookup x (Tray m) = Map.lookup x m

---  Command  ---

renderCommand :: Command -> BinaryCommand
renderCommand = renderSetCookie . applyToSetCookie

render :: (ToCommandList a) => a -> [BinaryCommand]
render = fmap renderCommand . toCommandList

renderLBS :: (ToCommandList a) => a -> [LBS.ByteString]
renderLBS = fmap binaryCommandByteStringLazy . render

---  Rendering internals  ---

renderSetCookie :: Endo Web.SetCookie -> BinaryCommand
renderSetCookie f =
  BinaryCommand $ Binary.toLazyByteString $ Web.renderSetCookie $ appEndo f Web.def

class ApplyToSetCookie a where
  applyToSetCookie :: a -> Endo Web.SetCookie

instance ApplyToSetCookie Command where
  applyToSetCookie x =
    applyToSetCookie (Command.name x)
      <> applyToSetCookie (Command.meta x)
      <> applyToSetCookie (Command.action x)

instance (ApplyToSetCookie a) => ApplyToSetCookie (Named a) where
  applyToSetCookie x =
    applyToSetCookie (name x)
      <> applyToSetCookie (value x)

instance ApplyToSetCookie Name where
  applyToSetCookie x = Endo \sc -> sc {Web.setCookieName = nameByteString x}

instance ApplyToSetCookie Value where
  applyToSetCookie x = Endo \sc -> sc {Web.setCookieValue = valueByteString x}

instance ApplyToSetCookie TransportEncryption where
  applyToSetCookie x = Endo \sc -> sc {Web.setCookieSecure = y}
    where
      y = case x of
        RequireEncryptedTransport -> True
        AllowUnencryptedTransport -> False

instance ApplyToSetCookie Security where
  applyToSetCookie x =
    applyToSetCookie (jsAccess x)
      <> applyToSetCookie (origin x)

instance ApplyToSetCookie Origin where
  applyToSetCookie = \case
    SameSite o -> applyToSetCookie o
    CrossSite -> Endo \sc ->
      sc
        { Web.setCookieSameSite = Just Web.sameSiteNone,
          Web.setCookieSecure = True -- When SameSite=None, Secure is required
        }

instance ApplyToSetCookie SameSiteOptions where
  applyToSetCookie x =
    applyToSetCookie (sameSiteStrictness x)
      <> applyToSetCookie (transportEncryption x)

instance ApplyToSetCookie SameSiteStrictness where
  applyToSetCookie x = Endo \sc ->
    sc
      { Web.setCookieSameSite = Just
          case x of
            SameSiteStrict -> Web.sameSiteStrict
            SameSiteLax -> Web.sameSiteLax
      }

instance ApplyToSetCookie JavascriptAccess where
  applyToSetCookie x = Endo \sc -> sc {Web.setCookieHttpOnly = y}
    where
      y = case x of
        HiddenFromJavascript -> True
        AccessibleFromJavascript -> False

instance ApplyToSetCookie Domain where
  applyToSetCookie x = Endo \sc -> sc {Web.setCookieDomain = y}
    where
      y = case x of
        Domain z -> Just z
        CurrentHostExcludingSubdomains -> Nothing

instance ApplyToSetCookie Expiry where
  applyToSetCookie = \case
    ExpiryTime x -> Endo \sc -> sc {Web.setCookieExpires = Just x}
    ExpiryAge x -> Endo \sc -> sc {Web.setCookieMaxAge = Just x}

instance ApplyToSetCookie Path where
  applyToSetCookie x = Endo \sc -> sc {Web.setCookiePath = y}
    where
      y = case x of
        Path z -> Just z
        CurrentPath -> Nothing

instance ApplyToSetCookie Scope where
  applyToSetCookie x =
    applyToSetCookie (domain x)
      <> applyToSetCookie (path x)

instance ApplyToSetCookie Meta where
  applyToSetCookie x =
    applyToSetCookie (metaScope x)
      <> applyToSetCookie (metaSecurity x)

instance (ApplyToSetCookie a) => ApplyToSetCookie (Secured a) where
  applyToSetCookie x =
    applyToSetCookie (security x)
      <> applyToSetCookie (secured x)

instance (ApplyToSetCookie a) => ApplyToSetCookie (Action a) where
  applyToSetCookie = \case
    Put x -> applyToSetCookie x
    Delete ->
      applyToSetCookie (ExpiryTime (Time.posixSecondsToUTCTime 0))
        <> applyToSetCookie (Value (BS.Char8.pack "x"))

instance (ApplyToSetCookie a) => ApplyToSetCookie (Expiring a) where
  applyToSetCookie x =
    applyToSetCookie (expiry x)
      <> applyToSetCookie (expiring x)
