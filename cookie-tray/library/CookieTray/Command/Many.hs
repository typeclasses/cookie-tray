module CookieTray.Command.Many where

import CookieTray (Action (..), Expiring (Expiring), Expiry, Meta (..), Named (..), ToCommandList (..), Tray, Value)
import CookieTray qualified
import CookieTray.Command (Command (Command))
import CookieTray.Command qualified as Command
import Data.Functor ((<&>))
import Prelude (Eq, Ord, Show)

data Many = Many
  { tray :: Tray (Action Value),
    expiry :: Expiry,
    meta :: Meta
  }
  deriving (Eq, Ord, Show)

instance ToCommandList Many where
  toCommandList x =
    CookieTray.toList (tray x) <&> \y ->
      Command
        { Command.name = CookieTray.name y,
          Command.meta = meta x,
          Command.action =
            value y <&> \z ->
              Expiring
                { CookieTray.expiring = z,
                  CookieTray.expiry = expiry x
                }
        }
