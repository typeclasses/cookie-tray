module CookieTray.Command.PutMany where

import CookieTray (Action (..), Expiring (Expiring), Expiry, Meta (..), ToCommandList (..), Tray, Value)
import CookieTray qualified
import CookieTray.Command (Command (Command))
import CookieTray.Command qualified as Command
import Data.Functor ((<&>))
import Prelude (Eq, Ord, Show)

data PutMany = PutMany
  { tray :: Tray Value,
    expiry :: Expiry,
    meta :: Meta
  }
  deriving (Eq, Ord, Show)

instance ToCommandList PutMany where
  toCommandList x =
    CookieTray.toList (tray x) <&> \y ->
      Command
        { Command.name = CookieTray.name y,
          Command.meta = meta x,
          Command.action =
            Put
              Expiring
                { CookieTray.expiring = CookieTray.value y,
                  CookieTray.expiry = expiry x
                }
        }
