module CookieTray.Command.PutOne where

import CookieTray (Action (..), Expiring (Expiring), Expiry, Meta (..), Name, ToCommandList (..), Value)
import CookieTray qualified
import CookieTray.Command (Command (Command))
import CookieTray.Command qualified as Command
import Prelude (Eq, Ord, Show)

data PutOne = PutOne
  { name :: Name,
    value :: Value,
    expiry :: Expiry,
    meta :: Meta
  }
  deriving (Eq, Ord, Show)

instance ToCommandList PutOne where
  toCommandList x =
    (: [])
      Command
        { Command.name = name x,
          Command.meta = meta x,
          Command.action =
            Put
              Expiring
                { CookieTray.expiring = value x,
                  CookieTray.expiry = expiry x
                }
        }
