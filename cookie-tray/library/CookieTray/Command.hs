module CookieTray.Command where

import CookieTray.Types
import Prelude (Eq, Ord, Show)

data Command = Command
  { name :: Name,
    meta :: Meta,
    action :: Action (Expiring Value)
  }
  deriving (Eq, Ord, Show)

class ToCommandList a where
  toCommandList :: a -> [Command]

instance ToCommandList Command where
  toCommandList = (: [])
