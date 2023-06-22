module CookieTray.Time where

import Data.Time qualified as Time
import Prelude (($), (*), round)

-- | Roughly the length of a year
--
-- This can be useful for setting cookie expiration times.
year :: Time.DiffTime
year = Time.secondsToDiffTime $ round $ Time.nominalDiffTimeToSeconds $ Time.nominalDay * 365
