module Main (main) where

-- You'll definitely need to import this.
import CookieTray

-- You'll probably need to import only one of these.
import CookieTray.Command.Many (Many (Many))
import CookieTray.Command.PutMany (PutMany (PutMany))
import CookieTray.Command.PutOne (PutOne (PutOne))

import CookieTray.Time (year)
import Data.ByteString.Lazy qualified as LBS
import Data.Time qualified as Time
import Test.Hspec (hspec, shouldBe, specify)
import Prelude

main :: IO ()
main = hspec do
  -- The simplest case is using 'PutOne' to set a single cookie.
  specify "put one" do
    shouldBe @[LBS.ByteString]
      (renderLBS $ PutOne "userId" "917" (ExpiryAge year) (Meta scope1 security1))
      ["userId=917; Max-Age=31536000; Domain=.example.com; SameSite=Lax"]

  -- If you need to set multiple cookies, it might be slightly more
  -- convenient to assemble the data into a 'Tray' and render the
  -- entire tray at once using 'PutMany'.
  specify "put many" do
    shouldBe @[LBS.ByteString]
      (renderLBS $ PutMany [Named "a" "1", Named "b" "2"] (ExpiryTime time1) (Meta scope1 security2))
      [ "a=1; Expires=Sat, 01-Jan-2050 00:00:00 GMT; Domain=.example.com; Secure; SameSite=Strict",
        "b=2; Expires=Sat, 01-Jan-2050 00:00:00 GMT; Domain=.example.com; Secure; SameSite=Strict"
      ]

  -- The most general utility is 'Many', which can handle a 'Tray' of
  -- actions, where each action is either 'Put' or 'Delete'.
  specify "put and delete" do
    shouldBe @[LBS.ByteString]
      (renderLBS $ Many [Named "a" Delete, Named "b" (Put "xyz")] (ExpiryAge year) (Meta scope2 security1))
      [ "a=x; Path=/docs; Expires=Thu, 01-Jan-1970 00:00:00 GMT; SameSite=Lax",
        "b=xyz; Path=/docs; Max-Age=31536000; SameSite=Lax"
      ]

scope1 :: Scope
scope1 = Scope (Domain ".example.com") CurrentPath

scope2 :: Scope
scope2 = Scope CurrentHostExcludingSubdomains (Path "/docs")

security1 :: Security
security1 = Security AccessibleFromJavascript (SameSite (SameSiteOptions SameSiteLax AllowUnencryptedTransport))

security2 :: Security
security2 = Security AccessibleFromJavascript (SameSite (SameSiteOptions SameSiteStrict RequireEncryptedTransport))

time1 :: Time.UTCTime
time1 = Time.UTCTime (Time.fromGregorian 2050 1 1) 0
