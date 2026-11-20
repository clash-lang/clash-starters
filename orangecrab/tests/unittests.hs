import Prelude

import Test.Tasty

import qualified Tests.Blink

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Blink.blinkTests
  ]
