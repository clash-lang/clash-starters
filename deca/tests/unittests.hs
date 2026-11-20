import Prelude

import Test.Tasty

import qualified Tests.DECA

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.DECA.tests
  ]
