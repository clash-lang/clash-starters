module Tests.DECA where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

import DECA (LedMode (..), flipMode)

prop_flipTwiceOriginal :: H.Property
prop_flipTwiceOriginal = H.property $ do
  a <- H.forAll (Gen.enum Rotate Complement)
  a === flipMode (flipMode a)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
