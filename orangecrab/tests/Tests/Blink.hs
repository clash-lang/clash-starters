module Tests.Blink where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as List

import Blink (blink)
import RGB (RGB(..))


-- Test the property that blink drives at most one of the (r, g, b) leds within
-- the orangecrab multicolor led at a time.
prop_blink :: H.Property
prop_blink = H.property $ do
  numTestCycles <- H.forAll (Gen.integral (Range.linear 3 500))

  let
    input :: [Bool]
    input = List.repeat False

    numberRgbProperty :: RGB -> Bool
    -- Since the LEDs are active low, we check if each color is active by checking the
    -- `not` of the corresponding color.
    numberRgbProperty (RGB r g b) = 1 >= (fromEnum (not r) + fromEnum (not g) + fromEnum (not b))

    output :: [RGB]
    output = C.sample @C.System (blink (C.fromList input))

    -- Drop the first two clock cycles, which corresponds to RESET, and then
    -- take the next `numTestCycles` cycles to test (so that we don't try to
    -- compare two infinite lists, causing the test to hang).
    outputFinite :: [RGB]
    outputFinite = List.take numTestCycles (List.drop 2 output)

    propertyHolds :: [Bool]
    propertyHolds = List.map numberRgbProperty outputFinite

  propertyHolds H.=== List.replicate numTestCycles True


blinkTests :: TestTree
blinkTests = $(testGroupGenerator)


main :: IO ()
main = defaultMain blinkTests
