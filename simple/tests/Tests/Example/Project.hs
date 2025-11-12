module Tests.Example.Project where

import Prelude

import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Import the module containing the @accum@ function
import Example.Project (accum)

-- Define a Hedgehog property to test the @accum@ function
prop_accum :: H.Property
prop_accum = H.property $ do

  -- Simulate for a random duration between 1 and 100 cycles
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  -- Generate a list of random unsigned numbers.
  inp <- H.forAll
    (Gen.list (Range.singleton simDuration)
    (genUnsigned Range.linearBounded))
  let

    -- Simulate the @accum@ function for the pre-existing @System@ domain
    -- and 8 bit unsigned numbers.
    --
    -- The (hidden) reset input of @accum@ will be asserted in the first cycle;
    -- during this cycle it will emit its initial value and the input is
    -- ignored. So we need to present a dummy input value.
    simOut = C.sampleN (simDuration + 1) (accum @C.System @8 (C.fromList (0:inp)))
    -- Calculate the expected output. The first cycle is the initial value, and
    -- the result of the final input value does not appear because the
    -- accumulator has 1 cycle latency.
    expected = 0 : init (scanl (+) 0 inp)

  -- Check that the simulated output matches the expected output
  simOut H.=== expected

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
