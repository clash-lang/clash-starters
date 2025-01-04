{-# OPTIONS_GHC -Wno-orphans #-}
module DECA where

import Clash.Prelude
import Clash.Intel.ClockGen
import Clash.Annotations.SynthesisAttributes

data LedMode
  = Rotate
  -- ^ After some period, rotate active led to the left
  | Complement
  -- ^ After some period, turn on all disable LEDs, and vice versa
  deriving (Show, Eq, Enum, Generic, NFDataX)

-- Define a synthesis domain with a clock with a period of 20000 /ps/.
createDomain vSystem{vName="Input", vPeriod=20000}

-- Define a synthesis domain with a clock with a period of 50000 /ps/.
createDomain vSystem{vName="Dom50", vPeriod=50000}

{-# ANN deca
  (Synthesize
    { t_name   = "deca"
    , t_inputs = [ PortName "MAX10_CLK1_50"
                 , PortName "KEY0"
                 , PortName "KEY1"
                 ]
    , t_output = PortName "LED"
    }) #-}
deca
  :: Clock Input
      `Annotate` 'StringAttr "chip_pin" "M8"
      `Annotate` 'StringAttr "altera_attribute" "-name IO_STANDARD \"2.5 V\""
  -- ^ Incoming clock
  --
  -- Annotate with attributes to map the argument to the correct pin, with the
  -- correct voltage settings, on the DECA development kit.
  -> Signal Input Bool
      `Annotate` 'StringAttr "chip_pin" "H21"
      `Annotate` 'StringAttr "altera_attribute" "-name IO_STANDARD \"1.5 V Schmitt Trigger\""
  -- ^ Reset signal, straight from KEY0
  -> Signal Dom50 Bit
      `Annotate` 'StringAttr "chip_pin" "H22"
      `Annotate` 'StringAttr "altera_attribute" "-name IO_STANDARD \"1.5 V Schmitt Trigger\""
  -- ^ Mode choice, straight from KEY1. See 'LedMode'.
  -> Signal Dom50 (BitVector 8)
      `Annotate` 'StringAttr "chip_pin" "C5, B4, A5, C4, B7, A6, C8, C7"
      `Annotate` 'StringAttr "altera_attribute" "-name IO_STANDARD \"1.2 V\""
  -- ^ Output containing 8 bits, corresponding to 8 LEDs, LEDs are active-low
  --
  -- Use comma-seperated list in the "chip_pin" attribute to maps the individual
  -- bits of the result to the correct pins on the DECA development kit
deca clk20 rstBtn modeBtn =
  exposeClockResetEnable
    (mealy blinkerT initialStateBlinkerT . isRising 1)
    clk50
    rstSync
    en
    modeBtn
 where
  -- | Enable line for subcomponents: we'll keep it always running
  en = enableGen

  -- Start with the first LED turned on, in rotate mode, with the counter on zero
  initialStateBlinkerT = (1, Rotate, 0)

  -- Signal coming from the reset button is low when pressed, and high when
  -- not pressed. We convert this signal to the polarity of our domain with
  -- 'unsafeFromLowPolarity'.
  rst = unsafeFromLowPolarity rstBtn

  -- Instantiate a PLL: this stabilizes the incoming clock signal and indicates
  -- when the signal is stable. We're also using it to transform an incoming
  -- clock signal running at 20 MHz to a clock signal running at 50 MHz.
  (clk50, pllStable) =
    altpll
      @Dom50
      (SSymbol @"altpll50")
      clk20
      rst

  -- Synchronize reset to clock signal coming from PLL. We want the reset to
  -- remain active while the PLL is NOT stable, hence the conversion with
  -- 'unsafeFromLowPolarity'
  rstSync =
    resetSynchronizer
      clk50
      (unsafeFromLowPolarity pllStable)

-- | Changes the LED mode
--
-- >>> flipMode Rotate
-- Complement
-- >>> flipMode Complement
-- Rotate
flipMode :: LedMode -> LedMode
flipMode Rotate = Complement
flipMode Complement = Rotate

blinkerT
  :: (BitVector 8, LedMode, Index 6660000)
  -> Bool
  -> ((BitVector 8, LedMode, Index 6660000), BitVector 8)
blinkerT (leds, mode, cntr) key1R = ((leds', mode', cntr'), complement leds)
  where
    -- clock frequency = 20e6  (20 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = maxBound :: Index 6660000 -- 20e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = flipMode mode
          | otherwise = mode

    leds' | cntr == 0 =
              case mode of
                Rotate -> rotateL leds 1
                Complement -> complement leds
          | otherwise = leds
