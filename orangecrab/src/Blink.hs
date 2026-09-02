{-|
Blinking RGB led.
-}
module Blink where

import Clash.Annotations.TH
import Clash.Prelude

import Orangecrab.Domain
import RGB (RGB, Color, red, green, blue, driveRGB)


topEntity ::
  -- | Orangecrab clock pin
  "CLK" ::: Clock Dom48 ->
  -- | Builtin orangecrab button
  "BTN" ::: Reset Dom48 ->
  -- | Orangecrab pin B8, as defined in the `orangecrab.pcf` file
  "PMOD1_4" ::: Signal Dom48 Bool ->
  -- | Builtin orangecrab LED
  "rgb_led0" ::: Signal Dom48 RGB
topEntity clk rst btn = withClockResetEnable clk rst enableGen (blink btn)


blink ::
  -- | Constraint hiding the Clock, Reset, and Enable signals
  HiddenClockResetEnable dom =>
  -- | Input for whether to pause counter
  Signal dom Bool ->
  -- | Output color for LED (as RGB)
  Signal dom RGB
blink btn = driveRGB (mealy blinkStep initState btn)
 where
  initState = (0 :: Unsigned 26, 0 :: Index 3)

  -- Step function for state machine
  blinkStep (counter, colorIndex) pauseCounter =
    ( (newCounter, newColorIndex) -- Next state of state machine
    , blinkColors !! colorIndex   -- Output of state machine
    )
   where
    newCounter = if pauseCounter then counter else counter + 1
    newColorIndex = if counter == 0 then satSucc SatWrap colorIndex else colorIndex

  -- Colors to cycle through
  blinkColors :: Vec 3 Color
  blinkColors = red :> green :> blue :> Nil


makeTopEntity 'topEntity
