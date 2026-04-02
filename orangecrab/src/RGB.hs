{-|
RGB led color mixing.
-}
{-# LANGUAGE OverloadedRecordDot #-}
module RGB where

import Clash.Prelude

-- | Color values.
data Color =
  Color
    { r :: Unsigned 8
    , g :: Unsigned 8
    , b :: Unsigned 8
    }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

red, green, blue :: Color
red    = Color 255   0   0
green  = Color   0 255   0
blue   = Color   0   0 255

-- | RBG led interface
data RGB =
  RGB
    { rLed :: "r" ::: Bool
    , gLed :: "g" ::: Bool
    , bLed :: "b" ::: Bool
    }
  deriving (Generic, NFDataX, BitPack)

-- | Mixes color to an RGB led via pulse width modulation.
driveRGB ::
  (KnownDomain dom, HiddenClockResetEnable dom) =>
  Signal dom Color ->
  Signal dom RGB
driveRGB desiredColor = mealy drive 0 desiredColor
 where
  drive counter color =
    ( counter + 1
    , RGB (counter >= color.r) (counter >= color.g) (counter >= color.b)
    )
