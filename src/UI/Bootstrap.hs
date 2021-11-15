{-# LANGUAGE TemplateHaskell #-}
module VectorRenderer.Bootstrap where

import Control.Lens
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import Linear.V4(V4(..))

--------------------------------------------------------------------------------

data ThemeColor = ThemeColor { _primary   :: Canvas.Color
                             , _secondary :: Canvas.Color
                             , _success   :: Canvas.Color
                             , _info      :: Canvas.Color
                             , _danger    :: Canvas.Color
                             , _warning   :: Canvas.Color
                             , _dark      :: Canvas.Color
                             , _light     :: Canvas.Color
                             } deriving (Show,Eq,Ord)
makeLenses ''ThemeColor

bootstrapColors :: ThemeColor
bootstrapColors = ThemeColor { _primary   =  V4 0x0d 0x6e 0xfd 255
                             , _secondary =  V4 0x6c 0x75 0x7d 255
                             , _success   =  V4 0x19 0x87 0x54 255
                             , _info      =  V4 0x0d 0xca 0xf0 255
                             , _warning   =  V4 0xff 0xc1 0x07 255
                             , _danger    =  V4 0xdc 0x35 0x45 255
                             , _dark      =  V4 0x21 0x25 0x29 255
                             , _light     =  V4 0xf8 0xf9 0xfa 255
                             }
