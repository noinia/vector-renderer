{-# LANGUAGE TemplateHaskell #-}
module VectorRenderer.Viewport where

import Control.Lens
import Data.Geometry.Box
import Data.Geometry.Transformation
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Ext

--------------------------------------------------------------------------------

data Viewport r = Viewport { _viewPort    :: Rectangle () r -- in host world
                           , _worldToHost :: Transformation 2 r
                           -- , _hostToWorld :: Transformation 2 r
                           }
makeLenses ''Viewport

-- | Flips the y-coordinate so that the origin is in the bottom left.
flipY               :: Num r => Vector 2 r -> Viewport r
flipY (Vector2 w h) = Viewport (box (ext origin) (ext $ Point2 w h))
                               (flipY' h)




flipY'   :: Num r => r -> Transformation 2 r
flipY' h = translation (Vector2 0 h) |.| scaling (Vector2 1 (-1))
