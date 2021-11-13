{-# LANGUAGE TemplateHaskell #-}
module VectorRenderer.Viewport
  ( Viewport(Viewport), mkViewport
  , viewPort, worldToHost
  , flipY
  , centeredOrigin
  , alignedOrigin
  )
  where

import Control.Lens hiding (Zoom)
import Data.Bifunctor
import Data.Ext
import Data.Geometry.Box
import Data.Geometry.Point
import Data.Geometry.Transformation
import Data.Geometry.Vector
import Data.Range

--------------------------------------------------------------------------------

data Viewport r = Viewport { _viewPort    :: Rectangle () r -- in host world
                           , _worldToHost :: Transformation 2 r
                           -- , _hostToWorld :: Transformation 2 r
                           }
makeLenses ''Viewport

-- | Creates a viewport from the given rectangle and the
-- transformation. The transformation is applied with respect to the
-- center of the viewport.
mkViewport     :: Fractional r => Rectangle p r -> Transformation 2 r -> Viewport r
mkViewport r t = centeredOrigin r & worldToHost %~ (|.| t)

--------------------------------------------------------------------------------

-- | Flips the y-coordinate so that the origin is in the bottom left.
flipY               :: Num r => Vector 2 r -> Viewport r
flipY (Vector2 w h) = Viewport (box (ext origin) (ext $ Point2 w h))
                               (flipY' h)

-- | Transformation that flips the y-axis and shifts by h, essenitally
-- moving the origin from the top-left facing downards to the
-- bottom-left and upwards.
flipY'   :: Num r => r -> Transformation 2 r
flipY' h = translation (Vector2 0 h) |.| scaling (Vector2 1 (-1))

--------------------------------------------------------------------------------

-- | Creates a viewport in which the origin is at the center of the viewport
centeredOrigin       :: Fractional r => Rectangle p r -> Viewport r
centeredOrigin rect' = Viewport (first (const ()) rect') (translation $ centerPoint rect' .-. origin)

-- | Creates a viewport in which the origin at the bottom left of the viewport
alignedOrigin       :: Num r => Rectangle p r -> Viewport r
alignedOrigin rect' = Viewport (first (const ()) rect')  (translation $ bottomLeft .-. origin)
  where
    bottomLeft = rect'^.to minPoint.core


--------------------------------------------------------------------------------




data Zoom r = Zoom { _range        :: Range r
                   , _currentLevel :: r
                   }
            deriving (Show,Eq)
makeLenses ''Zoom

defaultZoom :: Fractional r => Zoom r
defaultZoom = Zoom (ClosedRange 0.1 4) 1
