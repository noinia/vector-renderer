{-# LANGUAGE TemplateHaskell #-}
module VectorRenderer.Viewport
  ( Viewport(Viewport), mkViewport
  , viewPort, worldToHost, hostToWorld
  , toWorldIn, toHostFrom
  , flipY
  , centeredOrigin
  , alignedOrigin
  -- * Panning
  , PanStatus(..)
  , applyPan
  )
  where

import Control.Lens hiding (Zoom)
import Data.Bifunctor
import Data.Ext
import Data.Geometry.Box
import Data.Geometry.Point
import Data.Geometry.Transformation
import Data.Geometry.Vector
import Data.Geometry.Properties
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


-- | Host to world transformation, i.e. given a point in the host
-- coordinate system, we can compute the point in world coordinates
-- using this transformation.
hostToWorld :: Fractional r => Getter (Viewport r) (Transformation 2 r)
hostToWorld = worldToHost.to inverseOf

--------------------------------------------------------------------------------

-- | Convert some geometry in host coordinates to world coordinates in
-- the viewport
toWorldIn    :: (IsTransformable g, NumType g ~ r, Dimension g ~ 2, Fractional r)
             => Viewport r -> g -> g
toWorldIn vp = transformBy (vp^.hostToWorld)

-- | Convert some geometry in world coordinates to host coordinates
-- according to the viewport
toHostFrom  :: (IsTransformable g, NumType g ~ r, Dimension g ~ 2, Num r)
             => Viewport r -> g -> g
toHostFrom vp = transformBy (vp^.worldToHost)


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
-- * Panning

data PanStatus r = NoPan
                 | PanFrom (Point 2 r) -- ^ point from which we are panning
                           (Point 2 r) -- ^ center point of the viewport at that time
                 deriving (Show,Eq)


applyPan         :: Num r
                 => PanStatus r
                 -> Point 2 r -- ^ current mouse position
                 -> Point 2 r   -- ^ current center
                 -> Point 2 r
applyPan ps p c = case ps of
  NoPan        -> c
  PanFrom q oc -> let Vector2 vx vy = p .-. q in oc .+^  Vector2 ((-1)*vx) vy



--------------------------------------------------------------------------------
data Zoom r = Zoom { _range        :: Range r
                   , _currentLevel :: r
                   }
            deriving (Show,Eq)
makeLenses ''Zoom

defaultZoom :: Fractional r => Zoom r
defaultZoom = Zoom (ClosedRange 0.1 4) 1
