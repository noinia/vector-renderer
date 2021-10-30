module SDL.GeometryUtil where

import Data.Geometry.Point
import Data.Ext
import Reflex
import Reflex.SDL2 hiding (Point)

--------------------------------------------------------------------------------

-- | Construct a Dyn with the mouse position. May be nothing if we are
-- outside of the window.
mousePositionDyn :: (Num r, ReflexSDL2 t m)
                 => m (Dynamic t (Maybe (Point 2 r :+ MouseMotionEventData)))
mousePositionDyn = do evMouseMove <- getMouseMotionEvent
                      -- I guess we still want to detect when we go outside of the screen again.
                      dMousePos <- holdDyn Nothing (Just <$> evMouseMove)
                      pure $ fmap (fmap f) dMousePos
  where
    f eventData = let P (V2 x y) = fromIntegral <$> mouseMotionEventPos eventData
                  in Point2 x y :+ eventData


-- | Mouseclick event
mouseClickEvent :: (Num r, ReflexSDL2 t m)
                => m (Event t (Point 2 r :+ MouseButtonEventData))
mouseClickEvent = fmap f <$> getMouseButtonEvent
  where
    f eventData = let P (V2 x y) = fromIntegral <$> mouseButtonEventPos eventData
                  in Point2 x y :+ eventData


-- | Convert to a proper math coordinate system with the origin in the bottom left
toMathCoords            :: Num r => r -> V2 r -> V2 r
toMathCoords h (V2 x y) = V2 x (h - y)
