module GeomViewer.Viewport.Dynamic.SDL
  ( pannableZoomableViewportDyn
  , pannableViewportDyn
  , zoomableViewportDyn
  )
  where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           GeomViewer.Viewport
import qualified GeomViewer.Viewport.Dynamic as Dynamic
import           Reflex
import           Reflex.SDL2 hiding (point, Rectangle, Point, origin)
import           SDL.GeometryUtil

--------------------------------------------------------------------------------
-- * SDL viewports

-- | Viewport supporting both panning and zooming (using the mouse).
pannableZoomableViewportDyn       :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                                  => ZoomConfig r -- ^ initial zoom convig
                                  -> Viewport r -- ^ initial viewport
                                  -> m (Dynamic t (Viewport r))
pannableZoomableViewportDyn z0 v0 = do
    mousePosDyn <- fmap (maybe origin (^.core)) <$> mousePositionDyn
    clickEvts   <- mouseClickEvent
    wheelEvts   <- fromWheelEvents <$> getMouseWheelEvent
    Dynamic.pannableZoomableViewportDyn z0 v0 mousePosDyn $ leftmost [ Left  <$> clickEvts
                                                                      , Right <$> wheelEvts
                                                                      ]
-- | Viewport supporting only panning (using the mouse)
pannableViewportDyn    :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                       => Viewport r -- ^ initial viewport
                       -> m (Dynamic t (Viewport r))
pannableViewportDyn v0 = do mousePosDyn <- fmap (maybe origin (^.core)) <$> mousePositionDyn
                            clickEvts   <- mouseClickEvent
                            Dynamic.pannableViewportDyn mousePosDyn v0 clickEvts

-- | Viewport supporting only zooming (by using the scroll wheel)
zoomableViewportDyn       :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                          => ZoomConfig r -- ^ zoom configuration
                          -> Viewport r -- ^ initial viewport
                          -> m (Dynamic t (Viewport r))
zoomableViewportDyn z0 v0 = Dynamic.zoomableViewportDyn z0 v0 . fromWheelEvents
                             =<< getMouseWheelEvent


fromWheelEvents :: (Reflex t, Fractional r) => Event t MouseWheelEventData -> Event t r
fromWheelEvents = fmap f
  where
    f evt = let V2 _ delta = fromIntegral <$> mouseWheelEventPos evt
            in delta / maxScrollSpeed


-- | maximum scroll speed
maxScrollSpeed :: Num r => r
maxScrollSpeed = 10
