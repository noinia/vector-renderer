
module VectorRenderer.PannableViewport
  ( pannableViewportDyn
  , zoomableViewportDyn, zoomableViewportDyn'
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import qualified Data.Geometry.Transformation as Transformation
import           Reflex
import           Reflex.SDL2 hiding (point, Rectangle, Point)
import           SDL.GeometryUtil
import           UI.Viewport

--------------------------------------------------------------------------------
-- * Pannable viewport

-- | Pannable viewport
pannableViewportDyn    :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                       => Viewport r -- ^ initial viewport
                       -> m (Dynamic t (Viewport r))
pannableViewportDyn v0 = do dViewport <- foldDyn f (v0, Nothing) . updated =<< transformDyn
                            pure $ fmap fst dViewport
  where
    f mt (vp, mvps) = case (mt, mvps) of
      (Nothing, Nothing)  -> (vp, mvps) -- initial actions when nothing is actually happening
      (Nothing, Just _) -> (vp, Nothing) -- stop panning
      (Just t, Nothing)   -> (vp&worldToHost %~ (t |.|), Just vp) -- start panning
      (Just t, Just vps)  -> (vps&worldToHost %~ (t |.|), Just vps) -- continue panning


-- | Transformation for the pannable viewpoint
transformDyn :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
             => m (Dynamic t (Maybe (Transformation 2 r)))
transformDyn = zipDynWith combine <$> panDyn <*> mousePositionDyn
  where
    combine = liftA2 (\s (p :+ _) -> Transformation.translation $ p .-. s)


-- | When we start and stop panning
panDyn :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
       => m (Dynamic t (Maybe (Point 2 r)))
panDyn = foldDyn (\(p :+ _) -> \case
                     Nothing -> Just p
                     Just _  -> Nothing
                 ) Nothing =<< mouseClickEvent


--------------------------------------------------------------------------------
-- * Zoomable viewport

-- | Zoomable viewport
zoomableViewportDyn     :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                        => ZoomConfig r -- ^ zoom configuration
                        -> Viewport r -- ^ initial viewport
                        -> m (Dynamic t (Viewport r))
zoomableViewportDyn z0 = fmap (fmap fst) . zoomableViewportDyn' z0


-- | A Zoomable viewport toether with its zoom configuration.
zoomableViewportDyn'       :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                           => ZoomConfig r -- ^ zoom configuration
                           -> Viewport r -- ^ initial viewport
                           -> m (Dynamic t (Viewport r, ZoomConfig r))
zoomableViewportDyn' z0 v0 = foldDyn f (v0,z0) . updated =<< zoomDyn z0
  where
    f z' (vp,z) = let delta  = z'^.currentLevel - z^.currentLevel
                      lambda = 1 + (delta / z^.currentLevel)
                      t      = wrtCenter vp $ uniformScaling lambda
                  in (vp&worldToHost %~ (t |.|), z')

-- | Maintain a zoom config
zoomDyn    :: (Fractional r, Ord r, ReflexSDL2 t m)
           => ZoomConfig r -> m (Dynamic t (ZoomConfig r))
zoomDyn z0 = foldDyn f z0 =<< getMouseWheelEvent
  where
    f evt z = let V2 _ delta' = fromIntegral <$> mouseWheelEventPos evt
                  delta       = delta' / maxScrollSpeed
              in z&currentLevel %~ (+ delta)

-- | maximum scroll speed
maxScrollSpeed :: Num r => r
maxScrollSpeed = 10

--------------------------------------------------------------------------------

-- pannableZoomableViewportDyn      :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
--                                  => ZoomConfig r -- ^ zoom configuration
--                                  -> Viewport r -- ^ initial viewport
--                                  -> m (Dynamic t (Viewport r))
-- pannableZoomableViewportDyn z v0 = zipDynWith f (pannableViewportDyn v0) (zoomableViewportDyn z v0)
--   where
--     f =
