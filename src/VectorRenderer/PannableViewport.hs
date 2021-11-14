
module VectorRenderer.PannableViewport
  ( pannableViewportDyn
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
import           VectorRenderer.Viewport

--------------------------------------------------------------------------------

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
