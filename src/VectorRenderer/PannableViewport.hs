
module VectorRenderer.PannableViewport
  -- ( pannableViewportDyn
  -- , zoomableViewportDyn, zoomableViewportDyn'
  -- )
  where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import qualified Data.Geometry.Transformation as Transformation
import           Reflex
import           Reflex.SDL2 hiding (point, Rectangle, Point, origin)
import           SDL.GeometryUtil
import           UI.Viewport


--------------------------------------------------------------------------------
-- * SDL viewports

pannableViewportDyn'    :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                        => Viewport r -- ^ initial viewport
                        -> m (Dynamic t (Viewport r))
pannableViewportDyn' v0 = do mousePosDyn <- fmap (maybe origin (^.core)) <$> mousePositionDyn
                             clickEvts   <- mouseClickEvent
                             pannableViewportDyn mousePosDyn v0 clickEvts

--------------------------------------------------------------------------------
-- * Pannable viewport

pannableViewportDyn             :: (Reflex t, MonadHold t m, MonadFix m, Num r)
                                => Dynamic t (Point 2 r) -- ^ mouse pos
                                -> Viewport r --
                                -> Event t a
                                -> m (Dynamic t (Viewport r))
pannableViewportDyn mousePos v0 = viewportDyn v0 <=< panEvents mousePos









viewportDyn    :: (Reflex t, MonadHold t m, MonadFix m)
               => Viewport r
               -> Event t (Transformation 2 r -> Transformation 2 r)
               -> m (Dynamic t (Viewport r))
viewportDyn v0 = applyUpdates v0 . fmap (\f vp -> vp&worldToHost %~ f)

applyUpdates :: (Reflex t, MonadHold t m, MonadFix m) => a -> Event t (a -> a) -> m (Dynamic t a)
applyUpdates = foldDyn ($)



mousePosAtLastClick          :: (Reflex t, MonadHold t m, MonadFix m)
                             => Dynamic t p
                             -> Event t a
                             -> m (Dynamic t (Maybe p))
mousePosAtLastClick mousePos = foldDyn (\p -> \case
                                           Nothing -> Just p
                                           Just _  -> Nothing
                                       ) Nothing
                             . tag (current mousePos)

deltaTransform         :: forall t m r a. (Reflex t, MonadHold t m, MonadFix m, Num r)
                       => Dynamic t (Point 2 r)
                       -> Event t a
                       -> m (Dynamic t (Maybe (Dynamic t (Transformation 2 r))))
deltaTransform mousePos = fmap f
                        . mousePosAtLastClick mousePos
  where
    f :: Dynamic t (Maybe (Point 2 r))
      -> Dynamic t (Maybe (Dynamic t (Transformation 2 r)))
    f = fmap (fmap g)

    g   :: Point 2 r -> Dynamic t (Transformation 2 r)
    g s = fmap (\p -> Transformation.translation $ p .-. s) mousePos


      -- zipDynWith (\p -> fmap (\s -> Transformation.translation $ p .-. s)) mousePos


panDyn          :: forall t m r a. (Reflex t, MonadHold t m, MonadFix m, Num r)
                => Dynamic t (Point 2 r)
                -> Event t a
                -> m (Dynamic t (Transformation 2 r -> Transformation 2 r))
panDyn mousePos = fmap f . deltaTransform mousePos
  where
    f :: Dynamic t (Maybe (Dynamic t (Transformation 2 r)))
      -> Dynamic t (Transformation 2 r -> Transformation 2 r)
    f = join . fmap (\case
                        Nothing       -> constDyn id
                        Just deltaDyn -> (\delta t -> delta |.| t) <$> deltaDyn
                    )

panEvents          :: forall t m r a. (Reflex t, MonadHold t m, MonadFix m, Num r)
                   => Dynamic t (Point 2 r)
                   -> Event t a
                   -> m (Event t (Transformation 2 r -> Transformation 2 r))
panEvents mousePos = fmap updated . panDyn mousePos


-- panEvents         :: forall t m r a. (Reflex t, MonadHold t m, MonadFix m)
--                   => Dynamic t (Point 2 r) -- ^ mouse position
--                   -> Event t a -- ^ update events
--                   -> m (Event t (Transformation 2 r -> Transformation 2 r))
-- panEvents mousePos = fmap f
--                    . mousePosAtLastClick (current mousePos)
--   where
--     f :: Event t (Maybe (Point 2 r)) -> Event t (Transformation 2 r -> Transformation 2 r)
--     f = undefined


    -- f p = maybe Transformation.identity (\s -> Transformation.translation $ p .-. s)

-- panEvents :: Event (Update a)
