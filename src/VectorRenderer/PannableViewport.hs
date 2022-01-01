{-# LANGUAGE RecursiveDo #-}

module VectorRenderer.PannableViewport
  -- ( pannableViewportDyn
  -- , zoomableViewportDyn, zoomableViewportDyn'
  -- )
  where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Data.Bifunctor
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


pannableZoomableViewportDyn'       :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                                   => ZoomConfig r -- ^ initial zoom convig
                                   -> Viewport r -- ^ initial viewport
                                   -> m (Dynamic t (Viewport r))
pannableZoomableViewportDyn' z0 v0 = do
    mousePosDyn <- fmap (maybe origin (^.core)) <$> mousePositionDyn
    clickEvts   <- mouseClickEvent
    wheelEvts   <- fromWheelEvents <$> getMouseWheelEvent
    pannableZoomableViewportDyn z0 v0 mousePosDyn $ leftmost [ Left  <$> clickEvts
                                                             , Right <$> wheelEvts
                                                             ]

pannableViewportDyn'    :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                        => Viewport r -- ^ initial viewport
                        -> m (Dynamic t (Viewport r))
pannableViewportDyn' v0 = do mousePosDyn <- fmap (maybe origin (^.core)) <$> mousePositionDyn
                             clickEvts   <- mouseClickEvent
                             pannableViewportDyn mousePosDyn v0 clickEvts

-- | Zoomable viewport
zoomableViewportDyn'       :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
                           => ZoomConfig r -- ^ zoom configuration
                           -> Viewport r -- ^ initial viewport
                           -> m (Dynamic t (Viewport r))
zoomableViewportDyn' z0 v0 = zoomableViewportDyn z0 v0 . fromWheelEvents =<< getMouseWheelEvent


fromWheelEvents :: (Reflex t, Fractional r) => Event t MouseWheelEventData -> Event t r
fromWheelEvents = fmap f
  where
    f evt = let V2 _ delta = fromIntegral <$> mouseWheelEventPos evt
            in delta / maxScrollSpeed


-- | maximum scroll speed
maxScrollSpeed :: Num r => r
maxScrollSpeed = 10

--------------------------------------------------------------------------------
-- * Pannable viewport

pannableViewportDyn             :: (Reflex t, MonadHold t m, MonadFix m, Num r)
                                => Dynamic t (Point 2 r) -- ^ mouse pos
                                -> Viewport r --
                                -> Event t a
                                -> m (Dynamic t (Viewport r))
pannableViewportDyn mousePos v0 =
  viewportDynM v0 (panEvents mousePos . fmap (&_1 %~ view worldToHost))


--------------------------------------------------------------------------------

viewportDynM                    :: (Reflex t, MonadHold t m, MonadFix m)
                                => Viewport r
                                -> (Event t (Viewport r, a)
                                    -> m (Event t (Transformation 2 r -> Transformation 2 r)))
                                -> Event t a
                                -> m (Dynamic t (Viewport r))
viewportDynM v0 mkEvents events = do
  rec dyn <- viewportDyn v0 <=< mkEvents . attach (current dyn) $ events
  pure dyn

viewportDyn    :: (Reflex t, MonadHold t m, MonadFix m)
               => Viewport r
               -- ^ Initial viewport
               -> Event t (Transformation 2 r -> Transformation 2 r)
               -- ^ Updates that we want ot apply
               -> m (Dynamic t (Viewport r))
viewportDyn v0 = applyUpdates v0 . fmap (\f vp -> vp&worldToHost %~ f)


applyUpdates :: (Reflex t, MonadHold t m, MonadFix m)
             => a -> Event t (a -> a) -> m (Dynamic t a)
applyUpdates = foldDyn ($)

----------------------------------------------------------------------------------


panEvents          :: (Reflex t, MonadHold t m, MonadFix m, Num r)
                   => Dynamic t (Point 2 r)
                   -> Event t (Transformation 2 r, a)
                   -> m (Event t (Transformation 2 r -> Transformation 2 r))
panEvents mousePos = fmap updated . panDyn mousePos . fmap fst


panDyn          :: forall t m r. (Reflex t, MonadHold t m, MonadFix m, Num r)
                => Dynamic t (Point 2 r)
                   -- ^ mouse position
                -> Event t (Transformation 2 r)
                   -- ^ start or stop, tagged by the current state of the transform.
                -> m (Dynamic t (Transformation 2 r -> Transformation 2 r))
panDyn mousePos = fmap f
                . deltaTransform mousePos
  where
    f :: Dynamic t (Maybe (Dynamic t (Transformation 2 r),Transformation 2 r))
      -> Dynamic t (Transformation 2 r -> Transformation 2 r)
    f = ((\case
             Nothing            -> constDyn id
             Just (deltaDyn,te) -> (\delta _t -> delta |.| te) <$> deltaDyn
         ) =<<)


deltaTransform         :: forall t m r a. (Reflex t, MonadHold t m, MonadFix m, Num r)
                       => Dynamic t (Point 2 r)
                       -- ^ mouse position
                       -> Event t a
                       -- ^ events when to start/stop
                       -> m (Dynamic t (Maybe (Dynamic t (Transformation 2 r),a)))
                       -- ^ the delta transform during the intervals when active
deltaTransform mousePos = fmap f
                        . mousePosAtLastClick mousePos
  where
    f :: Dynamic t (Maybe (Point 2 r,a))
      -> Dynamic t (Maybe (Dynamic t (Transformation 2 r),a))
    f = fmap (fmap (first g))

    g   :: Point 2 r -> Dynamic t (Transformation 2 r)
    g s = fmap (\p -> Transformation.translation $ p .-. s) mousePos


mousePosAtLastClick          :: (Reflex t, MonadHold t m, MonadFix m)
                             => Dynamic t p
                             -> Event t a
                             -> m (Dynamic t (Maybe (p,a)))
mousePosAtLastClick mousePos = foldDyn (\p -> \case
                                           Nothing -> Just p
                                           Just _  -> Nothing
                                       ) Nothing
                             . attach (current mousePos)

--------------------------------------------------------------------------------
-- * Zoomable viewport

-- | Zoomable viewport
zoomableViewportDyn       :: forall t m r. (Reflex t, MonadHold t m, MonadFix m, Fractional r, Ord r)
                          => ZoomConfig r -- ^ zoom configuration
                          -> Viewport r -- ^ initial viewport
                          -> Event t r -- ^ deltas
                          -> m (Dynamic t (Viewport r))
zoomableViewportDyn z0 v0 = viewportDynM v0 (zoomEvents z0)


zoomEvents           :: forall t m r. (Reflex t, MonadHold t m, MonadFix m, Fractional r, Ord r)
                     => ZoomConfig r -- ^ initial zoom config
                     -> Event t (Viewport r, r)
                     -> m (Event t (Transformation 2 r -> Transformation 2 r))
zoomEvents z0 events = do
    zDyn <- zoomDyn z0 $ snd <$> events
    pure $ attachWith f (current zDyn) events
  where
    f (_,_,lambda) (vp,_) = \t -> wrtCenter vp (uniformScaling lambda) |.| t

-- | Maintain a zoom config
zoomDyn    :: (Fractional r, Ord r, Reflex t, MonadHold t m, MonadFix m)
           => ZoomConfig r
           -- ^ Initial zoom config
           -> Event t r
           -> m (Dynamic t (ZoomConfig r, r, r))
           -- ^ (current ZoomConfig r, delta zoom-config since last change, change in scaling factor since last change, a value at last event)
zoomDyn z0 = foldDyn (\delta (z,_,_) -> let z' = z&currentLevel %~ (+ delta)
                                            delta' = z'^.currentLevel - z^.currentLevel
                                            lambda = 1 + (delta' / z^.currentLevel)
                                        in (z',delta',lambda)
                     ) (z0, 0, 1)


--------------------------------------------------------------------------------
-- * Pannable and Zoomable viewport

pannableZoomableViewportDyn       :: (Reflex t, MonadHold t m, MonadFix m, Fractional r, Ord r)
                                  => ZoomConfig r -> Viewport r
                                  -> Dynamic t (Point 2 r)
                                  -> Event t (Either a r)
                                  -> m (Dynamic t (Viewport r))
pannableZoomableViewportDyn z0 v0 mousePos inEvts = do
    let (inPEvents, inZEvents) = fanEither inEvts
    rec pEvents <- panEvents mousePos $ attach ((^.worldToHost) <$> current dyn) inPEvents
        zEvents <- zoomEvents z0 $ attach (current dyn) inZEvents
        dyn     <- viewportDyn v0 $ leftmost [pEvents, zEvents]
    pure dyn
