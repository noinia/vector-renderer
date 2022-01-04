{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.GeomViewer where

import           Cairo.Canvas.Ipe
import           Cairo.Canvas.Primitives
import           Control.Lens
import           Control.Lens.Extras (is)
import           Control.Monad (void)
import           Control.Monad.Fix
import           Data.Default
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Boundary
import           Data.Geometry.Box
import           Data.Geometry.Line (Line)
import qualified Data.Geometry.Line as Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.Polygon
import qualified Data.Geometry.Polygon as Polygon
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.LSeq as LSeq
import           Data.List.Util (minimum1By)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Ord (comparing)
import           Data.RealNumber.Rational
import           Data.Tuple
import           Debug.Trace
import           GeomViewer.Mode (ModeAction(..), PartialPolyLine(..), _PartialPolyLine, PartialPolygon(..), _MultiplePoints)
import qualified GeomViewer.Mode as Mode
import           GeomViewer.Viewport
import           GeomViewer.Viewport.Dynamic.SDL
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import           Ipe
import           Ipe.Color
import           Prelude hiding (filter)
import           Reflex
import           Reflex.SDL2 hiding (point, origin, Rectangle, Point)
import           SDL.GeometryUtil
import           SDL.Util
import           System.Random
import           VectorRenderer.Button
import           VectorRenderer.ReflexSDLRenderer
import           VectorRenderer.RenderCanvas
import           Witherable

--------------------------------------------------------------------------------

type R = RealNumber 5

--------------------------------------------------------------------------------

randomPoint :: IO (Point 2 R :+ ())
randomPoint = (\x y -> ext . fmap realToFrac $ Point2 x y)
              <$> randomRIO @Int (0,300)
              <*> randomRIO (0,300)

--------------------------------------------------------------------------------

myViewport :: Viewport Double
myViewport = mkViewport (box (ext $ Point2 100 10) (ext $ Point2 1240 810))
                        Data.Geometry.Transformation.identity


-- mkModelDyn :: m (Dynamic t (Model Geoms r))
-- mkModelDyn = holdDyn mempty


mkMode :: forall s r m t. (Reflex t, MonadHold t m, MonadFix m)
       => Mode.Mode -> Event t ModeAction -> m (Dynamic t Mode.Mode)
mkMode = foldDyn (\(UpdateMode m) _ -> m)


buttons :: (ReflexSDL2Renderer t m r, RealFrac r)
        => m (Map String (Event t (ButtonState, Mode.Mode)))
buttons = sequence
        $ Map.fromList [ mkButton "select"   (Point2 10 700) $ Mode.SelectMode
                       , mkButton "point"    (Point2 10 600) $ Mode.PointMode
                       , mkButton "polyline" (Point2 10 500) $ Mode.PolyLineMode
                       , mkButton "polygon"  (Point2 10 400) $ Mode.PolygonMode
                       ]
  where
    mkButton l p x = (l, fmap (,x) <$> button (box (ext p) (ext $ p .+^ size')) l)
    size' = Vector2 80 20

--------------------------------------------------------------------------------




-- pointMode             :: Event (Point 2 r :+ a) -> m ()
-- pointMode mouseClicks = do
--   dPoints <- foldDyn (:) [] mouseClicks


-- fanByMode :: Behavior t (Mode s r) -> Event t a -> Map () (Event)

inViewport             :: (Ord r, Fractional r)
                       => Viewport r -> Point 2 r :+ a -> Maybe (Point 2 r :+ a)
inViewport vp (p :+ e)
  | p `intersects` (vp^.viewPort) = Just $ toWorldIn vp p :+ e
  | otherwise                     = Nothing


isLeftClick :: (core :+ MouseButtonEventData) -> Bool
isLeftClick  = isClick ButtonLeft

isRightClick :: (core :+ MouseButtonEventData) -> Bool
isRightClick = isClick ButtonRight

isClick     :: MouseButton -> (core :+ MouseButtonEventData) -> Bool
isClick b e = e^.extra.to mouseButtonEventButton == b
           && e^.extra.to mouseButtonEventMotion == Released



--------------------------------------------------------------------------------
-- move these instances into HGeometry

class HasSquaredEuclideanDistance g where
  -- | Given a point q and a geometry g, the squared Euclidean distance between q and g.
  squaredEuclideanDistTo   :: (Num (NumType g), Arity (Dimension g))
                           => Point (Dimension g) (NumType g) -> g -> NumType g
  squaredEuclideanDistTo q = snd . pointClosestToWithDistance q

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance.
  pointClosestTo   :: (Num (NumType g), Arity (Dimension g))
                   => Point (Dimension g) (NumType g) -> g
                   -> Point (Dimension g) (NumType g)
  pointClosestTo q = fst . pointClosestToWithDistance q

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance. Returns both the point and the
  -- distance realized by this point.
  pointClosestToWithDistance     :: (Num (NumType g), Arity (Dimension g))
                                 => Point (Dimension g) (NumType g) -> g
                                 -> (Point (Dimension g) (NumType g), NumType g)
  pointClosestToWithDistance q g = let p = pointClosestTo q g
                                   in (p, squaredEuclideanDist p q)
  {-# MINIMAL pointClosestToWithDistance | pointClosestTo #-}

instance (Num r, Arity d) => HasSquaredEuclideanDistance (Point d r) where
  pointClosestTo _ p = p

instance (Fractional r, Arity d) => HasSquaredEuclideanDistance (Line d r) where
  pointClosestTo = Line.pointClosestTo

instance (Num r, Ord r) => HasSquaredEuclideanDistance (Box 2 p r) where
  pointClosestToWithDistance q bx =
      case ((q^.xCoord) `inRange` hor, (q^.yCoord) `inRange` ver) of
                      (False,False) -> if q^.yCoord < b
                                       then closest (Point2 l b) (Point2 r b)
                                       else closest (Point2 l t) (Point2 r t)
                      (True, False) -> if q^.yCoord < b
                                       then (q&yCoord .~ b, sq $ q^.yCoord - b)
                                       else (q&yCoord .~ t, sq $ q^.yCoord - t)
                      (False, True) -> if q^.xCoord < l
                                       then (q&yCoord .~ l, sq $ q^.xCoord - l)
                                       else (q&yCoord .~ r, sq $ q^.xCoord - r)
                      (True, True)  -> (q, 0) -- point lies inside the box
    where
      Vector2 hor@(Range' l r) ver@(Range' b t) = extent bx
      sq x = x*x
      closest p1 p2 = let d1 = squaredEuclideanDist q p1
                          d2 = squaredEuclideanDist q p2
                      in if d1 < d2 then (p1, d1) else (p2, d2)

instance (Fractional r, Arity d, Ord r) => HasSquaredEuclideanDistance (LineSegment d p r) where
  pointClosestToWithDistance q = swap . sqDistanceToSegArg q

instance (Fractional r, Arity d, Ord r) => HasSquaredEuclideanDistance (PolyLine d p r) where
  pointClosestToWithDistance q = minimumBy (comparing snd)
                               . fmap (pointClosestToWithDistance q)
                               . edgeSegments

instance (Fractional r, Ord r) => HasSquaredEuclideanDistance (Polygon t p r) where
  pointClosestToWithDistance q pg
    | q `intersects` pg = (q, 0)
    | otherwise         = pointClosestToWithDistance q (Boundary pg)

instance (Fractional r, Ord r) => HasSquaredEuclideanDistance (Boundary (Polygon t p r)) where
  pointClosestToWithDistance q = minimumBy (comparing snd)
                               . fmap (pointClosestToWithDistance q)
                               . listEdges . review _Boundary


--------------------------------------------------------------------------------

type SelectModeState r = Maybe (Point 2 r)
  -- todo generalize

type Selection r  = Point 2 r

dSelected            :: ( Reflex t, MonadHold t m, MonadFix m, Ord r, Num r
                        , HasSquaredEuclideanDistance s, NumType s ~ r, Dimension s ~ 2
                        )
                     => Dynamic t [s]
                     -> Event t (Selection r)
                     -> m (Dynamic t (Maybe s))
dSelected candidates = foldDyn f Nothing . attach (current candidates)
  where
    f (currentCandidates, q) _ = closestTo q currentCandidates
    closestTo q = minimum1By (comparing $ squaredEuclideanDistTo q)


selectMode :: ( Reflex t, MonadHold t m, MonadFix m, Ord r, Num r
                        , HasSquaredEuclideanDistance s, NumType s ~ r, Dimension s ~ 2
                        )
                     => Dynamic t [s]
                     -> Event t (Selection r)
                     -> Dynamic t Mode.Mode
                     -> m (Dynamic t (Maybe s))
selectMode candidates leftClicks dMode =
  dSelected candidates $ gate (is Mode._SelectMode <$> current dMode) leftClicks

--------------------------------------------------------------------------------

data ContinueOrFinish a = Continue a | Finish a
                        deriving (Show,Eq)

type PolyLineModeState r = Maybe (PartialPolyLine r)


-- import Snoc (LSeq (n) a) (LSeq n a)
polyLineMode                             :: (Reflex t, MonadHold t m, MonadFix m, Show r)
                                         => Event t (Point 2 r)
                                         -> Event t (Point 2 r)
                                         -> Dynamic t Mode.Mode
                                         -> m (Dynamic t (PolyLineModeState r, [PolyLine 2 () r]))
polyLineMode leftClicks rightClicks dMode = foldDyn f (Nothing, []) evts
  where
    inMode = gate (is Mode._PolyLineMode <$> current dMode)
    evts = leftmost [ traceShowId . Continue <$> inMode leftClicks
                    , Finish   <$> inMode rightClicks
                    ]

    f evt (current', completed) = case evt of
         Continue p -> (Just $ extend p current', completed)
         Finish p   -> (Nothing, (extend p current' ^.._PartialPolyLine) <> completed )


extend   :: Point 2 r -> PolyLineModeState r -> PartialPolyLine r
extend p = \case
  Nothing                   -> StartPoint p
  Just (StartPoint s)       -> PartialPolyLine . PolyLine.fromPointsUnsafe . map ext $ [s,p]
  Just (PartialPolyLine pl) -> PartialPolyLine $ pl&points %~ (LSeq.promise . (LSeq.|> ext p))


--------------------------------------------------------------------------------

type PolygonModeState r = Maybe (PartialPolygon r)

polygonMode                              :: (Reflex t, MonadHold t m, MonadFix m, Ord r, Fractional r)
                                         => Event t (Point 2 r)
                                         -> Event t (Point 2 r)
                                         -> Dynamic t Mode.Mode
                                         -> m (Dynamic t (PolygonModeState r, [SimplePolygon () r]))
polygonMode leftClicks rightClicks dMode = foldDyn f (Nothing, []) evts
  where
    inMode = gate (is Mode._PolygonMode <$> current dMode)
    evts = leftmost [ Continue <$> inMode leftClicks
                    , Finish   <$> inMode rightClicks
                    ]

    f evt (current', completed) = case evt of
         Continue p -> (Just $ extendPolygon p current', completed)
         Finish p   -> (Nothing, maybeToList (finishPolygon p current') <> completed)

finishPolygon     :: (Ord r, Fractional r)
                  => Point 2 r -> PolygonModeState r -> Maybe (SimplePolygon () r)
finishPolygon p s = polyLineToPolygon
                  <$> extendPolygon p s ^?_MultiplePoints

polyLineToPolygon :: (Ord r, Fractional r) => PolyLine 2 p r -> SimplePolygon p r
polyLineToPolygon = Polygon.toCounterClockWiseOrder
                 .  Polygon.unsafeFromPoints . (^.points.to toList)

extendPolygon   :: Point 2 r -> PolygonModeState r -> PartialPolygon r
extendPolygon p = \case
  Nothing -> SinglePoint p
  Just (SinglePoint s)     -> TwoPoints s p
  Just (TwoPoints a b)     -> MultiplePoints $ PolyLine.fromPointsUnsafe . map ext $ [a,b,p]
  Just (MultiplePoints pl) -> MultiplePoints $ pl&points %~ (LSeq.promise . (LSeq.|> ext p))

--------------------------------------------------------------------------------

-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double) => m ()
reflexMain = do
               drawLayer . pure $ ipeObject . iO $ defIO (Point2 0 0) ! attr SStroke black

               dViewport <- pannableZoomableViewportDyn def myViewport

               buttonsE <- buttons
               let onUp a = \case
                     ButtonStateDown -> Just a
                     _               -> Nothing
                   modeEvents = leftmost . Map.elems . ffor buttonsE $
                                  \events -> mapMaybe (\(s,m') -> onUp (UpdateMode m') s) events

               dMode <- mkMode @() Mode.SelectMode modeEvents
               performEvent_ $ ffor (updated dMode) (liftIO . print)

               mouseClicks <- mouseClickEvent


               let leftClicks  = filter isLeftClick  mouseClicks
                   rightClicks = filter isRightClick mouseClicks
                   viewportClicks = attachWithMaybe inViewport (current dViewport)
                   leftViewportClicks = viewportClicks leftClicks
                   rightViewportClicks = viewportClicks rightClicks

               -- performEvent_ $ ffor mouseClicks (liftIO . putStrLn . ("clicked " <> ). show)
               -- performEvent_ $ ffor mouseClicks (liftIO . putStrLn . ("left " <> ) . show)


               dMousePos <- mousePositionDyn
               let dMousePosInViewport = zipDynWith (\vp -> (inViewport vp =<<)) dViewport dMousePos
               ----------------------------------------
               -- * Points
               dPoints <- foldDyn (:) []
                         $ gate (is Mode._PointMode <$> current dMode) leftViewportClicks

               ----------------------------------------
               -- * PolyLines
               dPolyLineModeState  <- polyLineMode (view core <$> leftViewportClicks)
                                                   (view core <$> rightViewportClicks)
                                                   dMode

               let dPolylines       = snd <$> dPolyLineModeState
                   dCurrentPolyLine = fst <$> dPolyLineModeState

               performEvent_ $ ffor (updated dCurrentPolyLine ) (liftIO . print)


               ----------------------------------------
               -- * Polygons

               dPolygonModeState  <- polygonMode (view core <$> leftViewportClicks)
                                                 (view core <$> rightViewportClicks)
                                                 dMode

               let dPolygons       = snd <$> dPolygonModeState
                   dCurrentPolygon = fst <$> dPolygonModeState


               ----------------------------------------
               -- * Selection
               dSelection <- selectMode (fmap (view core) <$> dPoints)
                                        (view core <$> leftViewportClicks)
                                        dMode

               performEvent_ $ ffor (updated dSelection ) (liftIO . print)

               ----------------------------------------
               -- * Draw Everything in the Viewport

               let dDrawAll = drawAll
                              [ -- points
                                fmap (mapM_ (point . _core) . reverse) dPoints
                              -- polylines
                              , fmap (mapM_ polyLine . reverse) dPolylines
                              , zipDynWith drawPartialPolyLine dCurrentPolyLine dMousePosInViewport
                              -- polygons
                              , fmap (mapM_ polygon . reverse) dPolygons
                              , zipDynWith drawPartialPolygon dCurrentPolygon dMousePosInViewport
                              ]
               let drawStuff = do
                     ipeObject . iO $ defIO (traceShowId $ Point2 0 0) ! attr SStroke blue
                     -- ipeObject . iO $ defIO (Point2 20 0)
                     void $ textAt origin "foo"

               drawLayer $ flip drawInViewport drawStuff <$> dViewport
               drawLayer $ zipDynWith drawInViewport dViewport dDrawAll

               -- show a point at the mouse pos
               drawLayer $ drawCurrentMousePosition dMousePos

drawAll :: Reflex t => [Dynamic t (Canvas () )] -> Dynamic t (Canvas ())
drawAll = fmap sequence_ . sequenceA


drawPartialPolygon       :: (Real r, Ord r, Fractional r)
                         => PolygonModeState r -> Maybe (Point 2 r :+ a) -> Canvas ()
drawPartialPolygon state = \case
  Nothing       -> pure ()
  Just (p :+ _) -> case extendPolygon p state of
                     SinglePoint _     -> pure ()
                     TwoPoints a b     -> lineSegment $ ClosedLineSegment (ext a) (ext b)
                     MultiplePoints pl -> polygon $ polyLineToPolygon pl


drawPartialPolyLine       :: (Real r, Show r) => PolyLineModeState r -> Maybe (Point 2 r :+ a) -> Canvas ()
drawPartialPolyLine state = \case
  Nothing       -> pure ()
  Just (p :+ _) -> case extend p state ^?_PartialPolyLine of
                     Nothing -> pure ()
                     Just pl -> polyLine $ traceShowId pl


--------------------------------------------------------------------------------

-- | Draws the content in the viewport
drawViewport          :: RealFrac r => Viewport r -> Canvas ()
drawViewport viewport = ipeObject . iO $ defIO (viewport^.viewPort) ! attr SStroke black

-- | Draws the content in the viewport
drawInViewport              :: RealFrac r => Viewport r -> Canvas () -> Canvas ()
drawInViewport viewport act = do drawViewport viewport
                                 renderInViewport viewport act

--------------------------------------------------------------------------------


drawCurrentMousePosition           :: (Reflex t, Real r)
                                   => Dynamic t (Maybe (Point 2 r :+ MouseMotionEventData))
                                   -> Dynamic t (Canvas ())
drawCurrentMousePosition dMousePos = ffor dMousePos $ \case
      Nothing         -> pure () -- don't draw anything
      Just (p :+ dat) -> let color = if null (mouseMotionEventState dat)
                                     then black
                                     else blue
                         in colored point (p :+ color)




--------------------------------------------------------------------------------

main :: IO ()
main = do
  initializeAll
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowGraphicsContext = OpenGLContext ogl
                         , windowResizable       = True
                         -- , windowHighDPI         = False
                         , windowInitialSize     = V2 1440 810
                         }
  withWindow "convex hull" cfg $ \window -> do
    void $ glCreateContext window
    withRenderer window (-1) defaultRenderer $ \renderer -> do
      host $ reflexSdlApp window renderer reflexMain
  quit
