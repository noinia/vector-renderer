{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.SubdivViewer where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Applicative
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.Fix (MonadFix)
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.PlanarSubdivision.Draw
import           Data.Geometry.Point
import           Data.Geometry.PointLocation
import           Data.Geometry.Polygon
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Data.RealNumber.Rational
import qualified Data.Text as Text
import           Ipe
import           Cairo.Canvas.Primitives
import           Cairo.Canvas.Ipe
import           Ipe.Color
import           Reflex hiding (Group)
import           Reflex.SDL2 hiding (point, Rectangle, Point)
import           SDL.GeometryUtil
import           SDL.Util
import           VectorRenderer.ReflexSDLRenderer
import           VectorRenderer.RenderCanvas



--------------------------------------------------------------------------------

type R = RealNumber 5

data DTWorld

mkDT :: NonEmpty (Point 2 R :+ v) -> PlanarSubdivision DTWorld v () () R
mkDT = toPlanarSubdivision (Proxy @DTWorld) . delaunayTriangulation

mkNonEmpty :: [a] -> Maybe (NonEmpty a)
mkNonEmpty = \case
  pts@(_:_:_:_) -> Just $ NonEmpty.fromList pts
  _             -> Nothing

-- | Main reflex app that can also render layers
reflexMain :: forall t m. (ReflexSDL2Renderer t m Double) => m ()
reflexMain = do
               -- collect the points
               dPoints <- foldDyn (:) [] =<< mouseClickEvent
               let dDT = fmap (fmap mkDT . mkNonEmpty) dPoints
                   dPointLocDs = fmap (fmap pointLocationDS) dDT

               -- keep track of the mouse position
               dMousePos <- mousePositionDyn

               -- keep track of the current face
               dCurrentFace <- filterEqDyn
                 $ zipDynWith (liftA2 (\(q :+ _) -> faceIdContaining q)) dMousePos dPointLocDs


               let  f :: Behavior t (Maybe (FaceId' s) -> Performable m ())
                    f = ffor2 (current dPoints) (current dMousePos) $ \pts p fi ->  do
                              liftIO $ print "========================================"
                              liftIO $ print fi
                              liftIO $ print (pts&traverse.extra .~ ())
                              liftIO $ print (p&_Just.extra .~ ())

               performEvent_ $ f <@> updated dCurrentFace



                 -- (current dPoints <@> updated dCurrentFace)

                 -- ffor (updated dCurrentFace)
                 -- $ \f -> do
                 -- liftIO $ print f
                 -- pts <- sample (current dPoints)
                 -- liftIO $ print pts

               -- performEvent_ $ ffor (updated dCurrentFace) (updated dCurrentFace) $ \f pts ->
               --   do
               --     liftIO $ print f
               --     liftIO $ print pts

               -- performEvent_ $ ffor mouseClickEvent $ \p ->
               --   liftIO $ print p

               -- draw the points
               drawLayer $ fmap (mapM_ (point . _core) . reverse) dPoints

               -- draw the subdivision
               drawLayer $ ffor2 dDT dCurrentFace $ \case
                 Nothing  -> const $ pure ()
                 Just dt  -> \currentFaceId ->
                   flip ipeOut dt
                           $ drawPlanarSubdivisionWith drawVtx
                                                       drawEdge
                                                       (drawInternalFace dt currentFaceId)
                                                       drawOuterFace

               -- show a point at the mouse pos
               let dMousePosDrawing = ffor dMousePos $ \case
                     Nothing         -> pure () -- don't draw anything
                     Just (p :+ dat) -> let color = if null (mouseMotionEventState dat)
                                                    then black
                                                    else red
                                        in colored point (p :+ color)
               drawLayer dMousePosDrawing






-- | Only update the dyn when the value actually changes.
filterEqDyn   :: (Eq a, MonadSample t m, Reflex t, MonadHold t m, MonadFix m)
              => Dynamic t a -> m (Dynamic t a)
filterEqDyn d = do x0 <- sample $ current d
                   foldDynMaybe (\new cur -> if new == cur then Nothing else Just new)
                                x0 (updated d)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  initializeAll
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowGraphicsContext = OpenGLContext ogl
                         , windowResizable       = True
                         -- , windowHighDPI         = False
                         -- , windowInitialSize     = V2 640 480
                         }
  withWindow "Delaunay Triangulation" cfg $ \window -> do
    void $ glCreateContext window
    withRenderer window (-1) defaultRenderer $ \renderer -> do
      host $ reflexSdlApp window renderer reflexMain
  quit


--------------------------------------------------------------------------------

-- | Draw vertices using their default representation; disk marks. For
-- the rest we keep their original attributes.
drawVtx                       :: IpeOut' Maybe (VertexId' s, VertexData r v) IpeSymbol r
drawVtx (_vi, VertexData p _) = Just $ defIO p ! attr SFill black

-- | Draw edges using normal line segments
drawEdge              :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
drawEdge (_d, s :+ _) = Just $ defIO s

-- | Internal faces are filled polygons.
drawInternalFace                   :: (Fractional r)
                                   => PlanarSubdivision s v e f r -> Maybe (FaceId' s)
                                   -> IpeOut' Maybe (FaceId' s,   SomePolygon v' r :+ f)    Group r
drawInternalFace _ ci (fi, pg :+ _) = Just $ Ipe.ipeGroup [ iO $ defIO pg ! attr SFill color
                                                          , iO $ ipeLabel (showT fi :+ center)
                                                          ]
  where
    color | ci == Just fi  = red
          | otherwise      = lightcyan

    center = centroid (pg^?!_Left)

showT :: Show a => a -> Text.Text
showT = Text.pack . show

-- | Draw the outer face (in some box)
drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Group r
drawOuterFace (_, pg :+ _) = Nothing

  -- Just $ defIO pg ! attr SOpacity "10%"
  --                                            ! attr SFill lightgray



--------------------------------------------------------------------------------

-- bugDT = mkDT . NonEmpty.fromList
--     $ [Point2 102 452 :+ (),Point2 102 452 :+ (),Point2 143 219 :+ (),Point2 143 219 :+ (),Point2 614 146 :+ (),Point2 614 146 :+ (),Point2 543 469 :+ (),Point2 543 469 :+ (),Point2 227 319 :+ (),Point2 227 319 :+ ()]
-- bugPS = pointLocationDS bugDT

-- bugQ :: Point 2 R
-- bugQ = Point2 301 408

-- bugF = faceIdContaining bugQ bugPS

-- bugD = flip edgeSegment bugDT <$> dartAbove bugQ bugPS

-- verify = bugQ `intersects` ((^?!core._Left) $ internalFacePolygon bugF bugDT)

-- -- wrongAnswer? = FaceId 3
