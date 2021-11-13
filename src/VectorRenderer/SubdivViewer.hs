{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.SubdivViewer where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Monad (void)
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.PlanarSubdivision.Draw
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Data.RealNumber.Rational
import           Ipe
import           Ipe.Color
import           Reflex
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


-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double) => m ()
reflexMain = do
               -- collect the points
               dPoints <- foldDyn (:) [] =<< mouseClickEvent
               let dDT = fmap (fmap mkDT . NonEmpty.nonEmpty) dPoints
               -- draw the subdivision
               drawLayer $ ffor dDT $ \case
                 Nothing  -> pure ()
                 Just dt  -> flip ipeOut dt
                           $ drawPlanarSubdivisionWith drawVtx
                                                       drawEdge
                                                       (drawInternalFace dt)
                                                       drawOuterFace

               -- show a point at the mouse pos
               dMousePos <- mousePositionDyn
               let dMousePosDrawing = ffor dMousePos $ \case
                     Nothing         -> pure () -- don't draw anything
                     Just (p :+ dat) -> let color = if null (mouseMotionEventState dat)
                                                    then V4 255 255 0   128
                                                    else V4 0   255 255 128
                                        in colored' point (p :+ color)
               drawLayer dMousePosDrawing

blue = V4 0 0 255 128


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
  withWindow "convex hull" cfg $ \window -> do
    void $ glCreateContext window
    withRenderer window (-1) defaultRenderer $ \renderer -> do
      host $ reflexSdlApp window renderer reflexMain
  quit


--------------------------------------------------------------------------------

-- | Draw vertices using their default representation; disk marks. For
-- the rest we keep their original attributes.
drawVtx                       :: IpeOut' Maybe (VertexId' s, VertexData r v) IpeSymbol r
drawVtx (_vi, VertexData p _) = Just $ defIO p

-- | Draw edges using normal line segments
drawEdge              :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
drawEdge (_d, s :+ _) = Just $ defIO s

-- | Internal faces are filled polygons.
drawInternalFace                 :: PlanarSubdivision s v e f r
                                 -> IpeOut' Maybe (FaceId' s,   SomePolygon v' r :+ f)    Path r
drawInternalFace s (fi, pg :+ _) = Just $ defIO pg ! attr SFill lightcyan

-- | Draw the outer face (in some box)
drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Path r
drawOuterFace (_, pg :+ _) = Just $ defIO pg ! attr SOpacity "10%"
                                             ! attr SFill lightgray
