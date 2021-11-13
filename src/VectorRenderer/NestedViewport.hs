{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.NestedViewport where


import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Monad (guard, void, replicateM)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex
import           Data.Intersection
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Ipe.Color
import           Reflex
import           Reflex.SDL2 hiding (point, Rectangle, Point)
import           SDL.GeometryUtil
import           SDL.Util
import           System.Random
import           VectorRenderer.ReflexSDLRenderer
import           VectorRenderer.RenderCanvas
--------------------------------------------------------------------------------

type R = RealNumber 5

--------------------------------------------------------------------------------

randomPoint :: IO (Point 2 R :+ ())
randomPoint = (\x y -> ext . fmap realToFrac $ Point2 x y) <$> randomIO @Int <*> randomIO


-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double) => m ()
reflexMain = do
               pts <- liftIO $ replicateM 10 randomPoint

               -- collect the points

               -- dPoints <- foldDyn (:) [] =<< mouseClickEvent
               let dPoints = constDyn pts
               let dHull = fmap (fmap convexHull . NonEmpty.nonEmpty) dPoints


               -- draw the hull
               drawLayer $ ffor dHull $ \case
                 Nothing -> pure ()
                 Just h  -> colored (polygon . _simplePolygon) (h :+ blue)


               -- draw the points
               drawLayer $ fmap (mapM_ (point . _core) . reverse) dPoints

               -- show a point at the mouse pos
               dMousePos <- mousePositionDyn
               let dMousePosDrawing = ffor dMousePos $ \case
                     Nothing         -> pure () -- don't draw anything
                     Just (p :+ dat) -> let color = if null (mouseMotionEventState dat)
                                                    then black
                                                    else blue
                                        in colored point (p :+ color)
               drawLayer dMousePosDrawing


--------------------------------------------------------------------------------




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
