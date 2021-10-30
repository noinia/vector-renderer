{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.Main where


import Control.Monad (guard, void)
import Data.Ext
import Reflex
import Reflex.SDL2 hiding (point, Rectangle)
import SDL.GeometryUtil
import SDL.Util
import VectorRenderer.ReflexSDLRenderer
import VectorRenderer.RenderCanvas
import qualified Data.List.NonEmpty as NonEmpty
import Algorithms.Geometry.ConvexHull.GrahamScan
import           Data.Geometry.Box

import Data.Geometry.Polygon.Convex
import Data.Geometry.Point
import Data.Intersection

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2 t m, DynamicWriter t [Layer] m)
           => m ()
reflexMain = do

               -- ------------------------------------------------------------------------------
               -- -- A button!
               -- ------------------------------------------------------------------------------
               evBtnState <- button
               let evBtnPressed = fmapMaybe (guard . (== ButtonStateDown)) evBtnState
               performEvent_ $ ffor evBtnPressed $ const $ liftIO $ putStrLn "Button pressed!"


               -- collect the points

               dPoints <- foldDyn (:) [] =<< mouseClickEvent
               let dHull = fmap (fmap convexHull . NonEmpty.nonEmpty) dPoints


               -- draw the hull
               drawLayer $ ffor dHull $ \case
                 Nothing -> pure ()
                 Just h  -> colored' (polygon . _simplePolygon) (h :+ blue)


               -- draw the points
               drawLayer $ fmap (mapM_ (point . _core) . reverse) dPoints

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
      host $ reflexSdlApp window renderer False reflexMain
  quit
