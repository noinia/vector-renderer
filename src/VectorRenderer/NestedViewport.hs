{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.NestedViewport where


import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Lens
import           Control.Monad (guard, void, replicateM)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point

import           Data.Geometry.Transformation
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import           Ipe.Color
import           Ipe
import           Reflex
import           Reflex.SDL2 hiding (point, Rectangle, Point)
import           SDL.GeometryUtil
import           SDL.Util
import           System.Random
import           VectorRenderer.ReflexSDLRenderer
import           VectorRenderer.RenderCanvas
import           VectorRenderer.Viewport

--------------------------------------------------------------------------------

type R = RealNumber 5

--------------------------------------------------------------------------------

randomPoint :: IO (Point 2 R :+ ())
randomPoint = (\x y -> ext . fmap realToFrac $ Point2 x y)
              <$> randomRIO @Int (0,300)
              <*> randomRIO (0,300)


-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double) => m ()
reflexMain = do
               pts <- liftIO $ replicateM 10 randomPoint

               -- collect the points

               -- dPoints <- foldDyn (:) [] =<< mouseClickEvent
               let dPoints = constDyn pts
               let dHull = fmap (fmap convexHull . NonEmpty.nonEmpty) dPoints

               -- -- draw the hull
               -- drawLayer $ ffor dHull $ \case
               --   Nothing -> pure ()
               --   Just h  -> colored (polygon . _simplePolygon) (h :+ blue)

               -- draw the points
               drawLayer $ fmap (mapM_ (point . _core) . reverse) dPoints

               let r = box (ext $ Point2 10 10) (ext $ Point2 200 300)
               -- drawLayer . pure $ ipeObject . iO $ defIO r ! attr SFill red

               drawLayer . pure . drawInViewport myViewport $
                 ipeObject . iO $ defIO r ! attr SFill red

               -- show a point at the mouse pos
               dMousePos <- mousePositionDyn
               let dMousePosDrawing = ffor dMousePos $ \case
                     Nothing         -> pure () -- don't draw anything
                     Just (p :+ dat) -> let color = if null (mouseMotionEventState dat)
                                                    then black
                                                    else blue
                                        in colored point (p :+ color)
               drawLayer dMousePosDrawing



-- | Viewport in which everything is rotated 90 deg.
myViewport :: Viewport Double
myViewport = Viewport (box (ext $ Point2 100 100) (ext $ Point2 500 500))
                      (rotation 90)

-- | Draws the content in the viewport
drawViewport          :: RealFrac r => Viewport r -> Canvas ()
drawViewport viewport = ipeObject . iO $ defIO (viewport^.viewPort) ! attr SStroke black

-- | Draws the content in the viewport
drawInViewport              :: RealFrac r => Viewport r -> Canvas () -> Canvas ()
drawInViewport viewport act = do drawViewport viewport
                                 renderInViewport viewport act


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
