{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.NestedViewport where

import Cairo.Canvas.Ipe
import Cairo.Canvas.Primitives
import Control.Lens
import Control.Monad (void)
import Data.Default
import Data.Ext
import Data.Geometry.Box
import Data.Geometry.Point
import Data.Geometry.Transformation
import Data.RealNumber.Rational
import Debug.Trace
import Graphics.Rendering.Cairo.Canvas (Canvas)
import Ipe
import Ipe.Color
import Reflex
import Reflex.SDL2 hiding (point, origin, Rectangle, Point)
import SDL.GeometryUtil
import SDL.Util
import System.Random
import UI.Layout
import UI.Viewport
import VectorRenderer.PannableViewport
import VectorRenderer.ReflexSDLRenderer
import VectorRenderer.RenderCanvas

--------------------------------------------------------------------------------

type R = RealNumber 5

--------------------------------------------------------------------------------

randomPoint :: IO (Point 2 R :+ ())
randomPoint = (\x y -> ext . fmap realToFrac $ Point2 x y)
              <$> randomRIO @Int (0,300)
              <*> randomRIO (0,300)

----------------------------------------
myLayout :: Layout R (IpeColor R)
myLayout = Rows red [ Spacing 0, Padding 0
                    ]
                    [ Child 1  menuBar
                    , Child 24 mainWindow
                    ]
  where
    menuBar     = Full blue []
    mainWindow  = Columns yellow [ Spacing 0, Padding 0]
                                 [ Child 1  toolDrawer
                                 , Child 15 geomView
                                 , Child 7  editPane
                                 ]


    toolDrawer  = Full blue []
    geomView     = Full white []
    editPane     = Rows white [] [ Child 10 editStuff
                                 , Child 2  navToolsAndStatus
                                 ]
    editStuff = Full yellow []
    navToolsAndStatus = Columns green [] [ Child 3 miniMap
                                         , Child 3 status
                                         ]
    miniMap = Full orange []
    status = Full red []



-- renderLayout   :: RealFrac r => Rectangle p r -> Layout r (IpeColor r) -> Canvas ()



renderLayout'   :: RealFrac r => Rectangle p r -> Layout r (IpeColor r) -> Canvas ()
renderLayout' r = renderLayout r render
  where
    render = colored rectangle

--------------------------------------------------------------------------------


-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double) => m ()
reflexMain = do
               drawLayer . pure $ ipeObject . iO $ defIO (Point2 0 0) ! attr SStroke black
               let r = box (ext $ Point2 10 10) (ext $ Point2 200 300)
               drawLayer . pure $ ipeObject . iO $ defIO r ! attr SFill red



               drawLayer . pure $ roundedRectangle' (Just red)
                                                    (Just blue)
                                                    20 (box (ext $ Point2 100 700)
                                                            (ext $ Point2 400 800))


               -- dz <- zoomDyn defaultZoomConfig
               -- performEvent_ $ ffor (updated dz) $ \z ->
               --   liftIO $ print z

               dViewport <- pannableZoomableViewportDyn' def myViewport
               -- dViewport <- zoomableViewportDyn' def myViewport


               let drawStuff = do
                     ipeObject . iO $ defIO (traceShowId $ Point2 0 0) ! attr SStroke blue
                     ipeObject . iO $ defIO r ! attr SStroke blue
                     void $ textAt origin "foo"

               drawLayer $ flip drawInViewport drawStuff <$> dViewport

               let screen = box (ext $ Point2 0 0) (ext $ Point2 1440 810)
               drawLayer . pure $ renderLayout' screen myLayout


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
myViewport = mkViewport (box (ext $ Point2 100 100) (ext $ Point2 500 500))
                        Data.Geometry.Transformation.identity -- (rotation $ pi / 2 )

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
                         , windowInitialSize     = V2 1440 810
                         }
  withWindow "convex hull" cfg $ \window -> do
    void $ glCreateContext window
    withRenderer window (-1) defaultRenderer $ \renderer -> do
      host $ reflexSdlApp window renderer reflexMain
  quit
