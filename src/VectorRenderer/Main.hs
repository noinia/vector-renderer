{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns     #-}
module VectorRenderer.Main where



import           Control.Exception (catch)
import           Control.Monad (guard, void)
import           Data.Camera
import           Data.Ext
import           Data.Geometry.Arrangement
import           Data.Geometry.Arrangement.Draw
import           Data.Geometry.Box
import           Data.Geometry.Ipe.Color (IpeColor(..))
import qualified Data.Geometry.Ipe.Color as Ipe
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Graphics.Rendering.Cairo.Canvas (Canvas, (!@))
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Reflex
import           Reflex.SDL2 hiding (point, origin, Vector, Point, Rectangle)
import           SDL.GeometryUtil
import           SDL.Util
import           VectorRenderer.Import
import           VectorRenderer.ReflexSDLRenderer
import           VectorRenderer.RenderCanvas
-- import           VectorRenderer.Project
import qualified VectorRenderer.RenderCanvas as Render
import qualified Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval as HiddenSurfaceRemoval
import           Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval (EdgeSide(..), Tri)

import           Debug.Trace
import           Data.Geometry.Ipe (IpeWriteText(..))

--------------------------------------------------------------------------------
-- myCamera :: Camera Rational
-- myCamera = Camera origin
--                   (Vector3 1 1 0)
--                   (Vector3 0 0 1)
--                   100
--                   20
--                   30
--                   (Vector2 800 600)

type Scene r = [Triangle 3 () r :+ IpeColor r]


mkBottom :: r -> (r,r) -> (r,r) -> c -> [Triangle 3 () r :+ c]
mkBottom z (lx, ly) (rx, ry) c = [ Triangle' (Point3 lx ly z)
                                             (Point3 rx ly z)
                                             (Point3 lx ry z) :+ c
                                 , Triangle' (Point3 lx ry z)
                                             (Point3 rx ry z)
                                             (Point3 rx ly z) :+ c
                                 ]




xSide :: [Triangle 3 p r :+ c] -> [Triangle 3 p r :+ c]
xSide = map (\(t :+ c) -> pmap toX t :+ c)
  where
    toX (Point3 x y z) = Point3 z y x

ySide :: [Triangle 3 p r :+ c] -> [Triangle 3 p r :+ c]
ySide = map (\(t :+ c) -> pmap toY t :+ c)
  where
    toY (Point3 x y z) = Point3 x z y

myT = Triangle' origin (Point3 0 10 0) (Point3 10 10 0)


myScene :: Scene Rational
myScene = [myT :+ Ipe.red
          , Triangle' origin
                      (Point3 0 40 (-10))
                      (Point3 0 0  (-10)) :+ Ipe.blue
          , Triangle' (Point3 0 0 (-50))
                      (Point3 0 40 (-10))
                      (Point3 0 0  (-10)) :+ Ipe.green
          ]
        ++ axes
        -- ++ cube

axes = [ Triangle' origin
                   (Point3 500 0 (-10))
                   (Point3 500 0 0) :+ Ipe.red
       , Triangle' origin
                   (Point3 0 500 (-10))
                   (Point3 0 500 0) :+ Ipe.green
       , Triangle' origin
                   (Point3 0 (-10) 49)
                   (Point3 0 0     49) :+ Ipe.blue
       ]

-- myScene :: Scene Double
-- myScene = [ triangle (Point3 100 100 100)
--                      (Point3 20   10 100)
--                      (Point3 50   0 100) :+ Canvas.red 255
--           , triangle (Point3 70 350 110)
--                      (Point3 300 200 150)
--                      (Point3 0   30  105) :+ Canvas.blue 255
--           , myT :+ Canvas.rgb 200 0 200
--           ] ++ cube

cube = concat [ mkBottom 0 (400,0) (500,100) (Canvas.red 200)
              , mkBottom 100 (400,0) (500,100) (Canvas.blue 255) -- top
              -- , [triangle (Point3 400 0 0)
              --            (Point3 400 100 0)
              --            (Point3 400 100 100) :+ Canvas.green 100]
              , xSide $ mkBottom 400 (0,0) (100,100) (Canvas.green 255 !@ 100) -- front
              , xSide $ mkBottom 500 (0,0) (100,100) (Canvas.green 255 !@ 100) -- back
              -- , ySide $ mkBottom 0   (0,0) (100,100) (Canvas.red 200)
              -- , ySide $ mkBottom 100 (0,0) (100,100) (Canvas.red 255)
              ]

--           -- the triangles below form part of a a cube
--           , triangle (Point3 400 0 10)
--                      (Point3 500 0 10)
--                      (Point3 400 0 110) :+ Canvas.green 255
--           , triangle (Point3 400 0 110)
--                      (Point3 500 0 110)
--                      (Point3 500 0 10) :+ Canvas.green 255

--           , triangle (Point3 400 100 10)
--                      (Point3 500 100 10)
--                      (Point3 400 100 110) :+ Canvas.green 255
--           , triangle (Point3 400 100 110)
--                      (Point3 500 100 110)
--                      (Point3 500 100 10) :+ Canvas.green 255





--           , triangle (Point3 400 0   10)
--                      (Point3 400 100 10)
--                      (Point3 400 100 110) :+ Canvas.blue 255
--           , triangle (Point3 400 0   110)
--                      (Point3 400 100 110)
--                      (Point3 400 100 10) :+ Canvas.blue 255
--           ]

--------------------------------------------------------------------------------

-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double)
           => m ()
reflexMain = do
               -- collect the points

               -- dPoints <- foldDyn (:) [] =<< mouseClickEvent
               -- let dHull = fmap (fmap convexHull . NonEmpty.nonEmpty) dPoints


               -- -- draw the hull
               -- drawLayer $ ffor dHull $ \case
               --   Nothing -> pure ()
               --   Just h  -> colored' (polygon . _simplePolygon) (h :+ blue)


               -- draw the points
               -- drawLayer $ fmap (mapM_ (point . _core) . reverse) dPoints

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
                         , windowInitialSize     = V2 1024 768
                         }
  withWindow "Vector Renderer" cfg $ \window -> do
    void $ glCreateContext window
    withRenderer window (-1) defaultRenderer $ \renderer -> do
      host $ reflexSdlApp window renderer reflexMain
  quit






--------------------------------------------------------------------------------


shiftCamera     :: Num r => ArrowKey -> Camera r -> Camera r
shiftCamera k c = c&cameraPosition %~ (.+^ 2 *^ toDirection k)


drawScene         :: (Fractional r, Real r, RealFrac r
                     , Show r, IpeWriteText r, Read r
                     )
                  => Double -> Double
                  -> Camera r
                  -> Scene r
                  -> Canvas ()
drawScene w h c s = do
          Canvas.background $ Canvas.gray 255
          Canvas.scale     $ V2 1 (-1)
          Canvas.translate $ V2 (w/2) (-1*h/2)
          let Vector2 cw ch = realToFrac <$> c^.screenDimensions
          Canvas.scale     $ V2 (w/cw) (h/ch)
          Canvas.stroke $ Canvas.gray 0
          -- mapM_ (Render.colored Render.triangle . project c) s
          -- mapM_ (Render.colored Render.triangle) $ renderScene c s
          let arr' = renderAll c s
              arr = traceShow ("woei!",arr') arr'
          Render.ipeOut drawColoredArrangement $ arr



render (Triangle p q r) = Triangle (p&core %~ projectPoint)
                                   (q&core %~ projectPoint)
                                   (r&core %~ projectPoint)


renderScene     :: Fractional r => Camera r -> Scene r
                -> [Triangle 2 () r :+ IpeColor r]
renderScene c s = over core (render . transformBy t) <$> s
  where
    !t = cameraTransform c

renderScene'     :: Fractional r => Camera r -> Scene r
                 -> [Triangle 2 () r :+ (Triangle 3 () r :+ IpeColor r)]
renderScene' c s = (\t3 -> (render . transformBy t $ t3^.core) :+ t3) <$> s
  where
    !t = cameraTransform c



data Screen = Screen



renderAll      :: forall r. (Ord r, Fractional r, RealFrac r
                  , Show r, IpeWriteText r, Read r
                  )
               => Camera r -> Scene r
               -> Arrangement Screen
                              (NonEmpty.NonEmpty (Tri () () (IpeColor r) r))
                              ()
                              (Maybe EdgeSide)
                              (Maybe (IpeColor r))
                              r
renderAll c s = arr&subdivision.faceData %~ fmap mkColor
  where
    arr = HiddenSurfaceRemoval.render (Identity Screen) (c^.cameraPosition) rect ts
    ts = renderScene' c s
    -- ts = HiddenSurfaceRemoval.scene
    mkColor = fmap (^.extra.extra)
    rect :: Rectangle () r
    rect = box (ext origin) (ext . Point $ c^.screenDimensions)






-- | Mirror the canvas s.t. the bottom-left corner is the origin
mirrored     :: Double -> Canvas () -> Canvas ()
mirrored h d = do Canvas.scale     $ V2 1 (-1)
                  Canvas.translate $ V2 0 (-1*h)
                  d

centerScaleViewport         :: Real r
                            => Camera r -> Double -> Double -> Canvas () -> Canvas ()
centerScaleViewport c w h d = do let Vector2 cw ch = realToFrac <$> c^.screenDimensions
                                 Canvas.scale     $ V2 (w/cw) (h/ch)
                                 Canvas.translate $ V2 (w/2)  (h/2)
                                 d


data ArrowKey = UpKey | DownKey | LeftKey | RightKey deriving (Show,Read,Eq,Bounded,Enum)

toArrowKey         :: T.Text -> Maybe ArrowKey
toArrowKey "Up"    = Just UpKey
toArrowKey "Down"  = Just DownKey
toArrowKey "Left"  = Just LeftKey
toArrowKey "Right" = Just RightKey
toArrowKey _       = Nothing


toDirection          :: Num r => ArrowKey -> Vector 3 r
toDirection UpKey    = Vector3 0    1    0
toDirection DownKey  = Vector3 0    (-1) 0
toDirection LeftKey  = Vector3 (-1) 0    0
toDirection RightKey = Vector3 1    0    0


-- toDir       :: Num r => Camera r -> T.Text -> Maybe (Vector 3 r)
-- toDir c "w" = Just $ c^.cameraNormal
-- toDir c "a" = Just $ c^.
