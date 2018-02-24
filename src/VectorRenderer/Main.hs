{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module VectorRenderer.Main where

import           Control.Exception (catch)
import           Data.Camera
import           Data.GI.Base
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Geometry.Triangle
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.Rendering.Cairo.Canvas (Canvas, (!@))
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Linear.V2 (V2(..))
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk
import           VectorRenderer.Import
import           VectorRenderer.Project
import           VectorRenderer.RenderUtil
import qualified VectorRenderer.RenderCanvas as Render


--------------------------------------------------------------------------------

myCamera :: Camera Rational
myCamera = Camera (Point3 0 (-100) (-100)) (Vector3 0 0 1)

type Scene r = [Triangle 3 () r :+ Canvas.Color]


triangle       :: Point 3 r -> Point 3 r -> Point 3 r -> Triangle 3 () r
triangle p q r = Triangle (ext p) (ext q) (ext r)


mkBottom :: r -> (r,r) -> (r,r) -> Canvas.Color -> [Triangle 3 () r :+ Canvas.Color]
mkBottom z (lx, ly) (rx, ry) c = [ triangle (Point3 lx ly z)
                                            (Point3 rx ly z)
                                            (Point3 lx ry z) :+ c
                                 , triangle (Point3 lx ry z)
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


myScene :: Scene Rational
myScene = [ triangle (Point3 100 100 100)
                     (Point3 20   10 100)
                     (Point3 50   0 100) :+ Canvas.red 255
          , triangle (Point3 70 350 110)
                     (Point3 300 200 150)
                     (Point3 0   30  105) :+ Canvas.blue 255
          ] ++ cube

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
-- --------------------------------------------------------------------------------

main :: IO ()
main = runGtk `catch` (\(e::Gtk.GError) -> Gtk.gerrorMessage e >>= putStrLn . T.unpack)
  where
    runGtk = do
      _ <- Gtk.init Nothing
      compile networkDescription >>= actuate
      Gtk.main

networkDescription :: MomentIO ()
networkDescription = do
    b <- Gtk.builderNew
    _ <- Gtk.builderAddFromFile b "gui/canvas.ui"

    window   <- castB b "window" Gtk.Window
    destroyE <- signalE0 window #destroy
    reactimate $ Gtk.mainQuit <$ destroyE

    -- quitMenuItem <- castB b "quitMenuItem" Gtk.ImageMenuItem
    -- quitClickE <- signalE0 quitMenuItem #activate
    -- reactimate $ Gtk.mainQuit <$ quitClickE

    drawingArea   <- castB b "canvas" Gtk.DrawingArea
    drawingAreaH <- realToFrac . fromIntegral . snd <$> #getPreferredHeight drawingArea
    drawingAreaW <- realToFrac . fromIntegral . snd <$> #getPreferredWidth  drawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  , Gdk.EventMaskSmoothScrollMask
                                                  , Gdk.EventMaskKeyPressMask
                                                  ])

    -- scroll Events
    scrollE <- signalE1' drawingArea #scrollEvent $ \e ->
                     Gdk.getEventScrollDeltaY e

    -- events when we press a key
    keyPressedE <- signalE1' drawingArea #keyPressEvent $ \e -> do
                     v  <- Gdk.getEventKeyKeyval e
                     Gdk.keyvalName v
    -- events where we press an arrow key
    let arrowKeyE = filterJust . fmap (>>= toArrowKey) $ keyPressedE

    -- handle mouse clicks
    mousePressedE <- signalE1' drawingArea #buttonPressEvent $ \e -> do
                      x <- Gdk.getEventButtonX e
                      y <- Gdk.getEventButtonY e
                      return $! V2 x ((-1*y) + drawingAreaH)

    lastMousePressB <- stepper undefined mousePressedE

    cameraB <- accumB myCamera (shiftCamera <$> arrowKeyE)
    let sceneB  = pure myScene
        canvasB = drawScene <$> cameraB <*> sceneB

    draw drawingArea (mirrored drawingAreaH <$> canvasB)

    reactimate $ print <$> mousePressedE

    #showAll window


shiftCamera     :: Num r => ArrowKey -> Camera r -> Camera r
shiftCamera k c = c&cameraPosition %~ (.+^ 2 *^ toDirection k)


drawScene       :: Camera Rational
                -> Scene Rational
                -> Canvas ()
drawScene c s = do
        Canvas.background $ Canvas.gray 255
        Canvas.stroke $ Canvas.gray 0
        mapM_ (Render.colored Render.triangle . project c) s


-- -- | Mirror the canvas s.t. the bottom-left corner is the origin
-- mirrored       :: Double -> (a -> Canvas ()) -> a -> Canvas ()
-- mirrored h d x = do Canvas.scale     $ V2 1 (-1)
--                     Canvas.translate $ V2 0 (-1*h)
--                     d x


-- | Mirror the canvas s.t. the bottom-left corner is the origin
mirrored     :: Double -> Canvas () -> Canvas ()
mirrored h d = do Canvas.scale     $ V2 1 (-1)
                  Canvas.translate $ V2 0 (-1*h)
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
