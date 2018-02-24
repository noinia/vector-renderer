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
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Linear.V2 (V2(..))
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk
import           VectorRenderer.Import
import           VectorRenderer.Project
import           VectorRenderer.RenderUtil


--------------------------------------------------------------------------------

myCamera :: Camera Rational
myCamera = Camera (Point3 0 0 (-10)) (Vector3 0 0 1)

myScene :: Triangle 3 () Rational
myScene = Triangle (ext $ Point3 10 10 10)
                   (ext $ Point3 2 1 10)
                   (ext $ Point3 5 0 10)

--------------------------------------------------------------------------------

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

    quitMenuItem <- castB b "quitMenuItem" Gtk.ImageMenuItem
    quitClickE <- signalE0 quitMenuItem #activate
    reactimate $ Gtk.mainQuit <$ quitClickE

    openFileMenuItem <- castB b "openFileMenuItem" Gtk.ImageMenuItem
    openFileClickE   <- signalE0 openFileMenuItem #activate

    mouseLabel <- castB b "mouseLabel" Gtk.Label

    colorButton       <- castB b "colorButton" Gtk.Button
    pressedE          <- signalE0 colorButton #clicked
    colorButtonStateB <- accumB False (not <$ pressedE)

    drawingArea   <- castB b "canvas" Gtk.DrawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  ])


    -- canvasE <- drawE drawingArea updateCanvas'

    -- mouseMotionE <- signalE1' drawingArea #motionNotifyEvent $ \e -> do
    --                   x <- Gdk.getEventMotionX e
    --                   y <- Gdk.getEventMotionY e
    --                   return $! V2 x y

    -- mouseMotionB  <- stepper (V2 0 0) mouseMotionE
    -- sink mouseLabel   [#label :== mouseMotionB]


    draw drawingArea drawCanvas'



    mousePressedE <- signalE1' drawingArea #buttonPressEvent $ \e -> do
                      x <- Gdk.getEventButtonX e
                      y <- Gdk.getEventButtonY e
                      return (x,y)
    reactimate $ print <$> mousePressedE



    -- mousePressedB <- stepper "not clicked yet" (showT <$> mousePressedE)

    -- mouseMotionB  <- stepper ""                (showT <$> mouseMotionE)
    -- sink mouseLabel   [#label :== mouseMotionB]



    #showAll window

-- drawHull      :: Behavior [Point 2 Double] -> Behavior (Canvas ())
-- drawHull ptsB = undefined

drawCanvas' :: Behavior (Canvas ())
drawCanvas' = pure drawBlue



followMouse :: Behavior (V2 Double) -> Behavior (Canvas ())
followMouse = fmap $ \pos -> do
    Canvas.background $ Canvas.blue 255
    Canvas.stroke $ Canvas.red 250
    Canvas.circle' pos 100



updateCanvas :: Behavior Bool -> Behavior (Canvas ())
updateCanvas = fmap f
  where
    f True  = updateCanvas'
    f False = drawBlue



updateCanvas'             :: Canvas ()
updateCanvas'  = do
    Canvas.background $ Canvas.rgb 255 255 255
    Canvas.stroke $ Canvas.gray 10
    Canvas.fill $ Canvas.red 255
    Canvas.line (V2 30 30) (V2 400 100)
    let frames = 20

    Canvas.line (V2 10 10) (V2 (25+frames) (25+frames))

    img <- Canvas.grab (Canvas.D 0 0 300 300)
    Canvas.image img (V2 350 0)





drawBlue   :: Canvas ()
drawBlue = do
    Canvas.background $ Canvas.blue 250
