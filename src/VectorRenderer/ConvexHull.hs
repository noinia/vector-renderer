{-# LANGUAGE OverloadedStrings     #-}
module VectorRenderer.ConvexHull where


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

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving Eq


buttonState :: Bool -> Bool -> ButtonState
buttonState isInside isDown
  | not isInside = ButtonStateUp
  | isDown       = ButtonStateDown
  | otherwise    = ButtonStateOver


button
  :: (ReflexSDL2Renderer t m r, RealFrac r)
  => m (Event t ButtonState)
button = do
           let (rect :: Rectangle () Int) = box (ext $ Point2 0 0) (ext $ Point2 200 100)
           dMousePos <- mousePositionDyn @Int


           dMouseIsInside <- holdDyn False ((\case
                                               Nothing -> False
                                               Just (p :+ _) -> p `intersects` rect
                                            ) <$> updated dMousePos)


         -- evMotionData <- getMouseMotionEvent


         --   let position = V2 100 100
         --       size     = V2 100 100
         --       V2 tlx tly = position
         --       V2 brx bry = position + size
         --       evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
         --       evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
         --         (x >= tlx && x <= brx) && (y >= tly && y <= bry)
         --   dMouseIsInside <- holdDyn False evMouseIsInside


           evBtn <- mouseClickEvent
           let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion . _extra
           dButtonIsDown <- holdDyn False evBtnIsDown

           let dButtonStatePre = buttonState <$> dMouseIsInside <*> dButtonIsDown
           evPB         <- getPostBuild
           dButtonState <- holdDyn ButtonStateUp $ leftmost [ updated dButtonStatePre
                                                            , ButtonStateUp <$ evPB
                                                            ]
           drawLayer $ ffor dButtonState $ \st ->
             let color = case st of
                           ButtonStateUp   -> V4 192 192 192 255
                           ButtonStateOver -> 255
                           ButtonStateDown -> V4 128 128 128 255
             in colored' rectangle (rect :+ color)

           updated <$> holdUniqDyn dButtonState

--------------------------------------------------------------------------------

-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double)
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
