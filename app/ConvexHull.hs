{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Control.Monad (forM_, guard, void)
import Data.Ext
import Graphics.Rendering.Cairo.Canvas hiding (withRenderer, point)
import Reflex
import Reflex.SDL2 hiding (point)
import SDL.GeometryUtil
import SDL.Util
import VectorRenderer.ReflexSDLRenderer
import VectorRenderer.RenderCanvas

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | An axis aligned bounding box.
data AABB = AABB InputMotion (V2 Int)


--------------------------------------------------------------------------------
-- | Convert a mouse button to an AABB.
mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


--------------------------------------------------------------------------------
-- | Convert a mouse button motion to color.
motionToColor :: InputMotion -> Color
motionToColor Released = fromIntegral <$> V4 255 0 0   128
motionToColor Pressed  = fromIntegral <$> V4 0   0 255 128


--------------------------------------------------------------------------------
-- | Renders an AABB using the handy SDL 2d 'Renderer'.

renderAABB :: Color -> V2 Int -> Canvas ()
renderAABB color pos = do fill color
                          rect $ D (x-10) (y-10) 20 20
  where
    V2 x y = fromIntegral <$> pos



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
  :: (ReflexSDL2 t m, DynamicWriter t [Layer] m)
  => m (Event t ButtonState)
button = do
  evMotionData <- getMouseMotionEvent
  let position = V2 100 100
      size     = V2 100 100
      V2 tlx tly = position
      V2 brx bry = position + size
      evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
      evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
        (x >= tlx && x <= brx) && (y >= tly && y <= bry)
  dMouseIsInside <- holdDyn False evMouseIsInside

  evBtn <- getMouseButtonEvent
  let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
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
    in do fill color
          rect $ D 100 100 100 100

  updated <$> holdUniqDyn dButtonState


--------------------------------------------------------------------------------

-- data InOrOut a = Outside | Inside a





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


  --------------------------------------------------------------------------------
  -- draw the points

  evClicked <- mouseClickEvent
  dPoints <- foldDyn (\(p :+ _) pts -> p : pts) [] evClicked
  drawLayer $ fmap (\points -> mapM_ point $ reverse points) dPoints


  -- evClicked <- getMouseButtonEvent
  -- dPoints <- foldDyn (\dat pts -> let P pos = fromIntegral <$> mouseButtonEventPos dat
  --                                 in pos : pts
  --                    ) [] evClicked
  -- drawLayer $ ffor dPoints $ \points -> mapM_ (renderAABB $ blue 255) $ reverse points



  -- ------------------------------------------------------------------------------
  -- -- Ghosty trail of squares
  -- ------------------------------------------------------------------------------
  -- -- Gather all mouse motion events into a list, then draw a drawLayers that
  -- -- renders each move as a quarter alpha'd yello or cyan square.
  -- evMouseMove <- getMouseMotionEvent
  -- -- I guess we still want to detect when we go outside of the screen again.
  -- dMousePos <- holdDyn Nothing (Just <$> evMouseMove)
  -- let dMousePosDrawing = ffor dMousePos $ \case
  --       Nothing  -> pure () -- don't draw anything
  --       Just dat -> let P pos = fromIntegral <$> mouseMotionEventPos dat
  --                       color = if null (mouseMotionEventState dat)
  --                               then V4 255 255 0   128
  --                               else V4 0   255 255 128
  --                   in renderAABB color pos
  -- drawLayer dMousePosDrawing


  dMousePos <- mousePositionDyn
  let dMousePosDrawing = ffor dMousePos $ \case
        Nothing         -> pure () -- don't draw anything
        Just (p :+ dat) -> do let color = if null (mouseMotionEventState dat)
                                          then V4 255 255 0   128
                                          else V4 0   255 255 128
                              fill color
                              point p
  drawLayer dMousePosDrawing





  -- dMoves      <- foldDyn (\x _ -> Just x) Nothing evMouseMove
  -- dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  -- drawLayer $ fforMaybe dMoves $ fmap $ \dat ->

  ------------------------------------------------------------------------------
  -- Up and down squares
  ------------------------------------------------------------------------------
  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Draw a drawLayers of those rendered up/down AABBs.
  evMouseButton <- getMouseButtonEvent
  dBtns         <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseButton
  drawLayer $ ffor dBtns $ \btns ->
    forM_ (reverse btns) $ \dat -> do
      let AABB motion pos = mouseButtonToAABB dat
          color = motionToColor motion
      pure $ renderAABB color pos


white :: Color
white = gray 255

black :: Color
black = gray 0

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
