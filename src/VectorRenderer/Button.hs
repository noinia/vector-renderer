module VectorRenderer.Button where

import           Cairo.Canvas.Primitives
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Intersection
import           Reflex
import           Reflex.SDL2 hiding (point, Rectangle, Point)
import           SDL.GeometryUtil
import           VectorRenderer.ReflexSDLRenderer
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


button :: forall t m r. (ReflexSDL2Renderer t m r, RealFrac r)
       => Rectangle () r -> String -> m (Event t ButtonState)
button rect label = do
           dMousePos <- mousePositionDyn @r

           dMouseIsInside <- holdDyn False ((\case
                                               Nothing -> False
                                               Just (p :+ _) -> p `intersects` rect
                                            ) <$> updated dMousePos)

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
             in withFill color $ rectangle rect

           updated <$> holdUniqDyn dButtonState
