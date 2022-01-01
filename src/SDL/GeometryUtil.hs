{-# OPTIONS_GHC -fno-warn-orphans #-}
module SDL.GeometryUtil where

import           Control.Lens (view, to)
import           Control.Monad.Reader.Class
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.Geometry.Vector.VectorFamilyPeano
import           Data.Int (Int32)
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import           Reflex
import qualified Reflex.SDL2 as Reflex
import           Reflex.SDL2 hiding (Point, mult, vector)
import           GeomViewer.Viewport

--------------------------------------------------------------------------------


-- | A type representing one layer in our app.
type Layer = Canvas ()

type ReflexSDL2Renderer t m r = ( ReflexSDL2 t m
                                , DynamicWriter t [Layer] m
                                , MonadReader (Dynamic t (Viewport r)) m
                                )





--------------------------------------------------------------------------------


-- | Construct a Dyn with the mouse position. May be nothing if we are
-- outside of the window.
mousePositionDyn :: (Num r, RealFrac r', ReflexSDL2 t m, MonadReader (Dynamic t (Viewport r')) m)
                 => m (Dynamic t (Maybe (Point 2 r :+ MouseMotionEventData)))
mousePositionDyn = do evMouseMove <- getMouseMotionEvent
                      -- I guess we still want to detect when we go outside of the screen again.
                      dRawMousePos  <- holdDyn Nothing (Just <$> evMouseMove)
                      dViewPort     <- ask
                      let dMousePos = zipDynWith withViewPort dViewPort dRawMousePos
                      pure $ fmap (fmap f) dMousePos
  where
    f eventData = let P (V2 x y) = fromIntegral <$> mouseMotionEventPos eventData
                  in Point2 x y :+ eventData


-- | Dynamic that indicates when the left-mouse button is down.
mouseDownDyn :: ( RealFrac r
                , ReflexSDL2 t m, MonadReader (Dynamic t (Viewport r)) m)
             => m (Dynamic t Bool)
mouseDownDyn = toggle False =<< mouseClickEvent


mouseDownEvent :: ( Num r
                  , RealFrac r'
                  , ReflexSDL2 t m, MonadReader (Dynamic t (Viewport r')) m)
               => m (Event t (Point 2 r :+ MouseButtonEventData))
mouseDownEvent = mouseClickEvent


-- | Mouseclick event
mouseClickEvent :: ( Num r
                   , RealFrac r'
                   , ReflexSDL2 t m, MonadReader (Dynamic t (Viewport r')) m)
                => m (Event t (Point 2 r :+ MouseButtonEventData))
mouseClickEvent = do bViewport <- current <$> ask
                     fmap f . attachWith applyViewport bViewport <$> getMouseButtonEvent
  where
    f eventData = let P (V2 x y) = fromIntegral <$> mouseButtonEventPos eventData
                  in Point2 x y :+ eventData

    applyViewport (Viewport _ trans) evt =
      evt { mouseButtonEventPos = transformBy' trans $ mouseButtonEventPos evt
          }


-- | Convert to a proper math coordinate system with the origin in the bottom left
toMathCoords            :: Num r => r -> V2 r -> V2 r
toMathCoords h (V2 x y) = V2 x (h - y)



withViewPort                    :: RealFrac r => Viewport r
                                -> Maybe MouseMotionEventData -> Maybe MouseMotionEventData
withViewPort (Viewport _ trans) = fmap $ \evt ->
  evt { mouseMotionEventPos       = transformBy' trans $ mouseMotionEventPos evt
      , mouseMotionEventRelMotion = transformBy' trans $ mouseMotionEventRelMotion evt
      }



transformBy'   :: forall f r. (Functor f, IsTransformable (f r), RealFrac r
                  , Dimension (f r) ~ 2, NumType (f r) ~ r)
               => Transformation 2 r -> f Int32 -> f Int32
transformBy' t = fmap round . transformBy t . fmap (fromIntegral @Int32 @r)


type instance Dimension (V2 r) = 2
type instance NumType (V2 r) = r

type instance Dimension (Reflex.Point V2 r) = 2
type instance NumType (Reflex.Point V2 r) = r

instance Fractional r => IsTransformable (V2 r) where
  transformBy t = view (vector.unV.to (\(VectorFamily v2) -> v2))
                . transformBy t
                . Point . MKVector . VectorFamily

instance Fractional r => IsTransformable (Reflex.Point V2 r) where
  transformBy t (Reflex.P v) = Reflex.P $ transformBy t v
