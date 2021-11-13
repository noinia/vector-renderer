module VectorRenderer.ReflexSDLRenderer
  ( reflexSdlApp
  , Layer
  , drawLayers, drawLayer

  , windowSizeDyn
  , viewportDyn

  , renderInViewport
  , withFontMatrix, applyMatrix
  ) where

import           Control.Lens ((^.), view)
import           Control.Monad.Reader (ReaderT, runReaderT, MonadReader, ask)
import           Control.Monad.Trans.Class (lift)
import           Data.Geometry.Matrix
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.Geometry.Vector.VectorFamilyPeano
import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.Cairo.Canvas
import qualified Graphics.Rendering.Cairo.Matrix as CairoMatrix
import           Reflex
import           Reflex.SDL2 hiding (Vector)
import           SDL.Cairo
import           SDL.GeometryUtil
import           VectorRenderer.Viewport

-------------------------------------------------------------------------------


-- windowSizeDyn :: ReflexSDL2


fromV2 :: V2 r -> Vector 2 r
fromV2 = MKVector . VectorFamily

-- | get the current size of the window.
getWindowSize        :: (Num r, MonadIO m) => Window -> m (Vector 2 r)
getWindowSize window = fmap fromIntegral . fromV2 <$> get (windowSize window)


-- | A dynamic that keeps track of the size of the given window.
windowSizeDyn        :: (ReflexSDL2 t m, Num r) => Window -> m (Dynamic t (Vector 2 r))
windowSizeDyn window = do initSize <- getWindowSize window
                          foldDyn (\ev sz -> if windowSizeChangedEventWindow ev == window
                                             then fmap fromIntegral . fromV2
                                                  $ windowSizeChangedEventSize ev
                                             else sz
                                  ) initSize =<< getWindowSizeChangedEvent


-- | Get the viewport
viewportDyn :: (ReflexSDL2 t m, MonadReader (Dynamic t (Viewport r)) m)
            => m (Dynamic t (Viewport r))
viewportDyn = ask

-- | Runs a reflex app reflexMain' re-rendering the drawing when a
-- layer is written.
reflexSdlApp                             :: forall r t m. (ReflexSDL2 t m, r ~ Double
                                            -- , RealFrac r
                                            )
                                         => Window -> Renderer
                                         -> DynamicWriterT t [Layer]
                                                             (ReaderT (Dynamic t (Viewport r)) m)
                                                             ()
                                         -> m ()
reflexSdlApp window renderer reflexMain' = do
  texture <- liftIO $ createCairoTexture' renderer window
  dWindowSize <- windowSizeDyn window
  let dViewport = fmap flipY dWindowSize
  -- get a dynamic representing the layers
  (_, dynLayers) <- flip runReaderT dViewport . runDynamicWriterT
                    $ do reflexMain'
                         shutdownOn =<< getQuitEvent
  -- at every update to the layers, rerender
  let rerender viewport layers = do
                                   clear renderer
                                   liftIO . withCairoTexture' texture . runCanvas
                                     . renderInViewport viewport $ do
                                         background white
                                         sequence_ layers
                                   copy renderer texture Nothing Nothing
                                   present renderer
  performEvent_ $ attachWith rerender (current dViewport) (updated dynLayers)

renderInViewport              :: Real r => Viewport r -> Canvas () -> Canvas ()
renderInViewport viewport act =
  do applyTransformation (viewport^.worldToHost)
     act

-- | Draw a layer stack that changes over time.
drawLayers :: (ReflexSDL2 t m, DynamicWriter t [Layer] m)
           => Dynamic t [Layer] -> m ()
drawLayers = tellDyn

-- | Draw one layer that changes over time.
drawLayer :: (ReflexSDL2 t m, DynamicWriter t [Layer] m)
          => Dynamic t Layer -> m ()
drawLayer = drawLayers . fmap (:[])


--------------------------------------------------------------------------------

white :: Color
white = gray 255


--------------------------------------------------------------------------------

applyTransformation :: Real r => Transformation 2 r -> Canvas ()
applyTransformation = applyMatrix . toCairoMatrix . view transformationMatrix . fmap realToFrac

toCairoMatrix                            :: Matrix 3 3 Double -> CairoMatrix.Matrix
toCairoMatrix (Matrix (Vector3
                       (Vector3 a b c)
                       (Vector3 d e f)
                       _)              ) = CairoMatrix.Matrix a b d e c f


withFontTransform      :: Real r => Transformation 2 r -> Canvas () -> Canvas ()
withFontTransform t act = let m = toCairoMatrix . view transformationMatrix . fmap realToFrac $ t
                          in withFontMatrix m act

--------------------------------------------------------------------------------
-- Move to cairo-canvas

-- | Apply a given transformation
applyMatrix :: CairoMatrix.Matrix -> Canvas ()
applyMatrix = lift . C.transform


-- | Run a computation with a font matrix
withFontMatrix       :: CairoMatrix.Matrix -> Canvas () -> Canvas ()
withFontMatrix m act = do origMatrix <- lift C.getFontMatrix
                          lift $ C.setFontMatrix m
                          act
                          lift $ C.setFontMatrix origMatrix
