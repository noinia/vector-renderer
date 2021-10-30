module VectorRenderer.ReflexSDLRenderer
  ( reflexSdlApp
  , Layer
  , drawLayers, drawLayer
  ) where

import Graphics.Rendering.Cairo.Canvas
import Reflex
import Reflex.SDL2
import SDL.Cairo

-------------------------------------------------------------------------------
-- | A type representing one layer in our app.
type Layer = Canvas ()

-- | Runs a reflex app reflexMain' re-rendering the drawing when a
-- layer is written
reflexSdlApp                             :: ReflexSDL2 t m
                                         => Window -> Renderer
                                         -> DynamicWriterT t [Layer] m ()
                                         -> m ()
reflexSdlApp window renderer reflexMain' = do
  texture <- liftIO $ createCairoTexture' renderer window
  -- get a dynamic representing the layers
  (_, dynLayers) <- runDynamicWriterT $ do reflexMain'
                                           shutdownOn =<< getQuitEvent
  -- at every update to the layers, rerender
  performEvent_ $ ffor (updated dynLayers) $ \layers -> do
    clear renderer
    liftIO . withCairoTexture' texture $ runCanvas $ do
      background white
      sequence_ layers
    copyEx renderer texture Nothing Nothing 0 Nothing (V2 False True)
    present renderer

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
