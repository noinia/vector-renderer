module SDL.Util
  ( withWindow
  , withRenderer
  ) where

import Reflex.SDL2
import Data.Text
import Foreign.C.Types(CInt)

--------------------------------------------------------------------------------

-- | Creates a Window with the given title and config, runs the
-- function on it, and destroys the window at the end.
withWindow             :: MonadIO m => Text -> WindowConfig -> (Window -> m a) -> m a
withWindow title cfg f = do window <- createWindow title cfg
                            res <- f window
                            destroyWindow window
                            pure res

-- | Creates a renderer (using the same arguments as 'createRenderer',
-- runs the function on it, and finally destroys the renderer.)
withRenderer                     :: MonadIO m
                                 => Window -> CInt -> RendererConfig  -> (Renderer -> m b) -> m b
withRenderer window driver cfg f = do renderer <- createRenderer window driver cfg
                                      res <- f renderer
                                      destroyRenderer renderer
                                      pure res
