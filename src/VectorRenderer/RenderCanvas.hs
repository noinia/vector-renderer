{-# LANGUAGE ScopedTypeVariables #-}
module VectorRenderer.RenderCanvas where

import           Cairo.Canvas.Ipe
import           Cairo.Canvas.Primitives
import           Data.Ext
import           Data.Geometry.Box
import           Data.Maybe (fromMaybe)
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Ipe.Color
import           UI.Canvas

--------------------------------------------------------------------------------


colored            :: RealFrac r => (a -> Canvas b) -> a :+ IpeColor r -> Canvas b
colored f (x :+ c) = withStroke (fromMaybe (Canvas.gray 255) $ toCanvasColor c) $ f x


-- | Renders a rectangle with rounded corners.
roundedRectangle'                :: RealFrac r
                                => Maybe (IpeColor r) -- ^ stroke color
                                -> Maybe (IpeColor r) -- ^ fill color
                                -> r -- ^ border radius
                                -> Rectangle p r -- ^ non-rounded rect
                                -> Canvas ()
roundedRectangle' ms mf = roundedRectangle (ms >>= toCanvasColor) (mf >>= toCanvasColor)
