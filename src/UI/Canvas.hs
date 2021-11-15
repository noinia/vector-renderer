module UI.Canvas where

import           Control.Lens
import           Control.Monad (when)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import qualified Data.Geometry.Polygon as Polygon
import           Data.Maybe (isJust)
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Cairo.Canvas.Primitives

--------------------------------------------------------------------------------

-- | Renders a rectangle with rounded corners.
roundedRectangle                :: RealFrac r
                                => Maybe Canvas.Color -- ^ stroke color
                                -> Maybe Canvas.Color -- ^ fill color
                                -> r -- ^ border radius
                                -> Rectangle p r -- ^ non-rounded rect
                                -> Canvas ()
roundedRectangle mc mf s' rect' = do maybe Canvas.noFill   Canvas.fill   mf
                                     when (isJust mf) $ do
                                       Canvas.noStroke
                                       polygon . Polygon.fromPoints . map ext
                                         $ [ Point2 l     (b+s)
                                           , Point2 l     (t-s)
                                           , Point2 (l+s) t
                                           , Point2 (r-s) t
                                           , Point2 r     (t-s)
                                           , Point2 r     (b+s)
                                           , Point2 (r-s) b
                                           , Point2 (l+s) b
                                           ]
                                     maybe Canvas.noStroke Canvas.stroke mc
                                     arc  (Point2 l       (t-2*s)) 90 180
                                     arc  (Point2 (r-2*s) (t-2*s)) 0 90
                                     arc  (Point2 (r-2*s) b)     270 0
                                     arc  (Point2 l       b) 180 270
                                     line (Point2 l       (b+s)) (Point2 l     (t-s))
                                     line (Point2 (l+s)   t)     (Point2 (r-s) t)
                                     line (Point2 r       (t-s)) (Point2 r     (b+s))
                                     line (Point2 (r-s)   b)     (Point2 (l+s) b)
  where
    Point2 l b = fmap realToFrac . view core . minPoint $ rect'
    Point2 r t = fmap realToFrac . view core . maxPoint $ rect'

    s = realToFrac s'

    line a b' = Canvas.line (a^.toV2') (b'^.toV2')

    -- bottom left corner start angle end angle. positive x axis has angle zero, angles are ccw.
    arc (Point2 x y) a b' =
      Canvas.arc (Canvas.D x y (2*s) (2*s))
                 (Canvas.radians $ realToFrac a)
                 (Canvas.radians $ realToFrac b')
