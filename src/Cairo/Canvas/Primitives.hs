module Cairo.Canvas.Primitives where

import           Control.Lens
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import qualified Data.Geometry.Box as Box
import           Data.Geometry.Matrix
import           Data.Geometry.Triangle
import           Data.Geometry.Vector.VectorFamilyPeano
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.Rendering.Cairo.Canvas (Canvas, Color)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import qualified Graphics.Rendering.Cairo.Matrix as CairoMatrix
import           Linear.V2 (V2)

--------------------------------------------------------------------------------

-- | With fill color
withFill       :: Color -> Canvas b -> Canvas b
withFill c act = do Canvas.fill c
                    y <- act
                    Canvas.noFill
                    pure y

-- | With stroke color
withStroke       :: Color -> Canvas b -> Canvas b
withStroke c act = do Canvas.stroke c
                      y <- act
                      Canvas.noStroke
                      pure y

withTransformation       :: Real r => Transformation 2 r -> Canvas b -> Canvas b
withTransformation t act = do Canvas.pushMatrix
                              applyTransformation t
                              y <- act
                              Canvas.popMatrix
                              pure y

withFontTransform      :: Real r => Transformation 2 r -> Canvas () -> Canvas ()
withFontTransform t act = let m = toCairoMatrix . view transformationMatrix . fmap realToFrac $ t
                          in withFontMatrix m act


-- | use some clipping rectangle while drawing things
withClip :: (Num r, Real r) => Rectangle p r -> Canvas () -> Canvas ()
withClip rect' act = do lift $ Cairo.rectangle x y w h
                        lift Cairo.clip
                        act
                        lift Cairo.resetClip
  where
    Point2 x y = fmap realToFrac . view core . minPoint $ rect'
    Vector2 w h = realToFrac <$> Box.size rect'


--------------------------------------------------------------------------------
-- * Drawing primitives

-- | draw a point as a small disk
point   :: Real r => Point 2 r -> Canvas ()
point p = do Canvas.getStroke >>= \case
               Nothing -> pure ()
               Just c  -> Canvas.fill c
             Canvas.circle' (realToFrac <$> (p^.toV2')) 5

-- | draw as a point
point'   :: Real r => Point 2 r -> Canvas ()
point' p = Canvas.point . fmap realToFrac $ p^.toV2'


lineSegment    :: Real r => LineSegment 2 p r -> Canvas ()
lineSegment s' = let s = second realToFrac s'
                 in Canvas.line (s^.start.core.toV2') (s^.end.core.toV2')


rectangle    :: (Real r, Ord r, Num r) => Rectangle p r -> Canvas ()
rectangle r' = let r                                 = second realToFrac r'
                   (Corners _ _ _ (Point2 x y :+ _)) = corners r
               in Canvas.rect $ Canvas.D x y (width r) (height r)


polygon     :: Real r => SimplePolygon p r -> Canvas ()
polygon pg' = let pg = second realToFrac pg'
              in Canvas.polygon $ pg^..outerBoundaryVector.traverse.core.toV2'

polyLine    :: Real r => PolyLine 2 p r -> Canvas ()
polyLine p' = let p = second realToFrac p'
              in Canvas.shape Canvas.ShapeLines $ p^..points.traverse.core.toV2'

triangle                                         :: Real r => Triangle 2 p r -> Canvas ()
triangle t' = let (Triangle p q r) = second realToFrac t' in
    Canvas.triangle (p^.core.toV2') (q^.core.toV2') (r^.core.toV2')

--------------------------------------------------------------------------------
-- * Helpers

toV2' :: Getter (Point 2 r) (V2 r)
toV2' = vector.unV.to (\(VectorFamily v2) -> v2)

-- | Run a computation with a font matrix
withFontMatrix       :: CairoMatrix.Matrix -> Canvas () -> Canvas ()
withFontMatrix m act = do origMatrix <- lift Cairo.getFontMatrix
                          lift $ Cairo.setFontMatrix m
                          act
                          lift $ Cairo.setFontMatrix origMatrix



applyTransformation :: Real r => Transformation 2 r -> Canvas ()
applyTransformation = applyMatrix . toCairoMatrix . view transformationMatrix . fmap realToFrac

toCairoMatrix                            :: Matrix 3 3 Double -> CairoMatrix.Matrix
toCairoMatrix (Matrix (Vector3
                       (Vector3 a b c)
                       (Vector3 d e f)
                       _)              ) = CairoMatrix.Matrix a b d e c f


--------------------------------------------------------------------------------
-- Move to cairo-canvas

-- | Apply a given transformation
applyMatrix :: CairoMatrix.Matrix -> Canvas ()
applyMatrix = lift . Cairo.transform


--------------------------------------------------------------------------------
