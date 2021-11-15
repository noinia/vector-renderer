{-# LANGUAGE ScopedTypeVariables #-}
module VectorRenderer.RenderCanvas where


import           Control.Lens
import           Control.Monad (void, when)
import           Control.Monad.Trans.Class (lift)
import           Data.Bifunctor
import           Data.Colour.Names (readColourName)
import           Data.Colour.SRGB (RGB(..), toSRGB24)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import qualified Data.Geometry.Box as Box
import qualified Data.Geometry.Polygon as Polygon
import           Data.Geometry.Matrix
import           Data.Geometry.Triangle
import           Data.Geometry.Vector.VectorFamilyPeano
import           Data.Maybe (fromMaybe, isJust)
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import qualified Graphics.Rendering.Cairo.Matrix as CairoMatrix
import qualified Ipe
import qualified Ipe.Attributes as A
import           Ipe.Attributes hiding (Matrix)
import           Ipe.Color
import           Ipe.IpeOut (IpeOut,iO)
import           Ipe.Types hiding (ipeObject', width)
import           Ipe.Value
import           Linear.V2 (V2)
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------


colored     :: RealFrac r => (a -> Canvas b) -> a :+ IpeColor r -> Canvas b
colored f x = colored' f (x&extra %~ fromMaybe (Canvas.gray 255) . toCanvasColor)

colored'            :: (a -> Canvas b) -> a :+ Canvas.Color -> Canvas b
colored' f (x :+ c) = do Canvas.fill c
                         y <- f x
                         Canvas.noFill
                         pure y

rectangle    :: (Real r, Ord r, Num r) => Rectangle p r -> Canvas ()
rectangle r' = let r                                 = second realToFrac r'
                   (Corners _ _ _ (Point2 x y :+ _)) = corners r
               in Canvas.rect $ Canvas.D x y (width r) (height r)


-- | Renders a rectangle with rounded corners.
roundedRectangle                :: RealFrac r
                                => Maybe (IpeColor r) -- ^ stroke color
                                -> Maybe (IpeColor r) -- ^ fill color
                                -> r -- ^ border radius
                                -> Rectangle p r -- ^ non-rounded rect
                                -> Canvas ()
roundedRectangle ms mf = roundedRectangle' (ms >>= toCanvasColor) (mf >>= toCanvasColor)

-- | Renders a rectangle with rounded corners.
roundedRectangle'                :: RealFrac r
                                => Maybe Canvas.Color -- ^ stroke color
                                -> Maybe Canvas.Color -- ^ fill color
                                -> r -- ^ border radius
                                -> Rectangle p r -- ^ non-rounded rect
                                -> Canvas ()
roundedRectangle' mc mf s' rect' = do maybe Canvas.noFill   Canvas.fill   mf
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


polygon     :: Real r => SimplePolygon p r -> Canvas ()
polygon pg' = let pg = second realToFrac pg'
              in Canvas.polygon $ pg^..outerBoundaryVector.traverse.core.toV2'

lineSegment    :: Real r => LineSegment 2 p r -> Canvas ()
lineSegment s' = let s = second realToFrac s'
                 in Canvas.line (s^.start.core.toV2') (s^.end.core.toV2')

polyLine    :: Real r => PolyLine 2 p r -> Canvas ()
polyLine p' = let p = second realToFrac p'
              in Canvas.shape Canvas.ShapeLines $ p^..points.traverse.core.toV2'

triangle                                         :: Real r => Triangle 2 p r -> Canvas ()
triangle t' = let (Triangle p q r) = second realToFrac t' in
    Canvas.triangle (p^.core.toV2') (q^.core.toV2') (r^.core.toV2')

toV2' :: Getter (Point 2 r) (V2 r)
toV2' = vector.unV.to (\(VectorFamily v2) -> v2)


-- | draw a point as a small disk
point   :: Real r => Point 2 r -> Canvas ()
point p = do Canvas.getStroke >>= \case
               Nothing -> pure ()
               Just c  -> Canvas.fill c
             Canvas.circle' (realToFrac <$> (p^.toV2')) 5

-- | draw as a point
point'   :: Real r => Point 2 r -> Canvas ()
point' p = Canvas.point . fmap realToFrac $ p^.toV2'

pathSegment                     :: Real r => PathSegment r -> Canvas ()
pathSegment (PolyLineSegment p) = polyLine p
pathSegment (PolygonPath p)     = polygon p
pathSegment _                   = error "pathSegment: Not implemented yet"



-- ipeRoundedRectangle         :: r -- ^ border radius
--                             -> Rectangle 2 p r -- ^ non-rounded rect
--                             -> Path r
-- ipeRoundedRectangle r rect' = Path . fromList $ [ MoveTo bl'
--                                                 , LineTo tl'

--                                                 ]




ipeUse              :: Real r => IpeSymbol r -> Canvas ()
ipeUse (Symbol p _) = point p --- Canvas.circle' (realToFrac <$> p^.toV2') 10

ipePath          :: Real r => Path r -> Canvas ()
ipePath (Path p) = mapM_ pathSegment p

ipeGroup :: RealFrac r => Group r -> Canvas ()
ipeGroup = mapM_ ipeObject . view groupItems


ipeTextLabel                 :: Real r => TextLabel r -> Canvas ()
ipeTextLabel (Ipe.Label t p) = void $ Canvas.text (Text.unpack t) (realToFrac <$> p^.toV2')

ipeObject'              :: forall g r. (RealFrac r, AllConstrained ApplyAttr (AttributesOf g))
                        => (g r -> Canvas ())
                        -> g r :+ IpeAttributes g r
                        -> Canvas ()
ipeObject' f (i :+ ats) = do
                            Canvas.pushMatrix
                            Canvas.noFill
                            Canvas.noStroke
                            applyAttributes (Proxy :: Proxy g) ats
                            f i
                            Canvas.popMatrix

ipeObject                  :: RealFrac r
                           => IpeObject r -> Canvas ()
ipeObject (IpeGroup g)     = ipeObject' ipeGroup g
ipeObject (IpeImage _)     = undefined
ipeObject (IpeTextLabel t) = ipeObject' ipeTextLabel t
ipeObject (IpeMiniPage _)  = undefined
ipeObject (IpeUse p)       = ipeObject' ipeUse  p
ipeObject (IpePath p)      = ipeObject' ipePath p


ipeOut    :: (RealFrac r, ToObject i) => IpeOut g i r -> g -> Canvas ()
ipeOut io = ipeObject . iO . io

applyAttributes               :: (RealFrac r, AllConstrained ApplyAttr (AttributesOf g))
                              => proxy g -> IpeAttributes g r -> Canvas ()
applyAttributes _ (Attrs ats) = applyAttributes' ats

applyAttributes'            :: (RealFrac r, AllConstrained ApplyAttr rs)
                            => Rec (Attr (AttrMapSym1 r)) rs
                            -> Canvas ()
applyAttributes' RNil       = pure ()
applyAttributes' (a :& ats) = applyAttribute a >> applyAttributes' ats


newtype CanvasM = CanvasM { unCanvasM :: Canvas () }

instance Semigroup CanvasM where
  (CanvasM a) <> (CanvasM b) = CanvasM $ a >> b
instance Monoid CanvasM where
  mempty = CanvasM $ pure ()
  a `mappend` b = a <> b

applyAttribute' :: (RealFrac r, ApplyAttr label)
                => Attr (AttrMapSym1 r) label -> CanvasM
applyAttribute' = CanvasM . applyAttribute


class ApplyAttr (label :: AttributeUniverse) where
  applyAttribute :: RealFrac r => Attr (AttrMapSym1 r) label -> Canvas ()


instance ApplyAttr Stroke where
  applyAttribute NoAttr   = pure ()
  applyAttribute (Attr c) = maybe (pure ()) Canvas.stroke $ toCanvasColor c

instance ApplyAttr Fill where
  applyAttribute NoAttr   = pure ()
  applyAttribute (Attr c) = maybe (pure ()) Canvas.fill $ toCanvasColor c

instance ApplyAttr Pen where
  applyAttribute NoAttr            = pure ()
  applyAttribute (Attr (IpePen p)) = case p of
      Named _  -> pure () -- TODO
      Valued v -> Canvas.strokeWeight (realToFrac v)

instance ApplyAttr Clip where
  applyAttribute _ = pure ()

instance ApplyAttr Size where
  applyAttribute _ = pure ()

instance ApplyAttr Dash where
  applyAttribute _ = pure ()


instance ApplyAttr Layer where
  applyAttribute _ = pure ()

instance ApplyAttr LineCap where
  applyAttribute _ = pure ()

instance ApplyAttr LineJoin where
  applyAttribute _ = pure ()

instance ApplyAttr A.Matrix where
  applyAttribute _ = pure ()

instance ApplyAttr Pin where
  applyAttribute _ = pure ()

instance ApplyAttr FillRule where
  applyAttribute _ = pure ()

instance ApplyAttr Arrow where
  applyAttribute _ = pure ()

instance ApplyAttr RArrow where
  applyAttribute _ = pure ()

instance ApplyAttr StrokeOpacity where
  applyAttribute _ = pure ()

instance ApplyAttr Opacity where
  applyAttribute _ = pure ()

instance ApplyAttr Tiling where
  applyAttribute _ = pure ()

instance ApplyAttr Gradient where
  applyAttribute _ = pure ()

instance ApplyAttr Transformations where
  applyAttribute _ = pure ()




-- | Looks up the colorname in the SVG colors if it is a name.
toCanvasColor :: RealFrac r => IpeColor r -> Maybe Canvas.Color
toCanvasColor (IpeColor c) = case c of
    Named t            -> h . toSRGB24 <$> readColourName (Text.unpack $ Text.toLower t)
    Valued v           -> Just $ f v
  where
    f (RGB r g b) = floor <$> V4 (255 *r) (255*g) (255*b) 255
    h (RGB r g b) = V4 r g b 255


withTransformation       :: Real r => Transformation 2 r -> Canvas () -> Canvas ()
withTransformation t act = do Canvas.pushMatrix
                              applyTransformation t
                              act
                              Canvas.popMatrix


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
applyMatrix = lift . Cairo.transform


-- | Run a computation with a font matrix
withFontMatrix       :: CairoMatrix.Matrix -> Canvas () -> Canvas ()
withFontMatrix m act = do origMatrix <- lift Cairo.getFontMatrix
                          lift $ Cairo.setFontMatrix m
                          act
                          lift $ Cairo.setFontMatrix origMatrix


--------------------------------------------------------------------------------

-- | use some clipping rectangle while drawing things
withClip :: (Num r, Real r) => Rectangle p r -> Canvas () -> Canvas ()
withClip rect' act = do lift $ Cairo.rectangle x y w h
                        lift Cairo.clip
                        act
                        lift Cairo.resetClip
  where
    Point2 x y = fmap realToFrac . view core . minPoint $ rect'
    Vector2 w h = realToFrac <$> Box.size rect'
