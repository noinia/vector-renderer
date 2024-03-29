module Cairo.Canvas.Ipe where

import           Cairo.Canvas.Primitives
import           Control.Lens
import           Control.Monad (void, when)
import           Data.Colour.Names (readColourName)
import           Data.Colour.SRGB (RGB(..), toSRGB24)
import           Data.Ext
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Vinyl
import           Data.Vinyl.TypeLevel
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import qualified Ipe
import qualified Ipe.Attributes as A
import           Ipe.Attributes hiding (Matrix)
import           Ipe.Color
import           Ipe.IpeOut (IpeOut,iO)
import           Ipe.Types hiding (ipeObject', width)
import           Ipe.Value
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

pathSegment                     :: Real r => PathSegment r -> Canvas ()
pathSegment (PolyLineSegment p) = polyLine p
pathSegment (PolygonPath p)     = polygon p
pathSegment _                   = error "pathSegment: Not implemented yet"


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
