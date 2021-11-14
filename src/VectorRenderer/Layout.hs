{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VectorRenderer.Layout where

import Data.Traversable
import Data.Ext
import Control.Lens
import Data.Geometry.Box
import Data.Geometry.Point
import Data.Geometry.Transformation
import Data.Geometry.Vector
import qualified Data.List as List

--------------------------------------------------------------------------------

type Size = Int

data Attribute r = Spacing r
                 | Padding r
                 deriving (Show,Eq)

data Child r a = Child { _relativeSize :: Size
                       , _theLayout    :: Layout r a
                       } deriving (Show,Eq)


data Layout r a = Full    a [Attribute r]
                | Rows    a [Attribute r] [Child r a]
                | Columns a [Attribute r] [Child r a]
                deriving (Show,Eq)

makeLenses ''Child


instance Functor (Child r) where
  fmap = fmapDefault
instance Foldable (Child r) where
  foldMap = foldMapDefault
instance Traversable (Child r) where
  traverse f c = c&theLayout %%~ traverse f

instance Functor (Layout r) where
  fmap = fmapDefault
instance Foldable (Layout r) where
  foldMap = foldMapDefault
instance Traversable (Layout r) where
  traverse f = \case
    Full    x ats     -> flip Full ats                   <$> f x
    Rows    x ats chs -> (\y chs' -> Rows y ats chs')    <$> f x <*> traverse (traverse f) chs
    Columns x ats chs -> (\y chs' -> Columns y ats chs') <$> f x <*> traverse (traverse f) chs



data HAlign = AlignLeft
            | HCenter
            | AlignRight
            deriving (Show,Eq)

data VAlign = AlignTop
            | VCenter
            | AlignBottom
            deriving (Show,Eq)


data LayoutResult r = LayoutResult { _assignedSpace :: Rectangle () r
                                   -- ^ the space (including spacing) assigned to this node
                                   , _transform     :: Transformation 2 r
                                   -- ^ the transformation that would appropriately draw
                                   -- the content of the item
                                   }





-- computeLayout     :: Fractional r
--                   => Vector 2 r -> Layout a -> Layout (Rectangle 2 r, Transformation 2 r)
-- computeLayout dim =

computeLayout        :: forall r a. Fractional r
                     => Rectangle () r -> Layout r a
                     -> Layout r (Rectangle () r :+ a)
computeLayout screen = \case
    Full e ats        -> Full    (screen :+ e) ats
    Rows e ats chs    -> Rows    (screen :+ e) ats
                       $ go (\alpha -> Vector2 w (alpha*h)) (\(Vector2 _ h') -> Vector2 0 h') chs
    Columns e ats chs -> Columns (screen :+ e) ats
                       $ go (\alpha -> Vector2 (alpha*w) h) (\(Vector2 w' _) -> Vector2 w' 0) chs
  where
    Vector2 w h = size screen
    (bl :+ _) = minPoint screen

    totalWeight = fromIntegral . sum . map (^.relativeSize)

    go             :: (r -> Vector 2 r)          -- how to build the size of a child
                   -> (Vector 2 r -> Vector 2 r) -- how to compute the new bottomleft point
                   -> [Child r a] -> [Child r (Rectangle () r :+ a)]
    go f shift chs = let t = totalWeight chs
                     in map (\(r :+ c) -> c&theLayout %~ computeLayout r)
                      . placeAll bl shift
                      . map (assignFract f t) $ chs



assignFract         :: Fractional r
                    => (r -> Vector 2 r)
                    -> r -- ^ total
                    -> Child r a
                    -> Vector 2 r :+ Child r a
assignFract f total c = f ((c^.relativeSize.to fromIntegral) / total) :+ c

placeAll     :: Num r
             => Point 2 r -- ^ bottom left corner
             -> (Vector 2 r -> Vector 2 r) -- ^ bottomLeft to bottom left function
             -> [Vector 2 r :+ Child r a]
             -> [Rectangle () r :+ Child r a]
placeAll bl0 f = snd . List.mapAccumL (\bl (v :+ c) -> (bl .+^ f v, place bl v :+ c)) bl0

place     :: Num r => Point 2 r -> Vector 2 r -> Rectangle () r
place p v = box (ext p) (ext $ p .+^ v)


-- shrink
