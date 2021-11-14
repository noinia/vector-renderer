{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VectorRenderer.Layout where

import Data.Maybe
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
makePrisms ''Attribute


data Child r a = Child { _relativeSize :: Size
                       , _theLayout    :: Layout r a
                       } deriving (Show,Eq)


data Layout r a = Full    a [Attribute r]
                | Rows    a [Attribute r] [Child r a]
                | Columns a [Attribute r] [Child r a]
                deriving (Show,Eq)


makeLenses ''Child

attributes :: Lens' (Layout r a) [Attribute r]
attributes = lens (\case
                      Full    _ ats   -> ats
                      Rows    _ ats _ -> ats
                      Columns _ ats _ -> ats
                  ) (\l ats -> case l of
                                 Full x _ -> Full x ats
                                 Rows x _ chs -> Rows x ats chs
                                 Columns x _ chs -> Columns x ats chs
                    )



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
  -- note this order is chosen on purpose so that we can render
  -- rectangles in back to front order.
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

computeLayout        :: forall r a. (Fractional r, Ord r)
                     => Rectangle () r -> Layout r a
                     -> Layout r (Rectangle () r :+ a)
computeLayout screen layout = case layout of
    Full e _ats        -> Full    (screen :+ e) ats
    Rows e _ats chs    -> let h' = h - totalSpacing chs -- available height
                              f alpha = Vector2 w (alpha*h')
                          in Rows    (screen :+ e) ats $ vertical   tl f spacing chs
    Columns e _ats chs -> let w' = w - totalSpacing chs -- available width after removing spacing
                              f alpha = Vector2 (alpha*w') h
                          in Columns (screen :+ e) ats $ horizontal bl f spacing chs
  where
    ats = layout^.attributes

    pad = fromMaybe 0 . listToMaybe . mapMaybe (^?_Padding) $ ats
    inner = shrink pad screen

    Vector2 w h = size inner
    (bl :+ _) = minPoint inner
    tl = bl .+^ Vector2 0 h

    spacing = getSpacing ats
    totalSpacing chs = (0 `max` (List.genericLength chs - 1)) * spacing


totalWeight :: Num r => [Child r a] -> r
totalWeight = fromIntegral . sum . map (^.relativeSize)

horizontal                   :: (Fractional r, Ord r)
                             => Point 2 r -> (r -> Vector 2 r) -> r -- ^ spacing
                             -> [Child r a] -> [Child r (Rectangle () r :+ a)]
horizontal bl0 f spacing chs = map (\(r :+ c) -> c&theLayout %~ computeLayout r)
                             . placeAll . map (assignFract f t) $ chs
  where
    t = totalWeight chs

    place p v = box (ext p) (ext $ p .+^ v)
    placeAll = snd . List.mapAccumL (\bl (v@(Vector2 w _) :+ c) -> ( bl .+^ Vector2 (w+spacing) 0
                                                                   , place bl v :+ c
                                                                   )
                                    ) bl0

getSpacing :: Num r => [Attribute r] -> r
getSpacing = fromMaybe 0 . listToMaybe . mapMaybe (^?_Spacing)

vertical                   :: (Fractional r, Ord r)
                           => Point 2 r -> (r -> Vector 2 r) -> r
                           -> [Child r a] -> [Child r (Rectangle () r :+ a)]
vertical tl0 f spacing chs = map (\(r :+ c) -> c&theLayout %~ computeLayout r)
                           . placeAll . map (assignFract f t) $ chs
 where
    t = totalWeight chs
    place (Point2 x y) (Vector2 w h) = box (ext $ Point2 x (y-h)) (ext $ Point2 (x+w) y)
    placeAll = snd . List.mapAccumL (\tl (v@(Vector2 _ h) :+ c) -> ( tl .-^ Vector2 0 (h+spacing)
                                                                   , place tl v :+ c
                                                                   )
                                    ) tl0


assignFract         :: Fractional r
                    => (r -> Vector 2 r)
                    -> r -- ^ total
                    -> Child r a
                    -> Vector 2 r :+ Child r a
assignFract f total c = f ((c^.relativeSize.to fromIntegral) / total) :+ c


--------------------------------------------------------------------------------

shrink         :: (Arity d, Num r) => r -> Box d p r -> Box d p r
shrink delta r = r&minP.core.cwMin %~ (.+^ pure delta)
                  &maxP.core.cwMax %~ (.-^ pure delta)
  -- fixme; make sure that min remains smaller than max.
