{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI.Layout

  where

import Data.Bifunctor
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Ext
import Control.Lens
import Data.Geometry.Box
import Data.Geometry.Point
import Data.Geometry.Transformation
import Data.Geometry.Vector
import qualified Data.List as List

--------------------------------------------------------------------------------

-- | Horizontal Alignments
data HAlign = AlignLeft
            | HCenter
            | AlignRight
            deriving (Show,Eq)

-- | Vertical allignments
data VAlign = AlignTop
            | VCenter
            | AlignBottom
            deriving (Show,Eq)


-- | Possible Attributes for the layout
--
-- padding and spacing is Elm-ui inspired.
--
data Attribute r = Spacing r -- ^ spacing between children.
                 | Padding r -- ^ around the boundary of the objects
                 | HAlign HAlign
                 | VAlign VAlign
                 deriving (Show,Eq)
makePrisms ''Attribute

type Attributes r = [Attribute r]


type Size = Int

-- | Child of some layout combinator.
data Child r a = Child { _relativeSize :: {-# UNPACK #-} !Size
                       , _theLayout    :: Layout r a
                       } deriving (Show,Eq)

-- | tiling based Layout
data Layout r a = Full    a (Attributes r)
                | Rows    a (Attributes r) [Child r a]
                | Columns a (Attributes r) [Child r a]
                deriving (Show,Eq)


makeLenses ''Child

-- | Lens to the content of the root node
content :: Lens' (Layout r a) a
content = lens (\case
                      Full    x _   -> x
                      Rows    x _ _ -> x
                      Columns x _ _ -> x
                  ) (\l x -> case l of
                                 Full _ ats -> Full x ats
                                 Rows _ ats chs -> Rows x ats chs
                                 Columns _ ats chs -> Columns x ats chs
                    )

-- | Get access to the attributes
attributes :: Lens' (Layout r a) (Attributes r)
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

--------------------------------------------------------------------------------
-- * Layout smart constructors

-- | Equal size rows
rowsUniform           :: a -> [Attribute r] -> [Layout r a] -> Layout r a
rowsUniform x ats chs = Rows x ats $ map (Child 1) chs

-- | Equal size columns
columnsUniform           :: a -> [Attribute r] -> [Layout r a] -> Layout r a
columnsUniform x ats chs = Columns x ats $ map (Child 1) chs


--------------------------------------------------------------------------------
-- * Rendering the layout


-- | Given an initial rectangle, and a rendering function that can
-- render the content of a single item, renders the entire layout in
-- back to front order.
--
-- The input to the render function is the rectangle that gives the
-- space assigned to this element.
--
renderLayout          :: (Applicative f, Fractional r, Ord r)
                      => Rectangle p r
                      -> ((Rectangle () r :+ a) -> f ())
                      -> Layout r a -> f ()
renderLayout r render = traverse_ render . computeLayout (first (const ()) r)



-- render :: ( IsBoxable g, IsTransformable g, NumType g ~ r, Dimension g ~ 2
--           , Ord r, Fractional r
--           )
--        => Rectangle p r -> Layout r g -> Layout r (LayoutResult r g)
-- render r = alignContent
--          . computeLayout (first (const ()) r)


-- alignContent :: Layout r (Rectangle () r :+ g) -> Layout r (LayoutResult r :+ g) ->
-- alignContent = \case
--   Full (r :+ g) _ats -> undefined
--   Rows (r :+ _)

--------------------------------------------------------------------------------
-- * Computing the layout

data LayoutResult r = LayoutResult { _assignedSpace :: Rectangle () r
                                   -- ^ the space (including padding) assigned to this node
                                   , _contentSpace  :: Rectangle () r
                                   -- ^ bounding box of the item to be rendered
                                   , _transform     :: Transformation 2 r
                                   -- ^ the transformation that would appropriately draw
                                   -- the content of the item
                                   }




-- | For each element of the layout we compute the rectangle
-- representing the space assigned to this node/element.
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

----------------------------------------
-- * Helper functions for computeLayout

-- | Computes the total weight of all children
totalWeight :: Num r => [Child r a] -> r
totalWeight = fromIntegral . sum . map (^.relativeSize)

-- | Lay out a bunch of children horizontally
horizontal                   :: (Fractional r, Ord r)
                             => Point 2 r -- ^ bottom left point
                             -> (r -> Vector 2 r) -- ^ determine the
                                                  -- size of child
                                                  -- based on its
                                                  -- relative size.
                             -> r -- ^ spacing
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

-- | Lay out a bunch of children vertically
vertical                   :: (Fractional r, Ord r)
                           => Point 2 r -- ^ top left corner
                           -> (r -> Vector 2 r) -- ^ determine the size from its relative size
                           -> r -- ^ spacing
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


-- | Get the spacing attribute (or 0) if not set
getSpacing :: Num r => Attributes r -> r
getSpacing = fromMaybe 0 . listToMaybe . mapMaybe (^?_Spacing)


-- | Compute the size of a child based on its relative size.
assignFract         :: Fractional r
                    => (r -> Vector 2 r) -- ^ how we compute the size from its relative size.
                    -> r -- ^ total relative size
                    -> Child r a
                    -> Vector 2 r :+ Child r a
assignFract f total c = f ((c^.relativeSize.to fromIntegral) / total) :+ c

--------------------------------------------------------------------------------
-- * Dealing with content









--------------------------------------------------------------------------------

shrink         :: (Arity d, Num r, Ord r) => r -> Box d p r -> Box d p r
shrink delta r = r&minP.core.cwMin %~ (.+^ pure delta)
                  &maxP.core.cwMax %~ (.-^ pure delta)
  -- fixme; make sure that min remains smaller than max.
