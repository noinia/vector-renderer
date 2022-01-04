{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module GeomViewer.Mode where

import           Control.Lens
import           Data.Default
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           Data.Vinyl.TypeLevel

--------------------------------------------------------------------------------

data PartialPolyLine r = StartPoint (Point 2 r)
                       | PartialPolyLine (PolyLine 2 () r)
                       deriving (Show,Eq)
makePrisms ''PartialPolyLine

data PartialPolygon r = SinglePoint (Point 2 r)
                      | TwoPoints (Point 2 r) (Point 2 r)
                      | MultiplePoints (PolyLine 2 () r)
                      deriving (Show,Eq)
makePrisms ''PartialPolygon

--------------------------------------------------------------------------------

data Mode s r = SelectMode (Maybe s)
              -- ^ the selected item, if any
              | PointMode
              | PolyLineMode (Maybe (PartialPolyLine r))
                -- ^ the points that we already placed
              | PolygonMode  (Maybe (PartialPolygon r))
              deriving (Show,Eq)
makePrisms ''Mode

instance Default (Mode s r) where
  def = SelectMode Nothing


----------------------------------------

newtype ModeAction s r = UpdateMode (Mode s r)
                       deriving (Show,Eq)







--------------------------------------------------------------------------------

data GeomUniverse = SPoint
                  | SPolyLine
                  | SSimplePolygon
                  | SRectangle
                  deriving (Show,Eq)

type family GeomUniverseF r u = (result :: *) | result -> u where
  GeomUniverseF r SPoint         = Point 2 r
  GeomUniverseF r SPolyLine      = PolyLine 2 () r
  GeomUniverseF r SSimplePolygon = SimplePolygon () r
  GeomUniverseF r SRectangle     = Rectangle () r

newtype Attr f r u = Attr { _unAttr :: f (GeomUniverseF r u) }

makeLenses ''Attr

deriving instance Show      (f (GeomUniverseF r u)) => Show (Attr f r u)
deriving instance Eq        (f (GeomUniverseF r u)) => Eq   (Attr f r u)
deriving instance Semigroup (f (GeomUniverseF r u)) => Semigroup   (Attr f r u)
deriving instance Monoid    (f (GeomUniverseF r u)) => Monoid      (Attr f r u)


newtype GeometryCollection f gs r =
  GeometryCollection { _toRec ::  Rec (Attr f r) gs }
makeLenses ''GeometryCollection


instance (Semigroup (Rec (Attr f r) gs)) => Semigroup (GeometryCollection f gs r) where
  (GeometryCollection a) <> (GeometryCollection b) = GeometryCollection $ a <> b

instance (Monoid (Rec (Attr f r) gs)) => Monoid (GeometryCollection f gs r) where
  mempty = (GeometryCollection mempty)


deriving instance (RMap gs, ReifyConstraint Show (Attr f r) gs, RecordToList gs)
                  => Show (GeometryCollection f gs r)

-- | add an element to the collection
insertWith       :: RElem u gs (RIndex u gs)
                 => (GeomUniverseF r u -> f (GeomUniverseF r u) -> f (GeomUniverseF r u))
                 -> GeomUniverseF r u -> GeometryCollection f gs r -> GeometryCollection f gs r
insertWith f x c = c&toRec.rlens.unAttr %~ f x



insert :: RElem u gs (RIndex u gs)
       => GeomUniverseF r u -> GeometryCollection [] gs r -> GeometryCollection [] gs r
insert = insertWith (:)




--------------------------------------------------------------------------------

data Model gs r = Model { _mode   :: Mode (CoRec (Attr Identity r) gs) r
                        , _layers :: Map String (GeometryCollection [] gs r)
                        }
makeLenses ''Model

deriving instance ( Show  (CoRec (Attr Identity r) gs)
                  , Show (GeometryCollection [] gs r), Show r
                  ) => Show (Model gs r)

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- * View
