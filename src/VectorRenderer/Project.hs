module VectorRenderer.Project where

import Control.Lens
import Data.Camera
import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.LineSegment
import Data.Geometry.Triangle
import VectorRenderer.Import


class Projectable t where
  type ProjectsTo t
  -- | Projects the point onto the xy plane w.r.t. a camera somewhere below the xyz plane
  projectWith :: (Ord r, Fractional r
                 , NumType t ~ r, NumType (ProjectsTo t) ~ r
                 ) => Camera r -> t -> ProjectsTo t


instance Projectable (Point 3 r) where
  type ProjectsTo (Point 3 r) = Point 2 r
  projectWith c p = Point . prefix $ (1-lambda) *^ toVec q .+^ lambda *^ toVec p
    where
      q = c^.cameraPosition
      lambda = let qz = q^.zCoord in qz / (p^.zCoord - qz)

instance Projectable (Triangle 3 p r) where
  type ProjectsTo (Triangle 3 p r) = Triangle 2 p r
  projectWith c (Triangle p q r) = Triangle (p&core %~ projectWith c)
                                            (q&core %~ projectWith c)
                                            (r&core %~ projectWith c)


instance Projectable (LineSegment 3 p r) where
  type ProjectsTo (LineSegment 3 p r) = LineSegment 2 p r
  projectWith c (LineSegment p q) = LineSegment (p&unEndPoint.core %~ projectWith c)
                                                (q&unEndPoint.core %~ projectWith c)
