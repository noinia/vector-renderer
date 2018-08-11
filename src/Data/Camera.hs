{-# LANGUAGE TemplateHaskell  #-}
module Data.Camera where

import VectorRenderer.Import hiding (snoc,element)
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Transformation
import Data.Default
import Data.Geometry.Triangle

data Camera r = Camera { _cameraPosition   :: !(Point 3 r)
                       , _rawCameraNormal  :: !(Vector 3 r)
                         -- ^ unit vector from camera into center of the screen
                       , _rawViewUp        :: !(Vector 3 r)
                       -- ^ viewUp; assumed to be unit vector
                       , _viewPlaneDepth   :: !r
                       , _nearDist         :: !r
                       , _farDist          :: !r
                       , _screenDimensions :: !(Vector 2 r)
                       } deriving (Show,Eq,Ord)
makeLenses ''Camera

instance Fractional r => Default (Camera r) where
  def = Camera origin
               (Vector3 0 0 (-1))
               (Vector3 0 1 0)
               (1/10) -- pick vp dist to be the same as the camera dist
               (1/10)
               1000
               (Vector2 800 600)


cameraNormal :: Floating r => Lens' (Camera r) (Vector 3 r)
cameraNormal = lens _rawCameraNormal (\c n -> c { _rawCameraNormal = signorm n} )

viewUp :: Floating r => Lens' (Camera r) (Vector 3 r)
viewUp = lens _rawViewUp (\c n -> c { _rawViewUp = signorm n})



myT :: Triangle 3 () Double
myT = Triangle (ext $ Point3 1  1  10)
               (ext $ Point3 20 1  10)
               (ext $ Point3 20 30 10)


-- | Full transformation that renders the figure
cameraTransform c =  toViewPort c
                 |.| perspectiveProjection c
                 |.| worldToView c

-- | Translates world coordinates into view coordinates
worldToView   :: Fractional r => Camera r -> Transformation 3 r
worldToView c =  rotateCoordSystem c
             |.| (translation $ (-1) *^ c^.cameraPosition.vector)

-- | Transformation into viewport coordinates
toViewPort   :: Fractional r => Camera r -> Transformation 3 r
toViewPort c = Transformation . Matrix
             $ Vector4 (Vector4 (w/2) 0     0     0)
                       (Vector4 0     (h/2) 0     0)
                       (Vector4 0     0     (1/2) (1/2))
                       (Vector4 0     0     0     1)
  where
    Vector2 w h = c^.screenDimensions


-- | constructs a perspective projection
perspectiveProjection   :: Fractional r => Camera r -> Transformation 3 r
perspectiveProjection c = Transformation . Matrix $
    Vector4 (Vector4 (-n/rx) 0       0              0)
            (Vector4 0       (-n/ry) 0              0)
            (Vector4 0       0       (-(n+f)/(n-f)) (-2*n*f/(n-f)))
            (Vector4 0       0       1              0)
  where
    n = c^.nearDist
    f = c^.farDist
    Vector2 rx ry = (/2) <$> c^.screenDimensions

-- | Rotates coordinate system around the camera, such that we look in the negative z
-- direction
rotateCoordSystem   :: Num r => Camera r -> Transformation 3 r
rotateCoordSystem c = rotateTo $ Vector3 u v n
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera

testRotate   :: Camera Double -> [Vector 3 Double]
testRotate c = map (transformBy t) [u, v, n]
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera
    t = rotateCoordSystem c

testToWorld   :: Camera Double -> [Vector 3 Double]
testToWorld c = map (transformBy t) [u, v, n, Vector3 80 20 40]
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera
    t = worldToView c


testProjection   :: Camera Double -> [Vector 3 Double]
testProjection c = map (transformBy t) [Vector3 30 30 (-10), Vector3 (30*50/10) 30 (-50)]
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera
    t = perspectiveProjection c

-- transformBy' (Transformation m) (Vector3 x y z) = m `mult` (Vector4 x y z (-z))


flipAxes :: Num r => Transformation 3 r
flipAxes = Transformation . Matrix
             $ Vector4 (Vector4 1 0 0 0)
                       (Vector4 0 0 1 0)
                       (Vector4 0 1 0 0)
                       (Vector4 0 0 0 1)



--------------------------------------------------------------------------------

myCamera :: Camera Double
myCamera = Camera (Point3 50 0 50)
                  (signorm $ Vector3 0 0 (-1))
                  (Vector3 0 1 0)
                  10
                  15
                  55
                  (Vector2 80 60)


myCamera1 :: Camera Double
myCamera1 = Camera origin
                  (Vector3 0 0 (-1))
                  (Vector3 0 1 0)
                  10
                  10
                  50 -- we can see up to the origin
                  (Vector2 60 40)
