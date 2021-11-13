{-# LANGUAGE TemplateHaskell  #-}
module Data.Camera(module Cam
                  -- , myCamera, myCamera1
                  ) where

import Graphics.Camera as Cam
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Default


--------------------------------------------------------------------------------

-- myCamera :: Camera Double
-- myCamera = Camera (Point3 50 0 50)
--                   (signorm $ Vector3 0 0 (-1))
--                   (Vector3 0 1 0)
--                   10
--                   15
--                   55
--                   (Vector2 80 60)


-- myCamera1 :: Camera Double
-- myCamera1 = Camera origin
--                   (Vector3 0 0 (-1))
--                   (Vector3 0 1 0)
--                   10
--                   10
--                   50 -- we can see up to the origin
--                   (Vector2 60 40)


instance Fractional r => Default (Camera r) where
  def = Camera (Point3 50 0 50)
               (Vector3 0 0 (-1))
               (Vector3 0 1 0)
               (1/10) -- pick vp dist to be the same as the camera dist
               (1/10)
               1000
               (Vector2 800 600)
