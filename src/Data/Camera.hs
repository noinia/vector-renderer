{-# LANGUAGE TemplateHaskell  #-}
module Data.Camera where

import VectorRenderer.Import
import Data.Geometry.Point
import Data.Geometry.Vector

data Camera r = Camera { _cameraPosition :: Point 3 r
                       , _orientation    :: Vector 3 r
                       } deriving (Show,Eq,Ord)
makeLenses ''Camera
