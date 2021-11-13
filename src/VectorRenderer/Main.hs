{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns     #-}
module VectorRenderer.Main where


import           Control.Lens
import           Control.Monad (void)
import           Data.Bifunctor
import           Data.Camera
import           Data.Default
import           Data.Ext
import           Data.Geometry.Arrangement
import           Data.Geometry.Box(Rectangle, box, size)
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon hiding (size)
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.PlanarSubdivision.Draw
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import           Graphics.Rendering.Cairo.Canvas (Canvas, (!@))
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Ipe.Color
import           Reflex hiding (Group)
import           Reflex.SDL2 hiding (point, origin, Vector, Point, Rectangle)
import           SDL.GeometryUtil
import           SDL.Util
import           VectorRenderer.ReflexSDLRenderer
import           VectorRenderer.RenderCanvas
import           VectorRenderer.Viewport
-- import           VectorRenderer.Project
import qualified VectorRenderer.RenderCanvas as Render
import qualified Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval as HiddenSurfaceRemoval
import           Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval (EdgeSide(..), Tri)
import           Ipe
import           Data.RealNumber.Rational
import           Debug.Trace


--------------------------------------------------------------------------------

type R = RealNumber 5

type Scene r = [Triangle 3 () r :+ IpeColor r]



mkBottom :: r -> (r,r) -> (r,r) -> c -> [Triangle 3 () r :+ c]
mkBottom z (lx, ly) (rx, ry) c = [ Triangle' (Point3 lx ly z)
                                             (Point3 rx ly z)
                                             (Point3 lx ry z) :+ c
                                 , Triangle' (Point3 lx ry z)
                                             (Point3 rx ry z)
                                             (Point3 rx ly z) :+ c
                                 ]

myT :: Triangle 3 () Rational
myT = Triangle (ext $ Point3 1  1  10)
               (ext $ Point3 20 1  10)
               (ext $ Point3 20 30 10)


-- FIXME 20 causes zero error
topSide z = [ Triangle' (Point3 0   0 z)
                        (Point3 17  0 z)
                        (Point3 17 17 z) :+ purple
            , Triangle' (Point3 0   0 z)
                        (Point3 0  17 z)
                        (Point3 17 17 z) :+ purple
            ]

leftSide x = [ Triangle' (Point3 x 0   0)
                         (Point3 x 17  0)
                         (Point3 x 17 17) :+ navy
             , Triangle' (Point3 x 0   0)
                         (Point3 x 0  17)
                         (Point3 x 17 17) :+ navy
             ]

cube = topSide 17 <> topSide 0 <> leftSide 0 <> leftSide 23

myScene :: Scene Rational
myScene = [ myT :+ seagreen
          -- , Triangle' origin
          --             (Point3 0 40 (-10))
          --             (Point3 0 0  (-10)) :+ purple
          -- , Triangle' (Point3 0 0 (-50))
          --             (Point3 0 40 (-10))
          --             (Point3 0 0  (-10)) :+ navy
          ] <> cube
        ++ axes
        -- ++ cube

axes = [ Triangle' origin
                   (Point3 500 0 (-10))
                   (Point3 500 0 0) :+ red
       , Triangle' origin
                   (Point3 0 500 (-10))
                   (Point3 0 500 0) :+ green
       , Triangle' origin
                   (Point3 0 (-10) 49)
                   (Point3 0 0     49) :+ blue
       ]


----------------------------------------

myCamera :: Camera Rational
myCamera = Camera (Point3 (-30) (-20) 20)
                  (Vector3 0 0 (-1))
                  (Vector3 0 1 0)
                  10
                  15
                  55
                  (Vector2 980 800)


-- triangulateSide          :: Num r => Rectangle p r :+ e -> [Triangle 2 p r :+ e]
-- triangulateSide (r :+ e) = [ Triangle bl tr tl :+ e
--                            , Triangle bl tr br :+ e
--                            ]
--   where
--     Corners tl tr br bl = corners r

-- | Lift a 2d triangle into R3
lift3                    :: (Point 2 r -> Point 3 r) -> Triangle 2 p r -> Triangle 3 p r
lift3 f (Triangle a b c) = Triangle (first f a) (first f b) (first f c)

--------------------------------------------------------------------------------


-- | Main reflex app that can also render layers
reflexMain :: (ReflexSDL2Renderer t m Double)
           => m ()
reflexMain = do
               -- the camera
               let dCamera = constDyn def
                   dScene  = constDyn myScene
               dViewportSize <- fmap (^.viewPort.to size) <$> viewportDyn

               liftIO $ print "woei"
               drawLayer $ drawScene <$> dViewportSize <*> dCamera <*> dScene



               -- dPoints <- foldDyn (:) [] =<< mouseClickEvent
               -- let dHull = fmap (fmap convexHull . NonEmpty.nonEmpty) dPoints


               -- -- draw the hull
               -- drawLayer $ ffor dHull $ \case
               --   Nothing -> pure ()
               --   Just h  -> colored' (polygon . _simplePolygon) (h :+ blue)


               -- draw the points
               -- drawLayer $ fmap (mapM_ (point . _core) . reverse) dPoints

               -- show a point at the mouse pos
               dMousePos <- mousePositionDyn
               let dMousePosDrawing = ffor dMousePos $ \case
                     Nothing         -> pure () -- don't draw anything
                     Just (p :+ dat) -> let color = if null (mouseMotionEventState dat)
                                                    then black
                                                    else blue
                                        in colored point (p :+ color)
               drawLayer dMousePosDrawing


--------------------------------------------------------------------------------

main :: IO ()
main = do
  initializeAll
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowGraphicsContext = OpenGLContext ogl
                         , windowResizable       = True
                         -- , windowHighDPI         = False
                         , windowInitialSize     = V2 1024 768
                         }
  withWindow "Vector Renderer" cfg $ \window -> do
    void $ glCreateContext window
    withRenderer window (-1) defaultRenderer $ \renderer -> do
      host $ reflexSdlApp window renderer reflexMain
  quit






--------------------------------------------------------------------------------


shiftCamera     :: Num r => ArrowKey -> Camera r -> Camera r
shiftCamera k c = c&cameraPosition %~ (.+^ 2 *^ toDirection k)


drawScene         :: (Fractional r, Real r, RealFrac r
                     , Show r, IpeWriteText r, Read r
                     )
                  => Vector 2 Double
                  -> Camera r
                  -> Scene r
                  -> Canvas ()
drawScene (Vector2 w h) c s = do
          Canvas.scale     $ traceShowId $ V2 1 (-1)
          Canvas.translate $ V2 (w/2) (-1*h/2)
          let Vector2 cw ch = realToFrac <$> c^.screenDimensions
          Canvas.scale     $ V2 (w/cw) (h/ch)
          Canvas.stroke $ Canvas.gray 0
          -- mapM_ (Render.colored Render.triangle . project c) $ Prelude.head s
          -- mapM_ (Render.colored Render.triangle) $ renderScene c s
          let arr' = renderAll c s
              arr = traceShow ("arr!",arr') arr'
          Render.ipeOut drawColoredArrangement arr


render                  :: Triangle 3 p r -> Triangle 2 p r
render (Triangle p q r) = Triangle (p&core %~ projectPoint)
                                   (q&core %~ projectPoint)
                                   (r&core %~ projectPoint)


renderScene     :: Fractional r => Camera r -> Scene r
                -> [Triangle 2 () r :+ IpeColor r]
renderScene c s = over core (render . transformBy t) <$> s
  where
    !t = cameraTransform c

renderScene'     :: Fractional r => Camera r -> Scene r
                 -> [Triangle 2 () r :+ (Triangle 3 () r :+ IpeColor r)]
renderScene' c s = (\t3 -> (render . transformBy t $ t3^.core) :+ t3) <$> s
  where
    !t = cameraTransform c



data Screen = Screen



renderAll      :: forall r. (Ord r, Fractional r, RealFrac r
                  , Show r, IpeWriteText r, Read r
                  )
               => Camera r -> Scene r
               -> Arrangement Screen
                              (NonEmpty.NonEmpty (Tri () () (IpeColor r) r))
                              ()
                              (Maybe EdgeSide)
                              (Maybe (IpeColor r))
                              r
renderAll c s = arr&subdivision.faceData %~ fmap mkColor
  where
    arr = HiddenSurfaceRemoval.render (Identity Screen) (c^.cameraPosition) rect' ts
    ts = renderScene' c s
    -- ts = HiddenSurfaceRemoval.scene
    mkColor = fmap (^.extra.extra)
    rect' :: Rectangle () r
    rect' = box (ext origin) (ext . Point $ c^.screenDimensions)






-- | Mirror the canvas s.t. the bottom-left corner is the origin
mirrored     :: Double -> Canvas () -> Canvas ()
mirrored h d = do Canvas.scale     $ V2 1 (-1)
                  Canvas.translate $ V2 0 (-1*h)
                  d

centerScaleViewport         :: Real r
                            => Camera r -> Double -> Double -> Canvas () -> Canvas ()
centerScaleViewport c w h d = do let Vector2 cw ch = realToFrac <$> c^.screenDimensions
                                 Canvas.scale     $ V2 (w/cw) (h/ch)
                                 Canvas.translate $ V2 (w/2)  (h/2)
                                 d


data ArrowKey = UpKey | DownKey | LeftKey | RightKey deriving (Show,Read,Eq,Bounded,Enum)

toArrowKey         :: Text.Text -> Maybe ArrowKey
toArrowKey "Up"    = Just UpKey
toArrowKey "Down"  = Just DownKey
toArrowKey "Left"  = Just LeftKey
toArrowKey "Right" = Just RightKey
toArrowKey _       = Nothing


toDirection          :: Num r => ArrowKey -> Vector 3 r
toDirection UpKey    = Vector3 0    1    0
toDirection DownKey  = Vector3 0    (-1) 0
toDirection LeftKey  = Vector3 (-1) 0    0
toDirection RightKey = Vector3 1    0    0


-- toDir       :: Num r => Camera r -> T.Text -> Maybe (Vector 3 r)
-- toDir c "w" = Just $ c^.cameraNormal
-- toDir c "a" = Just $ c^.


--------------------------------------------------------------------------------

drawColoredArrangement :: (Num r, Ord r)
                       => IpeOut (Arrangement s l v e (Maybe (IpeColor r)) r) Group r
drawColoredArrangement = drawColoredPlanarSubdivision . view subdivision

drawColoredPlanarSubdivision    :: (Num r, Ord r)
                                => IpeOut (PlanarSubdivision s v e (Maybe (IpeColor r)) r)
                                          Group r
drawColoredPlanarSubdivision ps = drawPlanarSubdivisionWith drawVtx
                                                            drawEdge
                                                            drawInternalFace
                                                            drawOuterFace
                                                            ps


-- | Draw vertices using their default representation; disk marks. For
-- the rest we keep their original attributes.
drawVtx                       :: IpeOut' Maybe (VertexId' s, VertexData r v) IpeSymbol r
drawVtx (_vi, VertexData p _) = Just $ defIO p ! attr SFill black

-- | Draw edges using normal line segments
drawEdge              :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
drawEdge (_d, s :+ _) = Just $ defIO s

-- | Internal faces are filled polygons.
drawInternalFace :: IpeOut' Maybe (FaceId' s,   SomePolygon v' r :+ Maybe (IpeColor r))    Path r
drawInternalFace (_, pg :+ mcolor) = (\color -> defIO pg ! attr SFill color) <$> mcolor


showT :: Show a => a -> Text.Text
showT = Text.pack . show

-- | Draw the outer face (in some box)
drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Path r
drawOuterFace (_, _ :+ _) = Nothing
