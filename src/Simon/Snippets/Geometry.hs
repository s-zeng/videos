module Simon.Snippets.Geometry where

import Codec.Picture
import Control.Monad.Trans.State (evalState)
import qualified Data.Vector.Unboxed as V
import Geom2D.CubicBezier.Linear
import Graphics.SvgTree
import Linear.Metric (dot, norm, normalize)
import Linear.V2 (V2 (..))
import Linear.Vector
import Reanimate
import Reanimate.Builtin.Documentation
import Relude hiding (evalState)
import Relude.Extra.Lens ((^.))
import Simon.Static.Colours (black, blue, foreground, green, purple, red, teal, white, yellow)

data Triangle a = Triangle {p1 :: a, p2 :: a, p3 :: a} deriving (Eq, Show, Functor)

-- | @mkTrianglePoints@ creates a triangle from triangle struct
mkTriangle :: Triangle (V2 Double) -> SVG
mkTriangle (Triangle (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) =
  mkLinePathClosed [(x1, y1), (x2, y2), (x3, y3)]

-- | @mkTriangleSafe@ creates a triangle struct that has the given three side lengths, if one exists
mkTriangleSafe :: Double -> Double -> Double -> Maybe (Triangle (V2 Double))
mkTriangleSafe a b c = do
  guard $ a > 0
  guard $ b > 0
  guard $ c > 0
  let [low, mid, high] = sort [a, b, c]
  guard $ low + mid > high
  let pointC = V2 0 0
  let pointB = V2 0 a
  let cosAngleC = (a * a + b * b - c * c) / (2 * a * b)
  let pointA = V2 (b * (1 - cosAngleC ** 2)) (b * cosAngleC)
  return $ Triangle pointC pointB pointA

-- | given triangle points, draw circumscribed circle
mkCircumscribedCircle :: Triangle (V2 Double) -> SVG
mkCircumscribedCircle (Triangle a@(V2 x1 y1) b@(V2 x2 y2) c@(V2 x3 y3)) =
  let sideAB = b - a
      sideBC = c - b
      sideAC = c - a
      -- https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2
      divisor = 2 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
      circumCenter@(V2 xc yc) =
        V2
          (((x1 * x1 + y1 * y1) * (y2 - y3) + (x2 * x2 + y2 * y2) * (y3 - y1) + (x3 * x3 + y3 * y3) * (y1 - y2)) / divisor)
          (((x1 * x1 + y1 * y1) * (x3 - x2) + (x2 * x2 + y2 * y2) * (x1 - x3) + (x3 * x3 + y3 * y3) * (x2 - x1)) / divisor)
      [smallest, middle, largest] = sort [norm sideAB, norm sideBC, norm sideAC]
      -- using numerically stable heron's: https://en.wikipedia.org/wiki/Heron%27s_formula#Numerical_stability
      radius = smallest * middle * largest / sqrt ((smallest + (middle + largest)) * (largest - (smallest - middle)) * (largest + (smallest - middle)) * (smallest + (middle - largest)))
   in translate xc yc . center $ mkCircle radius

-- | given triangle points, draw inscribed circle
mkInscribedCircle :: Triangle (V2 Double) -> SVG
mkInscribedCircle (Triangle a@(V2 x1 y1) b@(V2 x2 y2) c@(V2 x3 y3)) =
  let sideAB = b - a
      sideBC = c - b
      sideAC = c - a
      -- https://en.wikipedia.org/wiki/Incircle_and_excircles_of_a_triangle#Incircle_and_its_radius_properties
      divisor = (norm sideAB + norm sideBC + norm sideAC)
      inCenter@(V2 xc yc) =
        V2
          ((norm sideBC * x1 + norm sideAC * x2 + norm sideAB * x3) / divisor)
          ((norm sideBC * y1 + norm sideAC * y2 + norm sideAB * y3) / divisor)
      s = divisor / 2
      radius = sqrt (((s - norm sideAB) * (s - norm sideBC) * (s - norm sideAC)) / s)
   in translate xc yc . center $ mkCircle radius

-- | given triangle points, draw 9-point circle
-- we cheat a bit by just using circumscribed circle on the midpoints :D
mkNinePointCircle :: Triangle (V2 Double) -> SVG
mkNinePointCircle (Triangle a@(V2 x1 y1) b@(V2 x2 y2) c@(V2 x3 y3)) =
  let midpointAB = (b + a)/2
      midpointBC = (c + b)/2
      midpointAC = (c + a)/2
   in mkCircumscribedCircle (Triangle midpointAB midpointBC midpointAC)

tester :: IO ()
tester = reanimate $
  docEnv $
    mkAnimation 20 $ \t ->
      let triang = fromMaybe (Triangle 0 0 0) $ mkTriangleSafe 5 6 (2 + 6 * bellS 2 t)
          coreSvg =
            center $
              mkGroup
                [ -- piSvg
                  withStrokeColorPixel foreground $ mkTriangle triang,
                  withStrokeColorPixel red $ mkCircumscribedCircle triang,
                  withStrokeColorPixel blue $ mkInscribedCircle triang,
                  withStrokeColorPixel green $ mkNinePointCircle triang
                  -- drawTangent t piSvg
                ]
       in mkGroup
            [ mkBackgroundPixel black,
              coreSvg
            ]

-- drawTangent :: Double -> SVG -> SVG
-- drawTangent alpha | alpha >= 1 = id
-- drawTangent alpha = mapTree worker
--   where
--     worker (PathTree path) =
--       let (V2 posX posY, tangent) =
--             atPartial alpha . toLineCommands $ path ^. pathDefinition
--           normed@(V2 tangentX tangentY) = normalize tangent ^* 4
--           V2 midX midY = lerp 0.5 0 normed
--           V2 normVectX normVectY = normalize tangent ^* (svgWidth normalTxt * 1.1)
--           tangentSvg =
--             translate posX posY $
--               rotate (unangle normed / pi * 180 + 180) $
--                 translate
--                   0
--                   (svgHeight tangentTxt / 2)
--                   tangentTxt
--           normalSvg =
--             translate posX posY $
--               rotate (unangle normed / pi * 180 + 90) $
--                 translate
--                   (svgWidth normalTxt / 2 * 1.1)
--                   (svgHeight normalTxt / 2 * 1.3)
--                   normalTxt
--        in mkGroup
--             [ withStrokeWidth defaultStrokeWidth $
--                 withStrokeColorPixel blue $
--                   translate (posX - midX) (posY - midY) $
--                     mkLine (0, 0) (tangentX, tangentY),
--               withStrokeWidth defaultStrokeWidth $
--                 withStrokeColorPixel blue $
--                   translate posX posY $
--                     mkLine (0, 0) (- normVectY, normVectX),
--               withStrokeWidth (defaultStrokeWidth * 2) $
--                 withStrokeColorPixel
--                   blue
--                   tangentSvg,
--               withFillOpacity 1 $
--                 withFillColorPixel black $
--                   withStrokeWidth
--                     0
--                     tangentSvg,
--               withStrokeWidth (defaultStrokeWidth * 2) $
--                 withStrokeColorPixel
--                   blue
--                   normalSvg,
--               withFillOpacity 1 $
--                 withFillColorPixel black $
--                   withStrokeWidth
--                     0
--                     normalSvg
--             ]
--     worker t = t
--     tangentTxt = scale 1.1 $ center $ latex "tangent"
--     normalTxt = scale 1.1 $ center $ latex "normal"

-- atPartial :: Double -> [LineCommand] -> (V2 Double, V2 Double)
-- atPartial alpha cmds = evalState (worker 0 cmds) zero
--   where
--     worker _d [] = pure (0, 0)
--     worker d (cmd : xs) = do
--       from <- get
--       len <- lineLength cmd
--       let frac = (targetLen - d) / len
--       if len == 0 || frac >= 1
--         then worker (d + len) xs
--         else do
--           let bezier = lineCommandToBezier from cmd
--               (pos, tangent) = evalBezierDeriv bezier frac
--           pure (pos, tangent)
--     totalLen = evalState (sum <$> mapM lineLength cmds) zero
--     targetLen = totalLen * alpha

-- lineCommandToBezier :: V2 Coord -> LineCommand -> AnyBezier Coord
-- lineCommandToBezier from line =
--   case line of
--     LineBezier [a] ->
--       AnyBezier $ V.fromList [from, a]
--     LineBezier [a, b] ->
--       AnyBezier $ V.fromList [from, a, b]
--     LineBezier [a, b, c] ->
--       AnyBezier $ V.fromList [from, a, b, c]
--     _ -> error (show line)

-- unangle :: (Floating a, Ord a) => V2 a -> a
-- unangle a@(V2 ax ay) =
--   let alpha = asin $ ay / norm a
--    in if ax < 0
--         then pi - alpha
--         else alpha
