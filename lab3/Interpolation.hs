module Interpolation where

type Point = (Double, Double)

data Algorithm = Linear | Lagrange | Both deriving (Eq, Show)

linearInterpolation :: Double -> [Point] -> [Point]
linearInterpolation rate [(x1, y1), (x2, y2)] =
  [(x, y1 + (x - x1) * (y2 - y1) / (x2 - x1)) | x <- [x1, x1 + rate .. x2]]
linearInterpolation _ _ = []

lagrangeInterpolation :: Double -> [Point] -> [Point]
lagrangeInterpolation rate points = map interpolate xs
  where
    (xMin, xMax) = (fst (head points), fst (last points))
    xs = [xMin, xMin + rate .. xMax]
    interpolate x =
      ( x,
        sum
          [ y * product [(x - xj) / (xi - xj) | (xj, _) <- points, xj /= xi]
            | (xi, y) <- points
          ]
      )
