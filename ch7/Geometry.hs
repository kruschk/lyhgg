module Geometry
  ( cubeArea
  , cubeVolume
  , cuboidArea
  , cuboidVolume
  , sphereArea
  , sphereVolume
  ) where

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c =
  rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * radius ^ 2

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * radius ^ 3
