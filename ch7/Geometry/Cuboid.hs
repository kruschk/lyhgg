module Geometry.Cuboid (area, volume) where

area :: Float -> Float -> Float -> Float
area a b c =
  rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
