--
-- Loading modules
--
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: Eq a => [a] -> Int
numUniques = length . List.nub

--
-- Data.Map
--
phoneBook =
  [ ("betty", "555-2938")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  ]

findKey :: Eq k => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey' :: Eq k => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k, v):xs) =
  if key == k
    then Just v
    else findKey' key xs

findKey'' :: Eq k => k -> [(k, v)] -> Maybe v
findKey'' key =
  foldl
    (\acc (k, v) ->
       if key == k
         then Just v
         else acc)
    Nothing

penny = findKey'' "penny" phoneBook

myMap =
  Map.fromList
    [ ("betty", "555-2938")
    , ("bonnie", "452-2928")
    , ("patsy", "493-2928")
    , ("lucille", "205-2928")
    , ("wendy", "939-8282")
    , ("penny", "853-2492")
    ]
--
-- Data.Set
--
