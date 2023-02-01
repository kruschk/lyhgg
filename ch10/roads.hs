data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf size list
    | 0 <= size = take size list : groupsOf size (drop size list)
    | otherwise = []

optimalPath :: RoadSystem -> Path
optimalPath roadSystem
    = let (pathA, pathB) = foldl roadStep ([], []) roadSystem
      in if sum (map snd pathA) < sum (map snd pathB)
         then reverse pathA
         else reverse pathB

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c)
    = let priceA = sum $ map snd pathA
          priceB = sum $ map snd pathB
      in ( if a + priceA <= b + c + priceB
           then (A,a):pathA
           else (C,c):(B,b):pathB
         , if b + priceB <= a + c + priceA
           then (B,b):pathB
           else (C,c):(A,a):pathA
         )

main = do
    input <- getContents
    let path = optimalPath
               $ map (\[a,b,c] -> Section a b c)
               $ groupsOf 3
               $ map read
               $ lines input
    putStrLn $ "The best path to take is: " ++ concat (map (show . fst) path)
    putStrLn $ "The price is: " ++ show (sum $ map snd $ path)
