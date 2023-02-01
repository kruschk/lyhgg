--
-- Some higher-orderism is in order
--
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (a:as) (b:bs) = f a b : zipWith f as bs

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

--
-- Maps and filters
--
map' :: (t -> a) -> [t] -> [a]
map' f list = [f x | x <- list]

map'' :: (t -> a) -> [t] -> [a]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f list = [x | x <- list, f x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x:xs)
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)

largestDivisible =
  let divisible x = x `rem` 3829 == 0
   in head (filter divisible [99999,99998 ..])

sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

sumOddSquares' = sum (takeWhile (< 10000) [n ^ 2 | n <- [1 ..], odd (n ^ 2)])

collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (1 + 3 * n)

numLongChains =
  let isLong list = length list > 15
   in length (filter isLong (map collatz [1 .. 100]))

--
-- Lambdas
--
addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: Num a => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

--
-- Only folds and horses
--
sum' :: (Foldable t, Num a) => t a -> a
sum' list = foldl (\accumulator current -> accumulator + current) 0 list

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldl (+) 0

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' candidate =
  foldl (\accumulator current -> accumulator || (candidate == current)) False

map''' :: Foldable a => (b -> c) -> a b -> [c]
map''' f = foldr (\x acc -> f x : acc) []

maximum' :: Ord a => [a] -> a
maximum' =
  foldr1
    (\x acc ->
       if x > acc
         then x
         else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: Num a => [a] -> a
product' = foldr1 (*)

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p =
  foldr
    (\x acc ->
       if p x
         then x : acc
         else acc)
    []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

--
-- Function application with $
--
--($) :: (a -> b) -> a -> b
--f $ x = f x
functionApplicationMap = map ($ 3) [(4 +), (10 *), (^ 2), sqrt]

--
-- Function composition
--
--(.) :: (b -> c) -> (a -> b) -> a -> c
--f . g = \x -> f (g x)
oddSquareSum :: Integer
--oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]
