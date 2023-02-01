--
-- Maximum awesome
--
maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | maxTail < x = x
    | otherwise   = maxTail
    where maxTail = maximum' xs

maximum'' :: Ord a => [a] -> a
maximum'' []     = error "maximum of empty list"
maximum'' [x]    = x
maximum'' (x:xs) = max x (maximum'' xs)

--
-- A few more recursive functions
--
replicate' :: (Ord t, Num t) => t -> a -> [a]
replicate' repetitions element
    | repetitions <= 0 = []
    | otherwise        = element : replicate' (repetitions - 1) element

take' :: (Ord t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' number _
    | number <= 0 = []
take' number (x:xs) = x : take' (number - 1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' xs = last xs : reverse'' (init xs)

repeat' :: t -> [t]
repeat' element = element : repeat' element

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq t => t -> [t] -> Bool
elem' _ [] = False
elem' candidate (x:xs)
    | candidate == x = True
    | otherwise      = candidate `elem'` xs

--
-- Quick, sort!
--
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (head:tail) =
    let lesserEquals = [ x | x <- tail, x <= head]
        greaters = [ x | x <- tail, head < x]
    in quicksort lesserEquals ++ [head] ++ quicksort greaters
