--
-- Pattern matching
--
lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Integral a => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pairListSum :: Num a => [(a, a)] -> [a]
pairListSum xs = [a + b | (a, b) <- xs]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) =
  "This list is long. The first two elements are: " ++
  show x ++ " and " ++ show y

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Demonstrate "as patterns"
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

--
-- Guards, guards!
--
bmiTell :: RealFloat a => a -> String
bmiTell bmi
  | bmi < 18.5 = "You're underweight, you emo, you!"
  | bmi < 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi < 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell' :: RealFloat a => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 < 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 < 25.0 =
    "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 < 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: Ord a => a -> a -> a
max' a b
  | a < b = b
  | otherwise = a

compare' :: Ord a => a -> a -> Ordering
compare' a b
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

--
-- Where!?
--
bmiTell'' :: RealFloat a => a -> a -> String
bmiTell'' weight height
  | bmi < 18.5 = "You're underweight, you emo, you!"
  | bmi < 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi < 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f, '.', ' ', l, '.']

calcBMIs :: RealFloat a => [(a, a)] -> [a]
calcBMIs pairs = [bmi weight height | (weight, height) <- pairs]
  where
    bmi weight height = weight / height ^ 2

--
-- Let it be
--
cylinder :: RealFloat a => a -> a -> a
cylinder radius height =
  let sideArea = 2 * pi * radius * height
      topArea = pi * radius * radius
   in sideArea + 2 * topArea

calcBMIs' :: RealFloat a => [(a, a)] -> [a]
calcBMIs' pairs =
  [bmi | (weight, height) <- pairs, let bmi = weight / height ^ 2]

--
-- Case expressions
--
describeList :: [a] -> String
describeList xs =
  "The list is " ++
  case xs of
    [] -> "empty."
    [_] -> "A singleton list."
    xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where
    what [] = "empty."
    what [x] = "a singleton list."
    what xs = "a longer list."
