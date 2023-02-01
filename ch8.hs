--
-- Algebraic data types intro
--
data Point =
  Point Float Float
  deriving (Show)

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (a + x) (b + y)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (a + x1) (b + y1)) (Point (a + x2) (b + x2))

baseCircle :: Float -> Shape
baseCircle radius = Circle (Point 0 0) radius

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

--
-- Record syntax
--
-- data Person =
--   Person String String Int Float String String
--   deriving (Show)
--
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname
--
-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname
--
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
--
-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height
--
-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number
--
-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor
data Person =
  Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
    }
  deriving (Eq, Read, Show)

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

data Car =
  Car
    { make :: String
    , model :: String
    , year :: Int
    }
  deriving (Show)

--
-- Type parameters
--
--data Maybe a = Nothing | Just a
data Vector a =
  Vector a a a
  deriving (Eq, Read, Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

--
-- Recursive data structures
--
--data List a = Empty | Cons a (List a) deriving (Eq, Ord, Read, Show)
infixr 5 :-:

data List a
  = Empty
  | a :-: (List a)
  deriving (Show, Read, Eq, Ord)

--data BinaryTree a
--  = Leaf a
--  | Node a (BinaryTree a) (BinaryTree a)
--  deriving (Eq, Read, Show)
data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Eq, Read, Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node y left right) =
  case compare x y of
    LT -> Node y (treeInsert x left) right
    EQ -> tree
    GT -> Node y left (treeInsert x right)

treeElem :: Ord t => t -> Tree t -> Bool
treeElem x EmptyTree = False
treeElem x tree@(Node y left right) =
  case compare x y of
    LT -> treeElem x left
    EQ -> True
    GT -> treeElem x right

--
-- Typeclasses 102
--
data TrafficLight
  = Red
  | Yellow
  | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

--
-- A yes-no typeclass
--
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult

--
-- The Functor typeclass
--
--class Functor f where
--  fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node value left right) = Node (f value) (fmap f left) (fmap f right)

--instance Functor (Data.Map.Map k) where
--    fmap = Data.Map.map

--
-- Kinds and some type-foo
--
class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank { frankField :: b a } deriving (Show)

instance Tofu Frank where
  tofu = Frank

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
