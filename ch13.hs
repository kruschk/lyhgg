import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Ratio
import System.Random

--
-- Writer? I hardly know her!
--
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, log') = f x in (y, log `mappend` log')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do a <- logNumber 3
                 b <- logNumber 5
                 tell ["Gonna multiply these two"]
                 return (a*b)

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `rem` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a 0 = do tell ["Finished with " ++ show a]
               return a
gcd'' a b = do tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
               gcd'' b (a `rem` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a 0 = do tell ["Finished with " ++ show a]
                    return a
gcdReverse a b = do result <- gcdReverse b (a `rem` b)
                    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
                    return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    DiffList f <> DiffList g = DiffList (f . g) --(\xs -> f (g xs))

instance Monoid (DiffList a) where
    mempty = DiffList id --(\xs -> [] ++ xs)
    --(DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a 0 = do tell (toDiffList ["Finished with " ++ show a])
                     return a
gcdReverse' a b = do
    result <- gcdReverse' b (a `rem` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

something = mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 110 34

--
-- Tasteful stateful computations
--
type Stack = [Int]

--pop :: Stack -> (Int, Stack)
--pop (x : xs) = (x, xs)
--
--push :: Int -> Stack -> ((), Stack)
--push x xs = ((), x : xs)

--stackManip :: Stack -> (Int, Stack)
--stackManip stack = let ((), newStack1) = push 3 stack
--                       (a, newStack2) = pop newStack1
--                   in pop newStack2

--instance Monad (State s) where
--    return value = State $ \state -> (value, state)
--    (State f) >>= f' = State $ \state -> let (value, state') = f state
--                                             (State f'') = f' value
--                                         in f'' state'

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do a <- randomSt
                b <- randomSt
                c <- randomSt
                return (a, b, c)

--
-- Error error on the wall
--
--

--
-- Some useful monadic functions
--
solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe string = case reads string of [(x, "")] -> Just x
                                        _ -> Nothing

--
-- Making monads
--
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
    pure = return
    mf <*> mx = mf >>= (\f -> mx >>= (\x -> return (f x)))

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Eq, Show)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do a <- coin
               b <- coin
               c <- loadedCoin
               return (all (==Tails) [a,b,c])
