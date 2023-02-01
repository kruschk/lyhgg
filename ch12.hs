--
-- Walk the line
--
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft birds (left, right)
    | abs (birds + left - right) < 4 = Just (birds + left, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight birds (left, right)
    | abs (birds + right - left) < 4 = Just (left, birds + right)
    | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

interesting = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft 1 >>= landLeft 3
interesting' = return (0,0) >>= landLeft 1 >>= landRight 4 >>= banana >>= landLeft 3
interesting'' = return (0,0) >>= landLeft 1 >>= landRight 4 >> Nothing >>= landLeft 3

--
-- do notation
--
foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do x <- Just 3
          y <- Just "!"
          Just (show x ++ y)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

--
-- The list monad
--
listOfTuples = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

listOfTuples' :: [(Int,Char)]
listOfTuples' = do  n <- [1,2]
                    ch <- ['a','b']
                    return (n,ch)

listOfTuples'' = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

--
-- A knight's quest
--
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r)
    = [(c',r') | (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                            ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                            ]
               , (c' `elem` [1..8] && r' `elem` [1..8])]
    --do (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    --              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    --   guard (c' `elem` [1..8] && r' `elem` [1..8])
    --   return (c',r')

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
