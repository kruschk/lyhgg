import Data.List (break)

--
-- Taking a walk
--
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty)
                                        (Node 'T' Empty Empty))
                              (Node 'Y' (Node 'S' Empty Empty)
                                        (Node 'A' Empty Empty)))
                    (Node 'L' (Node 'W' (Node 'C' Empty Empty)
                                        (Node 'R' Empty Empty))
                              (Node 'A' (Node 'A' Empty Empty)
                                        (Node 'C' Empty Empty)))

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

newTree = changeToP' [R,L] freeTree
p = elemAt [R,L] newTree

--
-- A trail of breadcrumbs
--
--type Breadcrumbs = [Direction]
--
--goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
--goLeft (Node _ l _, bs) = (l, L:bs)
--
--goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
--goRight (Node _ _ r, bs) = (r, R:bs)
--
--twople = goLeft (goRight (freeTree, []))

x -: f = f x

--twople' = (freeTree, []) -: goRight -: goLeft

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: Zipper a -> Zipper a
goUp (l, LeftCrumb x r:bs) = (Node x l r, bs)
goUp (r, RightCrumb x l:bs) = (Node x l r, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft
focus = farLeft -: attach (Node 'Z' Empty Empty)

topMost :: Zipper a -> Zipper a
topMost z@(_, []) = z
topMost z = topMost (goUp z)

--
-- Focusing on lists
--
type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

--
-- A very simple file system
--
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = Folder "root"
                [ File "goat_yelling_like_man.wmv" "baaaaaa"
                , File "pope_time.avi" "god bless"
                , Folder "pics"
                    [ File "ape_throwing_up.jpg" "bleargh"
                    , File "watermelon_smash.gif" "smash!!"
                    , File "skull_man(scary).bmp" "Yikes!"
                    ]
                , File "dijon_poupon.doc" "best mustard"
                , Folder "programs"
                    [ File "fartwizard.exe" "10gotofart"
                    , File "owl_bandit.dmg" "mov eax, h00t"
                    , File "not_a_virus.exe" "really not a virus"
                    , Folder "source code"
                        [ File "best_hs_prog.hs" "main = print (fix error)"
                        , File "random.hs" "main = print 4"
                        ]
                    ]
                ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ item : rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs)
    = let (ls, item:rs) = break (nameIs name) items
      in (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (File fileName _) = name == fileName
nameIs name (Folder folderName _) = name == folderName

focus' = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
focus'' = focus' -: fsUp -: fsTo "watermelon_smash.gif"

fsRename :: Name -> FSZipper -> FSZipper
fsRename name (File _ data', bs) = (File name data', bs)
fsRename name (Folder _ items, bs) = (Folder name items, bs)

focus''' = (myDisk,[]) -: fsTo "pics" -: fsRename "cspi" -: fsUp

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder name items, bs) = (Folder name (item:items), bs)

focus'''' = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp

--
-- Watch your step
--
goLeft'' :: Zipper a -> Maybe (Zipper a)
goLeft'' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft'' (Empty, _) = Nothing

goRight'' :: Zipper a -> Maybe (Zipper a)
goRight'' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight'' (Empty, _) = Nothing

maybeFocus = goLeft'' (Empty, [])
maybeFocus' = goLeft'' (Node 'A' Empty Empty, [])

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (_, []) = Nothing
goUp' (l, LeftCrumb x r:bs) = Just (Node x l r, bs)
goUp' (r, RightCrumb x l:bs) = Just (Node x l r, bs)

coolTree = Node 1 Empty (Node 3 Empty Empty)
focus''''' = return (coolTree,[]) >>= goRight''
focus'''''' = return (coolTree,[]) >>= goRight'' >>= goRight''
focus''''''' = return (coolTree,[]) >>= goRight'' >>= goRight'' >>= goRight''
