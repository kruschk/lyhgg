import qualified Data.Foldable as Foldable

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Foldable.Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Node value left right) = Foldable.foldMap f left
                                        `mappend` f value
                                        `mappend` Foldable.foldMap f right

testTree = Node 5
                (Node 3
                      (Node 1 Empty Empty)
                      (Node 6 Empty Empty))
                (Node 9
                      (Node 8 Empty Empty)
                      (Node 10 Empty Empty))
