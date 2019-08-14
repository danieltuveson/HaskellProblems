-- file: ch03/ListADT.hs

data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Possibly a = Only a 
                | None 

toList :: List a -> [a]
toList (Cons x y) = x : toList y
toList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


data MaybeTree a = MaybeNode (Maybe (a, MaybeTree a, MaybeTree a))
                   deriving (Show)

maybeTreeToTree :: MaybeTree a -> Tree a 
maybeTreeToTree (MaybeNode (Just (a, b, c))) = Node a (maybeTreeToTree b) (maybeTreeToTree c) 
maybeTreeToTree (MaybeNode Nothing) = Empty 
