module Task6 where

import Todo(todo)

data Color = R | B

data LinkedTree a = E | T (LinkedTree a) Color (LinkedTree a) a (LinkedTree a)

find :: Ord a => LinkedTree a -> a -> Bool
find E _ = False
find (T _ _ l v r) x | x == v = True
find (T _ _ l v r) x | x < v = find l x
find (T _ _ l v r) x | otherwise = find r x

insert :: Ord a => LinkedTree a -> a -> LinkedTree a
insert s x = makeBlack $ ins s
  where
    ins E = let p = T p R E x E in p
    ins (T p color a y b) = T p color a y b
--                   | x < y  = balance color (ins a) y b
--                   | x == y = T color a y b
--                   | x > y  = balance color a y (ins b)
    makeBlack (T p _ a y b) = let p = T p B a y b in p

remove :: Ord a => LinkedTree a -> a -> LinkedTree a
remove = todo

--balance B (RBTree R (RBTree R a x b) y c) z d =
--  RBTree R (RBTree B a x b) y (RBTree B c z d)
--balance B (RBTree R a x (RBTree R b y c)) z d =
--  RBTree R (RBTree B a x b) y (RBTree B c z d)
--balance B a x (RBTree R (RBTree R b y c) z d) =
--  RBTree R (RBTree B a x b) y (RBTree B c z d)
--balance B a x (RBTree R b y (RBTree R c z d)) =
--  RBTree R (RBTree B a x b) y (RBTree B c z d)
--balance color left value right =
--  RBTree color left value right

getColor :: LinkedTree a -> String
getColor E = error "E"
getColor (T _ B _ _ _) = "b"
getColor (T _ R _ _ _) = "r"

getParent :: LinkedTree a -> LinkedTree a
getParent E = error "E"
getParent (T p _ _ _ _) = p
