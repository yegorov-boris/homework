module Task6 where

import Todo(todo)

data Color = R | B

data LinkedTree a = E | T (LinkedTree a) Color (LinkedTree a) a (LinkedTree a)

find :: Ord a => LinkedTree a -> a -> Bool
find E _ = False
find (T _ _ l v r) x | x == v = True
find (T _ _ l v r) x | x < v = find l x
find (T _ _ l v r) x | otherwise = find r x

insert :: (Ord a) => LinkedTree a -> a -> LinkedTree a
insert s x = refreshLinks $ makeBlack $ ins s
  where ins E  = T E R E x E
        ins (T _ color a y b)
          | x < y  = balance color (ins a) y b
          | x == y = T E color a y b
          | x > y  = balance color a y (ins b)
        makeBlack (T p _ a y b) = T p B a y b

remove :: Ord a => LinkedTree a -> a -> LinkedTree a
remove s x = refreshLinks s

balance B (T _ R (T _ R a x b) y c) z d =
  T E R (T E B a x b) y (T E B c z d)
balance B (T _ R a x (T _ R b y c)) z d =
  T E R (T E B a x b) y (T E B c z d)
balance B a x (T _ R (T _ R b y c) z d) =
  T E R (T E B a x b) y (T E B c z d)
balance B a x (T _ R b y (T _ R c z d)) =
  T E R (T E B a x b) y (T E B c z d)
balance color left value right =
  T E color left value right

refreshLinks :: LinkedTree a -> LinkedTree a
refreshLinks t = setParent E t
  where
    setParent _ E = E
    setParent p (T _ c l v r) = let p' = T p c (setParent p' l) v (setParent p' r) in p'

getV :: LinkedTree a -> a
getV E = error "E"
getV (T _ _ _ v _) = v

getParent :: LinkedTree a -> LinkedTree a
getParent E = error "E"
getParent (T p _ _ _ _) = p

getL :: LinkedTree a -> LinkedTree a
getL E = error "E"
getL (T _ _ l _ _) = l

old = let p = T p B (T p B E 2 E) 1 E in p

new = let p = T p B (T p B (getL old) 4 E) 3 E in p

validNew = refreshLinks new
