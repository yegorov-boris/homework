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
remove t x = refreshLinks $ makeBlack $ del x t
  where makeBlack (T _ _ a y b) = T E B a y b
        makeBlack E           = E

del :: (Ord a) => a -> LinkedTree a -> LinkedTree a
del x t@(T _ _ l y r)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse l r

delL :: (Ord a) => a -> LinkedTree a -> LinkedTree a
delL x t@(T _ B t1 y t2) = balL $ T E B (del x t1) y t2
delL x t@(T _ R t1 y t2) = T E R (del x t1) y t2

balL :: LinkedTree a -> LinkedTree a
balL (T _ B (T _ R t1 x t2) y t3) = T E R (T E B t1 x t2) y t3
balL (T _ B t1 y (T _ B t2 z t3)) = balance' (T E B t1 y (T E R t2 z t3))
balL (T _ B t1 y (T _ R (T _ B t2 u t3) z t4@(T _ B l value r))) =
  T E R (T E B t1 y t2) u (balance' (T E B t3 z (T E R l value r)))

delR :: (Ord a) => a -> LinkedTree a -> LinkedTree a
delR x t@(T _ B t1 y t2) = balR $ T E B t1 y (del x t2)
delR x t@(T _ R t1 y t2) = T E R t1 y (del x t2)

balR :: LinkedTree a -> LinkedTree a
balR (T _ B t1 y (T _ R t2 x t3)) = T E R t1 y (T E B t2 x t3)
balR (T _ B (T _ B t1 z t2) y t3) = balance' (T E B (T E R t1 z t2) y t3)
balR (T _ B (T _ R t1@(T _ B l value r) z (T _ B t2 u t3)) y t4) =
  T E R (balance' (T E B (T E R l value r) z t2)) u (T E B t3 y t4)

fuse :: LinkedTree a -> LinkedTree a -> LinkedTree a
fuse E t = t
fuse t E = t
fuse t1@(T _ B _ _ _) (T _ R t3 y t4) = T E R (fuse t1 t3) y t4
fuse (T _ R t1 x t2) t3@(T _ B _ _ _) = T E R t1 x (fuse t2 t3)
fuse (T _ R t1 x t2) (T _ R t3 y t4)  =
  let s = fuse t2 t3
  in case s of
       (T _ R s1 z s2) -> (T E R (T E R t1 x s1) z (T E R s2 y t4))
       (T _ B _ _ _)   -> (T E R t1 x (T E R s y t4))
fuse (T _ B t1 x t2) (T _ B t3 y t4)  =
  let s = fuse t2 t3
  in case s of
       (T _ R s1 z s2) -> (T E R (T E B t1 x s1) z (T E B s2 y t4))
       (T _ B s1 z s2) -> balL (T E B t1 x (T E B s y t4))

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

balance' :: LinkedTree a -> LinkedTree a
balance' E = E
balance' (T _ c l v r) = balance c l v r

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
