module Task5_1 where

import Control.Exception
import Todo(todo)

data IndexTooLargeException = IndexTooLargeException

instance Exception IndexTooLargeException where

instance Show IndexTooLargeException where
    show _ = "Index too large"

data NegativeIndexException = NegativeIndexException

instance Exception NegativeIndexException where

instance Show NegativeIndexException where
    show _ = "Negative index"

data DList a = DNil
             | DCons {
                left :: (DList a),
                current :: a,
                right :: (DList a)
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) =
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
index d i
  | i < 0     = throw NegativeIndexException
  | otherwise = index' d i
  where
    index' DNil _ = throw IndexTooLargeException
    index' d 0    = current d
    index' d i    = index' (right d) (i - 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt list index value = todo

removeAt :: DList a -> Int -> DList a
removeAt list index = todo
