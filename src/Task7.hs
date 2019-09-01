module Task7 where

import Todo(todo)

data Deque a = Deque Int [a] Int [a]

-- Пустая очередь
empty :: Deque a
empty = Deque 0 [] 0 []

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront (Deque sizeF front sizeR rear) x =
  check $ Deque (sizeF + 1) (x : front) sizeR rear

-- Удаление из начала очереди
popFront :: Deque a -> (a, Deque a)
popFront (Deque _ [] _ []) = error "empty"
popFront (Deque _ [] _ [x]) = (x, empty)
popFront (Deque _ [] _ _) = error "unbalanced"
popFront (Deque sizeF (f : fs) sizeR rear) =
  (f, check $ Deque (sizeF - 1) fs sizeR rear)

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
pushBack (Deque sizeF front sizeR rear) x =
  check $ Deque sizeF front (sizeR + 1) (x : rear)

-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> (a, Deque a)
popBack (Deque _ [] _ []) = error "empty"
popBack (Deque _ [x] _ []) = (x, empty)
popBack (Deque _ _ _ []) = error "unbalanced"
popBack (Deque sizeF front sizeR (r : rs)) =
  (r, check $ Deque sizeF front (sizeR - 1) rs)

check :: Deque a -> Deque a
check q@(Deque sizeF front sizeR rear)
    | sizeF > c * sizeR + 1 =
        let front' = take size1 front
            rear' = rear ++ reverse (drop size1 front)
        in
        Deque size1 front' size2 rear'
    | sizeR > c * sizeF + 1 =
        let front' = front ++ reverse (drop size1 rear)
            rear' = take size1 rear
        in
        Deque size2 front' size1 rear'
    | otherwise = q
    where
        size1 = (sizeF + sizeR) `div` 2
        size2 = (sizeF + sizeR) - size1
        c = 4
