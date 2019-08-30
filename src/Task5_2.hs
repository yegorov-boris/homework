module Task5_2 where

import Todo(todo)
import Data.Ratio

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)

sinPrecisions :: Double -> Stream Double
sinPrecisions a = fmap fst $ generate (a', 0) $ uncurry newState
  where
    a' = let x' = a / (2*pi) in 2*pi*(x' - fromIntegral (round x'))
    newState s n = let nn   = succ n
                       sins = sin a' nn
                       ns   = if nn `mod` 2 == 0 then s + sins else s - sins
                   in (ns, nn)
    sin x n = let m = 2*n + 1 in sin' x x m

ePrecisions :: Stream Rational
ePrecisions = fmap fst $ generate (2, 1) $ uncurry newState
  where
    newState s n = let nn = succ n in (s + (1 / (fromInteger $ fact 1 nn)), nn)

sin' :: Double -> Double -> Int -> Double
sin' s _ 1 = s
sin' s a n = sin' (s*a / (fromIntegral n)) a (n - 1)

fact :: Integer -> Integer -> Integer
fact a 1 = a
fact a n = fact (a*n) (n - 1)
