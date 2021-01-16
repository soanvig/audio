module Helpers where

  zipWithPadded :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
  zipWithPadded f a b (x:xs) (y:ys) = f x y : zipWithPadded f a b xs ys
  zipWithPadded f a _ []     ys     = map (f a) ys
  zipWithPadded f _ b xs     []     = map (`f` b) xs