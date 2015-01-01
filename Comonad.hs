module Comonad where

class Functor f => Comonad f where
  extract :: f a -> a
  duplicate :: f a -> f (f a)
  (=>>) :: f a -> (f a -> b) -> f b

  x =>> f = fmap f (duplicate x)
