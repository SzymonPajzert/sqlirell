module Util where

infixl 5 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) a f = f `fmap` a

infixl 5 |>
(|>) :: a -> (a -> b) -> b
(|>) a f = f a
