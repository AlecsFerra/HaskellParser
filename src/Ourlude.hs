module Ourlude
  ( module Prelude,
    (|>),
    (<|),
    (>>>),
    (<<<),
    foldMapM,
    mapLeft,
    first,
    second,
  )
where

import Prelude
import Data.Bifunctor (first, second)

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (f >>> Left) Right

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat

foldMap :: Monoid b => (a -> b) -> [a] -> b
foldMap f = mconcat . fmap f

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) = (.)
{-# INLINE (<<<) #-}

infixr 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = flip (.)
{-# INLINE (>>>) #-}
