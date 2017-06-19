{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Kmett where

import Prelude hiding ((.), id, Functor(fmap))
import Control.Category
import Data.Constraint
import Data.Constraint.Forall

class (Category r, Category t) => Functor r t f | f r -> t, f t -> r where
  fmap :: r a b -> t (f a) (f b)

newtype Natural (cat :: k -> k -> *) (f :: j -> k) (g :: j -> k) = 
  Natural { runNatural :: forall x. cat (f x) (g x) }

instance Category cat => Category (Natural cat) where
  id = Natural id
  Natural c . Natural d = Natural (c . d)

type Contravariant r = Functor (Opp r)

contramap :: Contravariant r t f => r a b -> t (f b) (f a)
contramap = fmap . Opp

lmap :: Contravariant r (Natural t) f => r a b -> t (f b x) (f a x)
lmap = runNatural . contramap

newtype Opp cat a b = Opp { runOpp :: cat b a }

instance Category cat => Category (Opp cat) where
  id = Opp id
  Opp f . Opp g = Opp (g . f)

instance Functor (->) (->) ((->) a) where
  fmap = (.)

instance Functor (Opp (->)) (Natural (->)) (->) where
  fmap (Opp h) = Natural (.h)

type Profunctor r s t f = (Contravariant r (Natural t) f, ForallF (Functor s t) f)

dimap :: Profunctor r s t f => r a b -> s c d -> t (f b c) (f a d)
dimap = case instF of Sub d -> go d
  where go :: (Contravariant r (Natural t) f) => Dict (Functor s t (f a)) -> r a b -> s c d -> t (f b c) (f a d)
        go Dict f g = fmap g . lmap f
