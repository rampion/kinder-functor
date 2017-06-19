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

newtype Opp cat a b = Opp { runOpp :: cat b a }

instance Category cat => Category (Opp cat) where
  id = Opp id
  Opp f . Opp g = Opp (g . f)

instance Functor (->) (->) ((->) a) where
  fmap = (.)

instance Functor (Opp (->)) (Natural (->)) (->) where
  fmap (Opp h) = Natural (.h)
