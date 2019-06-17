One thing that's often bothered me about the `Functor` typeclass is how arbitrarily it seemed tied
to the *last* parameter of a type. It seemed unfair that `Either a b` and `(a, b)` couldn't be functors
over both their parameters, only `Functor1`s or some other such workaround.

So let's change that, and get you a `Functor` that can do both.

# Setup 
Since this is a literate haskell file, we need to specify all our language extensions up front.

```haskell
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PackageImports #-}
module Kinder.Functor where
```

We need to get some of the `Prelude` out of the way - we'll be replacing its `id`
and `(.)` with `Category`'s, and its `Functor` with our own.

```haskell
import Prelude hiding (Functor(fmap), id, (.))
import "base" Control.Category (Category(id, (.)))
```

Since we're using `TypeInType`, we need to import `Data.Kind` to bring the `*`
kind (and its synonym `Type`) into scope.

```haskell
import "base" Data.Kind (Type, type (*))
```

The rest of the imports are just for some example Functor instances.

```haskell
import "base" Data.Functor.Compose (Compose(Compose, getCompose))
import "base" Unsafe.Coerce (unsafeCoerce)
import "constraints" Data.Constraint (Dict(Dict), (:-)(Sub))
import "constraints" Data.Constraint.Forall (ForallF, instF)
```

# the Functor typeclass

Using the power of `TypeInType` and `TypeFamilies`, we declare that
for each kind `k`, there's a default category `Cat k`.

```haskell
type family Cat k :: k -> k -> *
```

Now we define the `Functor` typeclass so that given a type `f` that
maps objects in `j` to objects in `k`, we have `fmap` to map
arrows in `Cat j` to arrows in `Cat k`:

```haskell
class (Category (Cat j), Category (Cat k)) => Functor (f :: j -> k) where
  fmap :: Cat j a b -> Cat k (f a) (f b)
```

# Backwards compatibility with Prelude

The only types that have actual values we can manipulate at runtime are those
of kind `*`. There's several categories that we could use to map types
with values to each other, but the one that `Prelude`'s `Functor` typeclass
assumes is, no suprise, `(->)`, so let's use that.

```haskell
type instance Cat Type = (->) -- use *'s synonym Type here b/c GHC barfs otherwise
```

Having made this declaration, our `Functor` class is now compatible with `Prelude`'s.

Don't believe me? Ask GHCi.

```haskell
{-$-----------------------------------------------------------------------------
>>> :set -XTypeInType -XTypeApplications
>>> :t fmap
fmap
  :: forall k j (f :: j -> k) (b :: j) (a :: j).
     Functor f =>
     Cat j a b -> Cat k (f a) (f b)
>>> :t fmap @Type @Type
fmap @Type @Type :: Functor f => (a -> b) -> f a -> f b
-}
```

So now we can make all the normal `Functor` instances, and they
work as normal, no special annotations required:

```haskell
{-|-----------------------------------------------------------------------------
>>> fmap (+1) Nothing
Nothing
>>> fmap (+1) $ Just 10
Just 11
-}
instance Functor Maybe where
  fmap h = maybe Nothing (Just . h)

{-|-----------------------------------------------------------------------------
>>> import Data.Char (toUpper)
>>> fmap toUpper "hello world"
"HELLO WORLD"
-}
instance Functor [] where
  fmap = map

{-|-----------------------------------------------------------------------------
>>> fmap (+1) (0,0)
(0,1)
-}
instance Functor ((,) x) where
  fmap h (x,a) = (x, h a)

{-|-----------------------------------------------------------------------------
>>> fmap (+1) $ Left 0
Left 0
>>> fmap (+1) $ Right 0
Right 1
-}
instance Functor (Either x) where
  fmap h = either Left (Right . h)

{-|-----------------------------------------------------------------------------
>>> fmap (+1) (+2) 3
6
-}
instance Functor ((->) x) where
  fmap = (.)

{-|-----------------------------------------------------------------------------
>>> import Data.Char (toUpper)
>>> fmap toUpper . Compose $ words "hello world"
Compose ["HELLO","WORLD"]
-}
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h = Compose . fmap (fmap h) . getCompose
```

# Natural transformations

But now you may ask "What about the other side of the pair? What about mapping over `Right`? I thought
we were going to do those too?" And right you are.

All unsaturated parameterized types have kinds that look like `a -> b -> c -> ... -> *`. Just like
the `(->)` type, the `(->)` kind really only considers one argument at a time, so if we want to
operate over the other parameters of some type, it suffices to consider types of kind `j -> k`.

```haskell
type instance Cat (j -> k) = Natural (Cat k)
```

`Natural` lets us lift an arrow between types of kind `k` to an arrow
between types of kind `j -> k` by just asserting that the parameter
of kind `j` doesn't matter, as long as it's the same before and
after.

```haskell
newtype Natural (cat :: k -> k -> *) (f :: j -> k) (g :: j -> k) = 
  Natural { runNatural :: forall x. cat (f x) (g x) }

instance Category cat => Category (Natural cat) where
  id = Natural id
  Natural c . Natural d = Natural (c . d)
```

I'm going to handwave in the direction of the many discussions
of natural transformations in Haskell that I've stolen this idea from.

So now we can define additional `Functor` instances for `(,)` and `Either`
that operate on the first parameter:

```haskell
fmap1 :: (Category (Cat j), Category (Cat k), Functor f) => Cat j a b -> Cat k (f a x) (f b x)
fmap1 = runNatural . fmap

{-|-----------------------------------------------------------------------------
>>> fmap1 (+1) (0,0)
(1,0)
-}
instance Functor (,) where
  fmap h = Natural $ \(a, x) -> (h a, x)

{-|-----------------------------------------------------------------------------
>>> fmap1 (+1) $ Left 0
Left 1
>>> fmap1 (+1) $ Right 0
Right 0
-}
instance Functor Either where
  fmap h = Natural $ either (Left . h) Right
```

But that's not all. We can also view `Compose f g a` as a functor in its first
two parameters, only instead of using arrows from `Cat Type`, we use arrows
from `Cat (Type -> Type)`

```haskell
{-|-----------------------------------------------------------------------------
>>> import Data.Maybe (listToMaybe)
>>> fmap1 (Natural listToMaybe) . Compose $ words "hello world"
Compose [Just 'h',Just 'w']
-}
instance Functor f => Functor (Compose f) where
  fmap h = Natural $ Compose . fmap (runNatural h) . getCompose

fmap2 :: (Category (Cat j), Category (Cat k), Functor f) => Cat j a b -> Cat k (f a x y) (f b x y)
fmap2 = runNatural . fmap1

{-|-----------------------------------------------------------------------------
>>> import Data.Maybe (listToMaybe)
>>> fmap2 (Natural listToMaybe) . Compose $ words "hello world"
Compose (Just "hello")
-}
instance Functor Compose where
  fmap h = Natural $ Natural $ Compose . runNatural h . getCompose
```

Like `(->)` or any other category, `Natural cat f g` is a functor over
its last parameter.

```haskell
{-|-----------------------------------------------------------------------------
>>> import Data.Maybe (maybeToList)
>>> let rightToMay (Left _) = Nothing ; rightToMay (Right a) = Just a
>>> :t fmap (Natural maybeToList) (Natural rightToMay)
fmap (Natural maybeToList) (Natural rightToMay)
  :: Natural (->) (Either t) []
-}
instance (Category cat, Cat k ~ cat) => Functor (Natural cat f) where
  fmap = (.)
```

It's also a functor over its first parameter, i.e. we can alter the category.

```haskell
{-|-----------------------------------------------------------------------------
>>> import Control.Arrow (Kleisli(..))
>>> :t Kleisli . fmap return
Kleisli . fmap return :: Monad m => (a -> b) -> Kleisli m a b
>>> import Data.Maybe (listToMaybe)
>>> :t fmap2 (Natural (Natural (Kleisli . fmap return))) (Natural listToMaybe)
fmap2 (Natural (Natural (Kleisli . fmap return))) (Natural listToMaybe)
  :: Monad m => Natural (Kleisli m) [] Maybe
-}
instance Functor Natural where
  fmap h = Natural $ Natural $ \(Natural h') -> Natural $ runNatural (runNatural h) h'
```

# Other Kinds

`Natural` gives us an alternate destination category for `fmap`, one that lets us
map over non-terminal parameters of a parameterized type. 

But our definition of `Functor` also allows us to choose an alternate source
category for `fmap`, so we're not limited to parameters of kind `*`.

We saw this for some of the `Functor` instances for `Compose` and `Natural` itself, but
there's other kinds than `*` and `j -> k`.

For instance, consider the Peano numbers:

```haskell
data Peano = Zero | Succ Peano
```

Using the power of `DataKinds` (automatically included with `TypeInType`), we can promote
values of type `Peano` into types of kind `Peano`.

A canonical example that uses these types is `SPeano`, called a singleton type because for
any `n :: Peano`, `SPeano n` has exactly one value.

```haskell
data SPeano (n :: Peano) where
  SZero :: SPeano 'Zero
  SSucc :: SPeano n -> SPeano ('Succ n)

instance Show (SPeano n) where
  show = \sn -> show (toInt sn 0) where
    toInt :: SPeano m -> Int -> Int
    toInt SZero m = m
    toInt (SSucc sm) m = toInt sm $! 1 + m
```

In order to make `SPeano` a functor, we need a way of representing arrows
between types of kind `Peano`. 

In the absense of creativity, let's call this category `CPeano`:

```haskell
type instance Cat Peano = CPeano

data CPeano (m :: Peano) (n :: Peano) where
  CSucc :: CPeano m n -> CPeano m ('Succ n)
  CId :: CPeano m m
  CPred :: CPeano m n -> CPeano ('Succ m) n

instance Show (CPeano m n) where
  show = \cn -> "(" ++ showSigned (toInt cn 0) ++ ")" where
    showSigned n | n < 0 = show n
    showSigned n | otherwise = '+' : show n

    toInt :: CPeano m' n' -> Int -> Int
    toInt CId n = n
    toInt (CSucc cn) n = toInt cn $ n + 1
    toInt (CPred cn) n = toInt cn $ n - 1

instance Category CPeano where
  id = CId

  CId . cn = cn
  cm . CId = cm
  CSucc cm . cn@(CSucc _) = CSucc (cm . cn)
  cm@(CPred _) . CPred cn = CPred (cm . cn)
  CPred cm . CSucc cn = cm . cn
  CSucc cm . CPred cn = coerceBy bisucc (cm . cn) where

bisucc :: CPeano m n -> CPeano ('Succ m) ('Succ n)
bisucc CId = CId
bisucc (CSucc cm) = CSucc (bisucc cm)
bisucc (CPred cm) = CPred (bisucc cm)

coerceBy :: (a -> b) -> a -> b
coerceBy _ = unsafeCoerce
```

Now that we can encode our arrows, we can define a `Functor` instance for `SPeano`:

```haskell
{-|-----------------------------------------------------------------------------
>>> let minus2 = CPred (CPred CId)
>>> let three = SSucc (SSucc (SSucc SZero))
>>> :t fmap minus2 three
fmap minus2 three :: SPeano ('Succ 'Zero)
>>> fmap minus2 three
1
-}
instance Functor SPeano where
  fmap CId sn = sn
  fmap (CSucc cm) sn = SSucc (fmap cm sn)
  fmap (CPred cm) (SSucc sn) = fmap cm sn
```

And, for that matter, for `CPeano` itself:

```haskell
{-|-----------------------------------------------------------------------------
>>> let minus2 = CPred (CPred CId)
>>> let plus3 = CSucc (CSucc (CSucc CId))
>>> fmap minus2 plus3
(+1)
>>> :t fmap minus2 plus3
fmap minus2 plus3 :: CPeano m ('Succ m)
>>> fmap plus3 minus2
(+1)
>>> :t fmap plus3 minus2
fmap plus3 minus2
  :: CPeano ('Succ ('Succ n)) ('Succ ('Succ ('Succ n)))
-}
instance Functor (CPeano m) where
  fmap = (.)
```

# Contravariant functors, profunctors, and bifunctors

What we've been dealing with so far is *covariant* functors, but there are also
*contravariant* functors, functors that reverse the arrows direction when lifting
it from one category to another.

```haskell
class (Category (Cat j), Category (Cat k)) => Contravariant (f :: j -> k) where
  contramap :: Cat j a b -> Cat k (f b) (f a)
```

`(->)`, `Natural cat` and `CPeano` all have `Contravariant` instances that
allow them to modify their inputs:

```haskell
-- types are often contravariant in their last-but-one parameter
lmap :: Contravariant f => Cat j a b -> Cat k (f b x) (f a x)
lmap = runNatural . contramap

{-|-----------------------------------------------------------------------------
>>> :t lmap show length
lmap show length :: Show a => a -> Int
-}
instance Contravariant (->) where
  contramap h = Natural (.h)

{-|-----------------------------------------------------------------------------
>>> import Data.Maybe (maybeToList)
>>> let rightToMay (Left _) = Nothing ; rightToMay (Right a) = Just a
>>> :t lmap (Natural rightToMay) (Natural maybeToList)
lmap (Natural rightToMay) (Natural maybeToList)
  :: Natural (->) (Either t) []
-}
instance (Category cat, Cat k ~ cat) => Contravariant (Natural cat) where
  contramap h = Natural (.h)

{-|-----------------------------------------------------------------------------
>>> let minus2 = CPred (CPred CId)
>>> let plus3 = CSucc (CSucc (CSucc CId))
>>> lmap minus2 plus3
(+1)
>>> :t lmap minus2 plus3
lmap minus2 plus3
  :: CPeano ('Succ ('Succ m)) ('Succ ('Succ ('Succ m)))
>>> lmap plus3 minus2
(+1)
>>> :t lmap plus3 minus2
lmap plus3 minus2 :: CPeano a ('Succ a)
-}
instance Contravariant CPeano where
  contramap h = Natural (.h)
```

Of course, there's a special name for types that are contravariant in one
parameter and covariant in the next -
[`Profunctor`s](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors)!

Unlike the [standard
implementation](http://hackage.haskell.org/package/profunctors), we don't need
a new typeclass for `Profunctor`s, we can get away with just an alias.

```haskell
type Profunctor f = (Contravariant f, ForallF Functor f)

{-|-----------------------------------------------------------------------------
>>> :t dimap show show length
dimap show show length :: Show a => a -> String
-}
dimap :: Profunctor f => Cat j a b -> Cat j' c d -> Cat k (f b c) (f a d)
dimap = case instF of Sub d -> go d
  where go :: (Contravariant f) => Dict (Functor (f a)) -> Cat j a b -> Cat j' c d -> Cat k (f b c) (f a d)
        go Dict f g = fmap g . lmap f
```

While we're at it, we could define another alias for [`Bifunctor`s, types that are covariant in two
adjacent parameters](https://hackage.haskell.org/package/bifunctors):

```haskell
type Bifunctor f = (Functor f, ForallF Functor f)

{-|-----------------------------------------------------------------------------
>>> bimap (+1) (+2) (0,0)
(1,2)
-}
bimap :: Bifunctor f => Cat j a b -> Cat j' c d -> Cat k (f a c) (f b d)
bimap = case instF of Sub d -> go d
  where go :: (Functor f) => Dict (Functor (f b)) -> Cat j a b -> Cat j' c d -> Cat k (f a c) (f b d)
        go Dict f g = fmap g . fmap1 f
```

# Limitations

Of course, this alternate definition of `Functor` has its own limitations. 

## A more categorical definition

The above `Functor` definition is very similar to Edward Kmett's "more
categorical definition of `Functor`" in the
[`categories`](http://hackage.haskell.org/package/categories-1.0.7/docs/Control-Categorical-Functor.html)
package:

    class (Category r, Category t) => Functor r t f | f r -> t, f t -> r where
      fmap :: r a b -> t (f a) (f b)

This typeclass obviates the need for a `Contravariant` class, as you can just choose
flip the direction of the first category:

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

In order to define a `Contravariant` in terms of our definition of `Functor`,
we'd need the ability to define values of types of kind other than than `*`,
which Haskell doesn't yet support.

(I actually tweaked `categories`' `Functor` definition very slightly - I made
it polykinded by just enablying `PolyKinds` and I permuted the parameter order
to make definining something like `Profunctor` easier)

    type Profunctor r s t f = (Contravariant r (Natural t) f, ForallF (Functor s t) f)

    dimap :: Profunctor r s t f => r a b -> s c d -> t (f b c) (f a d)
    dimap = case instF of Sub d -> go d
      where go :: (Contravariant r (Natural t) f) => Dict (Functor s t (f a)) -> r a b -> s c d -> t (f b c) (f a d)
            go Dict f g = fmap g . lmap f

## Categories and `DefaultSignatures`

You might have noticed that `(->)`, `Compose`, and `CNat`, all of which have `Category` instances,
all have identitical instances of `Functor` and `Contravariant`.  In fact, this will be true
for any `Category`, which makes it a great opportunity to use the `DefaultSignatures` language extension:

    class (Category (Cat j), Category (Cat k)) => Functor (f :: j -> k) where
      fmap :: Cat j a b -> Cat k (f a) (f b)

      default fmap :: (f ~ Cat j x, k ~ Type) => Cat j a b -> Cat j x a -> Cat j x b
      fmap = (.)

    class (Category (Cat j), Category (Cat k)) => Contravariant (f :: j -> k) where
      contramap :: Cat j a b -> Cat k (f b) (f a)

      default contramap :: (f ~ Cat j, k ~ j -> Type) => Cat j a b -> Natural (->) (f b) (f a)
      contramap h = Natural (.h)

However this doesn't compile in `ghc-8.0.2` [due to limitations in the type
system](https://github.com/rampion/kinder-functor/issues/1), though this may be
possible in a future version of GHC.

# Literate Haskell

This README.md file is a literate haskell file, for use with [`markdown-unlit`](https://github.com/sol/markdown-unlit#readme).
To allow GHC to recognize it, it's softlinked as `Kinder/Functor.lhs`, which you can compile with

    $ ghc -pgmL markdown-unlit Kinder/Functor.lhs

Many of the above examples are [`doctest`](https://github.com/sol/doctest#readme)-compatible, and can be run with

    $ doctest -pgmL markdown-unlit Kinder/Functor.lhs

Alternately, you can have cabal manage the dependencies and compile and test this with:

    $ cabal build
    $ cabal test
