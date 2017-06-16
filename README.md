Here is a basic example:

```haskell
module Kinder.Functor where

main :: IO ()
main = putStrLn "That was easy!"
```

```haskell
-- $
-- >>> main
-- That was easy!
```

# Literate Haskell

This README.md file is a literate haskell file, for use with [`markdown-unlit`](https://github.com/sol/markdown-unlit#readme).
To allow GHC to recognize it, it's softlinked as `Kinder/Functor.lhs`, which you can compile with

    $ ghc -pgmL markdown-unlit Kinder/Functor.lhs

Many of the above examples are [`doctest`](https://github.com/sol/doctest#readme)-compatible, and can be run with

    $ doctest -pgmL markdown-unlit Kinder/Functor.lhs

Alternately, you can have cabal manage the dependencies and compile and test this with:

    $ cabal build
    $ cabal test
