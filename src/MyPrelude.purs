module MyPrelude
  ( module X
  , indexOf
  , indexIn
  , fix
  ) where

import Data.Void
  ( Void
  , absurd
  ) as X

import Data.Boolean
  ( otherwise
  ) as X

import Data.Unit
  ( Unit
  , unit
  ) as X

import Data.Tuple
  ( Tuple(..)
  , fst
  , snd
  , curry
  , uncurry
  ) as X

import Data.Tuple.Nested
  ( type (/\)
  , (/\)
  ) as X

import Data.Maybe
  ( Maybe(..)
  , isJust
  , isNothing
  , fromJust
  , maybe
  ) as X

import Data.Either
  ( Either(..)
  ) as X

import Data.Function
  ( ($)
  , (#)
  , const
  , flip
  ) as X

import Data.Show
  ( class Show
  , show
  ) as X

import Data.Eq
  ( class Eq
  , (==)
  , (/=)
  ) as X

import Data.Ord
  ( class Ord
  , (<=)
  , (>=)
  , (<)
  , (>)
  , min
  , max
  , compare
  , comparing
  , between
  , clamp
  ) as X

import Data.Ordering
  ( Ordering(..)
  ) as X

import Data.Bounded
  ( class Bounded
  , top
  , bottom
  ) as X

import Data.Foldable
  ( class Foldable
  , foldr
  , foldl
  , fold
  , foldM
  , all
  , any
  , sum
  , product
  , indexl
  , indexr
  , length
  , null
  ) as X

import Data.Traversable
  ( class Traversable
  , traverse
  , sequence
  , for
  ) as X

import Data.Semiring
  ( class Semiring
  , (+)
  , (*)
  ) as X

import Data.Ring
  ( class Ring
  , (-)
  ) as X

import Data.EuclideanRing
  ( class EuclideanRing
  , (/)
  , mod
  , gcd
  , lcm
  ) as X

import Data.Functor
  ( class Functor
  , map
  , (<$>)
  , ($>)
  , (<$)
  , (<#>)
  , flap
  , (<@>)
  , void
  ) as X

import Control.Apply
  ( class Apply
  , apply
  , (<*>)
  , (<*)
  , (*>)
  ) as X

import Control.Applicative
  ( class Applicative
  , pure
  , liftA1
  , when
  , unless
  ) as X

import Control.Bind
  ( class Bind
  , bind
  , (>>=)
  , (=<<)
  , (>=>)
  , (<=<)
  , join
  , class Discard
  , discard
  ) as X

import Control.Monad
  ( class Monad
  , ap
  , liftM1
  , ifM
  , whenM
  , unlessM
  ) as X

import Data.HeytingAlgebra
  ( class HeytingAlgebra
  , (&&)
  , (||)
  , not
  ) as X

import Data.Semigroup
  ( class Semigroup
  , (<>)
  , append
  ) as X

import Data.Monoid
  ( class Monoid
  , mempty
  , guard
  ) as X

import Control.Semigroupoid
  ( class Semigroupoid
  , (<<<)
  , (>>>)
  ) as X

import Control.Category
  ( class Category
  , identity
  ) as X

import Data.Newtype
  ( class Newtype
  , un
  , ala
  ) as X

import Data.Monoid.Endo
  ( Endo(..)
  ) as X

import Effect
  ( Effect
  ) as X

import Data.NaturalTransformation
  ( type (~>)
  ) as X

import Partial.Unsafe
  ( unsafePartial
  ) as X

--

import Control.Category ((<<<))
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.HeytingAlgebra ((&&))
import Data.Semiring ((+))
import Data.Eq (class Eq, (==))
import Data.Function (flip)

-- Trivial port of Elm 'fix' implementation from Rosetta Code
data Mu a b = Roll (Mu a b -> a -> b)
fix :: forall a b. ((a -> b) -> (a -> b)) -> (a -> b)
fix f = let g r = f (\v -> unroll r r v) in g (Roll g)
  where unroll (Roll x) = x

indexOf :: forall f a. Eq a => Foldable f => a -> f a -> Maybe Int
indexOf it = fst <<< foldr (\x (r /\ i) -> let r' = if isNothing r && x == it then Just i else r in r' /\ (i + 1)) (Nothing /\ 0)

indexIn :: forall f a. Eq a => Foldable f => f a -> a -> Maybe Int
indexIn = flip indexOf
