module Erl.Data.List.Types
  ( NonEmptyList(..)
  , toList
  , nelCons
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldr)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Semigroup.Foldable (class Foldable1)
import Erl.Data.List (List, nil, (:))

newtype NonEmptyList a = NonEmptyList (NonEmpty List a)

toList :: NonEmptyList ~> List
toList (NonEmptyList (x :| xs)) = x : xs

nelCons :: forall a. a -> NonEmptyList a -> NonEmptyList a
nelCons a (NonEmptyList (b :| bs)) = NonEmptyList (a :| b : bs)

derive instance newtypeNonEmptyList :: Newtype (NonEmptyList a) _

derive newtype instance eqNonEmptyList :: Eq a => Eq (NonEmptyList a)
derive newtype instance ordNonEmptyList :: Ord a => Ord (NonEmptyList a)

instance showNonEmptyList :: Show a => Show (NonEmptyList a) where
  show (NonEmptyList nel) = "(NonEmptyList " <> show nel <> ")"

derive newtype instance functorNonEmptyList :: Functor NonEmptyList

instance applyNonEmptyList :: Apply NonEmptyList where
  apply (NonEmptyList (f :| fs)) (NonEmptyList (a :| as)) =
    NonEmptyList (f a :| (fs <*> a : nil) <> ((f : fs) <*> as))

instance applicativeNonEmptyList :: Applicative NonEmptyList where
  pure = NonEmptyList <<< NE.singleton

instance bindNonEmptyList :: Bind NonEmptyList where
  bind (NonEmptyList (a :| as)) f =
    case f a of
      NonEmptyList (b :| bs) ->
        NonEmptyList (b :| bs <> bind as (toList <<< f))

instance monadNonEmptyList :: Monad NonEmptyList

instance altNonEmptyList :: Alt NonEmptyList where
  alt = append

instance extendNonEmptyList :: Extend NonEmptyList where
  extend f w@(NonEmptyList (_ :| as)) =
    NonEmptyList (f w :| (foldr go { val: nil, acc: nil } as).val)
    where
    go a { val, acc } = { val: f (NonEmptyList (a :| acc)) : val, acc: a : acc }

instance comonadNonEmptyList :: Comonad NonEmptyList where
  extract (NonEmptyList (a :| _)) = a

instance semigroupNonEmptyList :: Semigroup (NonEmptyList a) where
  append (NonEmptyList (a :| as)) as' =
    NonEmptyList (a :| as <> toList as')

derive newtype instance foldableNonEmptyList :: Foldable NonEmptyList

-- derive newtype instance traversableNonEmptyList :: Traversable NonEmptyList

derive newtype instance foldable1NonEmptyList :: Foldable1 NonEmptyList

-- derive newtype instance unfoldable1NonEmptyList :: Unfoldable1 NonEmptyList

-- instance functorWithIndexNonEmptyList :: FunctorWithIndex Int NonEmptyList where
--   mapWithIndex fn (NonEmptyList ne) = NonEmptyList $ mapWithIndex (fn <<< maybe 0 (add 1)) ne

-- instance foldableWithIndexNonEmptyList :: FoldableWithIndex Int NonEmptyList where
--   foldMapWithIndex f (NonEmptyList ne) = foldMapWithIndex (f <<< maybe 0 (add 1)) ne
--   foldlWithIndex f b (NonEmptyList ne) = foldlWithIndex (f <<< maybe 0 (add 1)) b ne
--   foldrWithIndex f b (NonEmptyList ne) = foldrWithIndex (f <<< maybe 0 (add 1)) b ne

-- instance traversableWithIndexNonEmptyList :: TraversableWithIndex Int NonEmptyList where
--   traverseWithIndex f (NonEmptyList ne) = NonEmptyList <$> traverseWithIndex (f <<< maybe 0 (add 1)) ne

-- instance traversable1NonEmptyList :: Traversable1 NonEmptyList where
--   traverse1 f (NonEmptyList (a :| as)) =
--     foldl (\acc -> lift2 (flip nelCons) acc <<< f) (pure <$> f a) as
--       <#> case _ of NonEmptyList (x :| xs) → foldl (flip nelCons) (pure x) xs
--   sequence1 = traverse1 identity