module Erl.Data.List.Types
  ( NonEmptyList(..)
  , toList
  , nelCons
  , List
  , nil
  , cons
  , uncons
  , null
  , (:)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.MonadPlus (class Alternative, class MonadPlus, class MonadZero, class Plus)
import Data.Compactable (separateDefault)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Filterable (class Compactable, class Filterable)
import Data.Filterable as Filterable
import Data.Foldable (class Foldable, foldMapDefaultR, foldl, foldr, intercalate)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldMapWithIndexDefaultR, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Ord (class Ord1, compare1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Traversable (class Traversable1, traverse1)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)
import Data.Witherable (class Witherable, wiltDefault, witherDefault)

foreign import data List :: Type -> Type

foreign import nil :: forall a. List a

foreign import cons :: forall a. a -> List a -> List a

infixr 6 cons as :

--
-- | Break a list into its first element, and the remaining elements,
-- | or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons = unconsImpl Just Nothing

foreign import unconsImpl :: forall a b. (b -> Maybe b) -> Maybe b -> List a -> Maybe { head :: a, tail :: List a }

-- | Test whether a list is empty.
-- |
-- | Running time: `O(1)`
foreign import null :: forall a. List a -> Boolean

instance showList :: Show a => Show (List a) where
  show xs | null xs = "nil"
  show xs = "(" <> intercalate " : " (show <$> xs) <> " : nil)"

instance eqList :: Eq a => Eq (List a) where
  eq = eq1

instance eq1List :: Eq1 List where
  eq1 xs ys = go xs ys true
    where
      go _ _ false = false
      go xs' ys' acc =
        case uncons xs', uncons ys' of
          Nothing, Nothing -> acc
          Just { head: x, tail: xs'' }, Just { head: y, tail: ys'' } -> go xs'' ys'' $ acc && (y == x)
          _, _ -> false
         
instance ordList :: Ord a => Ord (List a) where
  compare = compare1

-- Adapted from https://hackage.haskell.org/package/base-4.4.1.0/docs/src/GHC-Classes.html
instance ord1List :: Ord1 List where
  compare1 xs ys =
      case uncons xs, uncons ys of
         Nothing, Nothing -> EQ
         Nothing, _       -> LT
         _, Nothing       -> GT
         Just { head: x, tail: xs'' }, Just { head: y, tail: ys'' } ->
           case compare x y of
             EQ -> compare1 xs'' ys''
             other -> other

instance semigroupList :: Semigroup (List a) where
  append = appendImpl

foreign import appendImpl :: forall a. List a -> List a -> List a

instance monoidList :: Monoid (List a) where
  mempty = nil

instance functorList :: Functor List where
  map = mapImpl

instance foldableList :: Foldable List where
  foldr = foldrImpl
  foldl = foldlImpl
  foldMap = foldMapDefaultR

foreign import mapImpl :: forall a b. (a -> b) -> List a -> List b

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> List a -> b

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> List a -> b

instance unfoldable1List :: Unfoldable1 List where
  unfoldr1 f b = go b nil
    where
      go source memo = case f source of
        Tuple one Nothing -> foldl (flip (:)) nil (one : memo)
        Tuple one (Just rest) -> go rest (one : memo)

instance unfoldableList :: Unfoldable List where
  unfoldr f b = go b nil
    where
      go source memo = case f source of
        Nothing -> foldl (flip (:)) nil memo
        Just (Tuple one rest) -> go rest (one : memo)

instance traversableList :: Traversable List where
  traverse f lst =
    case uncons lst of
      Nothing -> pure nil
      Just { head: h, tail: t } -> cons <$> f h <*> traverse f t
  sequence lst =
    case uncons lst of
      Nothing -> pure nil
      Just { head: h, tail: t } -> cons <$> h <*> sequence t

instance traversableWithIndexList :: TraversableWithIndex Int List where
  traverseWithIndex f lst =
    traverseWithIndexImpl f lst 0
      where 
          traverseWithIndexImpl f lst i = 
            case uncons lst of
              Nothing -> pure nil
              Just { head: h, tail: t } -> cons <$> f i h <*> traverseWithIndexImpl f t (i+1)

instance foldableWithIndexList :: FoldableWithIndex Int List where
  foldrWithIndex f z lst = foldr (\(Tuple i x) y -> f i x y) z $ mapWithIndex Tuple lst
  foldlWithIndex f z lst = foldl (\y (Tuple i x) -> f i y x) z $ mapWithIndex Tuple lst
  foldMapWithIndex f = foldMapWithIndexDefaultR f

instance functorWithIndexList :: FunctorWithIndex Int List where
  mapWithIndex f lst = foldl (flip (:)) nil $ go 0 lst nil
    where
    go n l acc = case uncons l of
      Nothing -> acc
      Just { head: x, tail: xs } -> go (n+1) xs $ (f n x) : acc

instance applyList :: Apply List where
  apply list xs =
    case uncons list of
      Nothing -> nil
      Just { head: f, tail: fs } -> (f <$> xs) <> (fs <*> xs)

instance applicativeList :: Applicative List where
  pure a = a : nil

instance bindList :: Bind List where
  bind l f = case uncons l of
    Nothing -> nil
    Just { head: x, tail: xs } -> f x <> bind xs f

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = nil

instance alternativeList :: Alternative List

instance monadZeroList :: MonadZero List

instance monadPlusList :: MonadPlus List

-- | Apply a function to each element in a list, keeping only the results which
-- | contain a value.
-- |
-- | Running time: `O(n)`
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f = go nil
  where
  go acc l = case uncons l of
    Nothing -> foldl (flip (:)) nil acc
    Just { head: x, tail: xs } -> 
      case f x of
        Nothing -> go acc xs
        Just y -> go (y : acc) xs

-- | Filter a list, keeping the elements which satisfy a predicate function.
-- |
-- | Running time: `O(n)`
foreign import filter :: forall a. (a -> Boolean) -> List a -> List a

instance compactableList :: Compactable List where
  compact = mapMaybe identity
  separate xs = separateDefault xs

instance filterableList :: Filterable List where
  partitionMap :: forall a l r. (a -> Either l r) -> List a -> { left :: List l, right :: List r }
  partitionMap p xs = foldr select { left: nil, right: nil } xs
      where
        select x { left, right } = case p x of
                                     Left l -> { left: l : left, right }
                                     Right r -> { left, right: r : right }
  
  partition :: forall a. (a -> Boolean) -> List a -> { no :: List a, yes :: List a }
  partition p xs = foldr select { no: nil, yes: nil } xs
      where
        -- select :: (a -> Boolean) -> a -> { no :: List a, yes :: List a } -> { no :: List a, yes :: List a }
        select x { no, yes } = if p x
                                 then { no, yes: x : yes }
                                 else { no: x : no, yes }

  
  filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
  filterMap = mapMaybe

  filter :: forall a. (a -> Boolean) -> List a -> List a
  filter = filter

instance witherableList :: Witherable List where
  wilt = wiltDefault
  wither = witherDefault

--------------------------------------------------------------------------------
-- NonEmpty---------------------------------------------------------------------
--------------------------------------------------------------------------------


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

derive newtype instance traversableNonEmptyList :: Traversable NonEmptyList

derive newtype instance foldable1NonEmptyList :: Foldable1 NonEmptyList

derive newtype instance unfoldable1NonEmptyList :: Unfoldable1 NonEmptyList

instance functorWithIndexNonEmptyList :: FunctorWithIndex Int NonEmptyList where
  mapWithIndex fn (NonEmptyList ne) = NonEmptyList $ mapWithIndex (fn <<< maybe 0 (add 1)) ne

instance foldableWithIndexNonEmptyList :: FoldableWithIndex Int NonEmptyList where
  foldMapWithIndex f (NonEmptyList ne) = foldMapWithIndex (f <<< maybe 0 (add 1)) ne
  foldlWithIndex f b (NonEmptyList ne) = foldlWithIndex (f <<< maybe 0 (add 1)) b ne
  foldrWithIndex f b (NonEmptyList ne) = foldrWithIndex (f <<< maybe 0 (add 1)) b ne

instance traversableWithIndexNonEmptyList :: TraversableWithIndex Int NonEmptyList where
  traverseWithIndex f (NonEmptyList ne) = NonEmptyList <$> traverseWithIndex (f <<< maybe 0 (add 1)) ne

instance traversable1NonEmptyList :: Traversable1 NonEmptyList where
  traverse1 f (NonEmptyList (a :| as)) =
    foldl (\acc -> lift2 (flip nelCons) acc <<< f) (pure <$> f a) as
      <#> case _ of NonEmptyList (x :| xs) → foldl (flip nelCons) (pure x) xs
  sequence1 = traverse1 identity
