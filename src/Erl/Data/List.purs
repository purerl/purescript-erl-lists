module Erl.Data.List
  ( module Erl.Data.List.Types
  , module Exports
  , toUnfoldable
  , fromFoldable
  , singleton
  , range
  , (..)
  , length
  , snoc
  , unsnoc
  , insert
  , insertBy
  , head
  , last
  , tail
  , init
  , index
  , (!!)
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , alterAt
  , reverse
  , concat
  , concatMap
  , filterM
  , catMaybes
  , sort
  , sortBy
  , merge
  , mergeBy
  , slice
  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  , group
  , group'
  , groupBy
  , nub
  , nubBy
  , union
  , unionBy
  , delete
  , deleteBy
  , (\\)
  , difference
  , intersect
  , intersectBy
  , zipWith
  , zipWithA
  , zip
  , transpose
  , unzip
  , foldM
  ) where

import Prelude

import Data.Filterable (filter, filterMap)
import Data.Filterable (filter, filterMap) as Exports
import Data.Foldable (class Foldable, any, foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex) as Exports
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Erl.Data.List.Types (List, NonEmptyList(..), cons, nil, null, uncons, (:))

-- | Convert a list into any unfoldable structure.
-- |
-- | Running time: `O(n)`
toUnfoldable :: forall f a. Unfoldable f => List a -> f a
toUnfoldable = unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> uncons xs)

-- | Construct a list from a foldable structure.
-- |
-- | Running time: `O(n)`
fromFoldable :: forall f a. Foldable f => f a -> List a
fromFoldable = foldr cons nil

--------------------------------------------------------------------------------
-- List creation ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create a list with a single element.
-- |
-- | Running time: `O(1)`
singleton :: forall a. a -> List a
singleton a = a : nil

-- | An infix synonym for `range`.
infix 8 range as ..

foreign import rangeImpl :: Int -> Int -> Int -> List Int

-- | Create a list containing a range of integers, including both endpoints.
range :: Int -> Int -> List Int
range n m = rangeImpl n m (if n > m then -1 else 1)


-- -- | Attempt a computation multiple times, requiring at least one success.
-- -- |
-- -- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- -- | termination.
-- some :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
-- some v = Cons <$> v <*> defer (\_ -> many v)
--
-- -- | Attempt a computation multiple times, returning as many successful results
-- -- | as possible (possibly zero).
-- -- |
-- -- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- -- | termination.
-- many :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
-- many v = some v <|> pure Nil
--
--------------------------------------------------------------------------------
-- List size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the length of a list
-- |
-- | Running time: `O(n)`
foreign import length :: forall a. List a -> Int

--------------------------------------------------------------------------------
-- Extending lists -------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- | Append an element to the end of a list, creating a new list.
-- |
-- | Running time: `O(2n)`
snoc :: forall a. List a -> a -> List a
snoc xs x = reverse (x : reverse xs)

-- | Insert an element into a sorted list.
-- |
-- | Running time: `O(n)`
insert :: forall a. Ord a => a -> List a -> List a
insert = insertBy compare

-- | Insert an element into a sorted list, using the specified function to
-- | determine the ordering of elements.
-- |
-- | Running time: `O(n)`
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp x ys = case uncons ys of
  Nothing -> x : nil
  Just { head: y, tail: ys' } ->
    case cmp x y of
        GT -> y : (insertBy cmp x ys')
        _  -> x : ys

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`.
head :: forall a. List a -> Maybe a
head = map _.head <<< uncons

-- | Get the last element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`.
last :: forall a. List a -> Maybe a
last list = case uncons list of
  Just { head: h, tail: t } | null t -> Just h
  Just { tail: t } -> last t
  _ -> Nothing

-- | Get all but the first element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
tail :: forall a. List a -> Maybe (List a)
tail = map _.tail <<< uncons

-- | Get all but the last element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`
init :: forall a. List a -> Maybe (List a)
init lst = _.init <$> unsnoc lst



-- | Break a list into its last element, and the preceding elements,
-- | or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`
unsnoc :: forall a. List a -> Maybe { init :: List a, last :: a }
unsnoc lst = (\h -> { init: reverse h.revInit, last: h.last }) <$> go lst nil
  where
  go lst' acc = case uncons lst' of
    Just { head: h, tail: t } | null t -> Just { revInit: acc, last: h }
    Just { head: h, tail: t } -> go t $ h : acc
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the element at the specified index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)` where `n` is the required index.
index :: forall a. List a -> Int -> Maybe a
index l i = case uncons l of
  Nothing -> Nothing
  Just { head: a } | i == 0 -> Just a
  Just { tail: as } -> index as (i - 1)

-- | An infix synonym for `index`.
infixl 8 index as !!

-- | Find the index of the first element equal to the specified element.
elemIndex :: forall a. Eq a => a -> List a -> Maybe Int
elemIndex x = findIndex (_ == x)

-- | Find the index of the last element equal to the specified element.
elemLastIndex :: forall a. Eq a => a -> List a -> Maybe Int
elemLastIndex x = findLastIndex (_ == x)

-- | Find the first index for which a predicate holds.
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn = go 0
  where
  go :: Int -> List a -> Maybe Int
  go n l = case uncons l of 
    Just { head: x, tail: xs } | fn x -> Just n
                               | otherwise -> go (n + 1) xs
    Nothing -> Nothing

-- | Find the last index for which a predicate holds.
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = ((length xs - 1) - _) <$> findIndex fn (reverse xs)

-- | Insert an element into a list at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
insertAt :: forall a. Int -> a -> List a -> Maybe (List a)
insertAt 0 x xs = Just (x : xs)
insertAt n x ys'
  | Just { head: y, tail: ys } <- uncons ys'
  = (y : _) <$> insertAt (n - 1) x ys
insertAt _ _ _  = Nothing

-- | Delete an element from a list at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
deleteAt :: forall a. Int -> List a -> Maybe (List a)
deleteAt n l = case uncons l of 
  Just { head: y, tail: ys }
    | n == 0 -> Just ys
    | otherwise -> (y:_) <$> deleteAt (n - 1) ys
  Nothing -> Nothing

-- | Update the element at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
updateAt :: forall a. Int -> a -> List a -> Maybe (List a)
updateAt n x l = case uncons l of
  Just { head: y, tail: ys }
    | n == 0 -> Just (x : ys)
    | otherwise -> (y:_) <$> updateAt (n - 1) x ys
  Nothing -> Nothing

-- | Update the element at the specified index by applying a function to
-- | the current value, returning a new list or `Nothing` if the index is
-- | out-of-bounds.
-- |
-- | Running time: `O(n)`
modifyAt :: forall a. Int -> (a -> a) -> List a -> Maybe (List a)
modifyAt n f = alterAt n (Just <<< f)

-- | Update or delete the element at the specified index by applying a
-- | function to the current value, returning a new list or `Nothing` if the
-- | index is out-of-bounds.
-- |
-- | Running time: `O(n)`
alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> Maybe (List a)
alterAt n f l = case uncons l of
  Just { head: y, tail: ys }
    | n == 0 -> Just $ case f y of
                        Nothing -> ys
                        Just y' -> y' : ys
    | otherwise -> (y:_) <$> alterAt (n - 1) f ys
  Nothing -> Nothing

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Reverse a list.
-- |
-- | Running time: `O(n)`
foreign import reverse :: forall a. List a -> List a
--
-- | Flatten a list of lists.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
foreign import concat :: forall a. List (List a) -> List a

-- | Apply a function to each element in a list, and flatten the results
-- | into a single, new list.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
concatMap :: forall a b. (a -> List b) -> List a -> List b
concatMap f list =
  case uncons list of
    Nothing -> nil
    Just { head: h, tail: t } -> f h <> concatMap f t

-- | Filter where the predicate returns a monadic `Boolean`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | powerSet :: forall a. [a] -> [[a]]
-- | powerSet = filterM (const [true, false])
-- | ```
filterM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM p = uncons >>> case _ of
  Nothing -> pure nil
  Just { head: x, tail: xs } -> do
    b <- p x
    xs' <- filterM p xs
    pure if b then x : xs' else xs'

-- | Filter a list of optional values, keeping only the elements which contain
-- | a value.
catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = filterMap identity
   
--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Sort the elements of an list in increasing order.
sort :: forall a. Ord a => List a -> List a
sort xs = sortBy compare xs

-- | Sort the elements of a list in increasing order, where elements are
-- | compared using the specified ordering.
sortBy :: forall a. (a -> a -> Ordering) -> List a -> List a
sortBy cmp = mergeAll <<< sequences
  -- implementation lifted from http://hackage.haskell.org/package/base-4.8.0.0/docs/src/Data-OldList.html#sort
  where
  sequences :: List a -> List (List a)
  sequences xs0 
    | Just { head: a, tail } <- uncons xs0
    , Just { head: b, tail: xs } <- uncons tail =
      case a `cmp` b of
        GT -> descending b (singleton a) xs
        _ -> ascending b (a : _) xs
  sequences xs = singleton xs

  descending :: a -> List a -> List a -> List (List a)
  descending a as bs0
    | Just { head: b, tail: bs } <- uncons bs0
    , a `cmp` b == GT = descending b (a : as) bs
  descending a as bs = (a : as) : (sequences bs)

  ascending :: a -> (List a -> List a) -> List a -> List (List a)
  ascending a as bs0
    | Just { head: b, tail: bs } <- uncons bs0
    , a `cmp` b /= GT = ascending b (\ys -> as (a : ys)) bs
  ascending a as bs = (cons (as $ singleton a) (sequences bs))

  mergeAll :: List (List a) -> List a
  mergeAll xs
    | Just { head: x, tail } <- uncons xs
    , null tail = x
  mergeAll xs = mergeAll (mergePairs xs)

  mergePairs :: List (List a) -> List (List a)
  mergePairs xs0 
    | Just { head: a, tail } <- uncons xs0
    , Just { head: b, tail: xs} <- uncons tail =
        mergeBy cmp a b : mergePairs xs
  mergePairs xs = xs

merge :: forall a. Ord a => List a -> List a -> List a
merge = mergeBy compare

mergeBy :: forall a. (a -> a -> Ordering) -> List a -> List a -> List a
mergeBy cmp as bs = case uncons as, uncons bs of
  Just { head: a, tail: as' }, Just { head: b, tail: bs' }
    | a `cmp` b == GT -> b : (mergeBy cmp as bs')
    | otherwise -> a : (mergeBy cmp as' bs)
  Nothing, _ -> bs
  _, Nothing -> as

--------------------------------------------------------------------------------
-- Sublists --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Extract a sublist by a start and end index.
slice :: forall a. Int -> Int -> List a -> List a
slice start end xs = take (end - start) (drop start xs)

-- | Take the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to take.
take :: forall a. Int -> List a -> List a
take = go nil
  where
  go acc 0 _ = reverse acc
  go acc n l = case uncons l of
    Nothing -> reverse acc
    Just { head: x, tail: xs } -> go (x : acc) (n - 1) xs

-- | Take those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile p = go nil
  where
  go acc l | Just { head: x, tail: xs } <- uncons l
           , p x = go (x : acc) xs
  go acc _ = reverse acc

-- | Drop the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to drop.
drop :: forall a. Int -> List a -> List a
drop 0 xs = xs
drop n l = case uncons l of 
  Nothing -> nil
  Just { head: _, tail: xs } -> drop (n - 1) xs

-- | Drop those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p = go
  where
  go l | Just { head: x, tail: xs } <- uncons l
       , p x = go xs
  go xs = xs

-- | Split a list into two parts:
-- |
-- | 1. the longest initial segment for which all elements satisfy the specified predicate
-- | 2. the remaining elements
-- |
-- | For example,
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) (1 : 3 : 2 : 4 : 5 : Nil) == { init: (1 : 3 : Nil), rest: (2 : 4 : 5 : Nil) }
-- | ```
-- |
-- | Running time: `O(n)`
span :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
span p xs = case uncons xs of
  Just { head: x, tail: xs' } | p x -> case span p xs' of
    { init: ys, rest: zs } -> { init: x:ys, rest: zs }
  _ -> { init: nil, rest: xs }

-- | Group equal, consecutive elements of a list into lists.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group (1 : 1 : 2 : 2 : 1 : Nil) == (1 : 1 : Nil) : (2 : 2 : Nil) : (1 : Nil) : Nil
-- | ```
-- |
-- | Running time: `O(n)`
group :: forall a. Eq a => List a -> List (NonEmptyList a)
group = groupBy (==)

-- | Sort and then group the elements of a list into lists.
-- |
-- | ```purescript
-- | group' [1,1,2,2,1] == [[1,1,1],[2,2]]
-- | ```
group' :: forall a. Ord a => List a -> List (NonEmptyList a)
group' = group <<< sort

-- | Group equal, consecutive elements of a list into lists, using the specified
-- | equivalence relation to determine equality.
-- |
-- | Running time: `O(n)`
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (NonEmptyList a)
groupBy eq = uncons >>> case _ of
  Nothing -> nil
  Just { head: x, tail: xs } -> case span (eq x) xs of
    { init: ys, rest: zs } -> NonEmptyList (x :| ys) : (groupBy eq zs)

--------------------------------------------------------------------------------
-- Set-like operations ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Remove duplicate elements from a list.
-- |
-- | Running time: `O(n^2)`
nub :: forall a. Eq a => List a -> List a
nub = nubBy eq

-- | Remove duplicate elements from a list, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
nubBy eq' l = case uncons l of 
  Nothing -> nil
  Just { head: x, tail: xs } -> x : nubBy eq' (filter (\y -> not (eq' x y)) xs)

-- | Calculate the union of two lists.
-- |
-- | Running time: `O(n^2)`
union :: forall a. Eq a => List a -> List a -> List a
union = unionBy (==)

-- | Calculate the union of two lists, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | Delete the first occurrence of an element from a list.
-- |
-- | Running time: `O(n)`
delete :: forall a. Eq a => a -> List a -> List a
delete = deleteBy (==)

-- | Delete the first occurrence of an element from a list, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n)`
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
deleteBy eq' x l = case uncons l of 
  Nothing -> nil
  Just { head: y, tail: ys }
    | eq' x y -> ys
    | otherwise -> y : (deleteBy eq' x ys)

infix 5 difference as \\

-- | Delete the first occurrence of each element in the second list from the first list.
-- |
-- | Running time: `O(n^2)`
difference :: forall a. Eq a => List a -> List a -> List a
difference = foldl (flip delete)

-- | Calculate the intersection of two lists.
-- |
-- | Running time: `O(n^2)`
intersect :: forall a. Eq a => List a -> List a -> List a
intersect = intersectBy (==)

-- | Calculate the intersection of two lists, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
intersectBy _  xs _   | Nothing <- uncons xs = nil
intersectBy _  _  ys  | Nothing <- uncons ys = nil
intersectBy eq xs ys  = filter (\x -> any (eq x) ys) xs

--------------------------------------------------------------------------------
-- Zipping ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Apply a function to pairs of elements at the same positions in two lists,
-- | collecting the results in a new list.
-- |
-- | If one list is longer, elements will be discarded from the longer list.
-- |
-- | For example
-- |
-- | ```purescript
-- | zipWith (*) (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 Nil) == 4 : 10 : 18 : Nil
-- | ```
-- |
-- | Running time: `O(min(m, n))`
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = reverse $ go xs ys nil
  where
  go l1 l2 acc = case uncons l1, uncons l2 of
    Nothing, _ -> acc
    _, Nothing -> acc
    Just { head: a, tail: as }, Just { head: b, tail: bs } -> go as bs $ (f a b) : acc

-- | A generalization of `zipWith` which accumulates results in some `Applicative`
-- | functor.
zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> List a -> List b -> m (List c)
zipWithA f xs ys = sequence (zipWith f xs ys)

-- | Collect pairs of elements at the same positions in two lists.
-- |
-- | Running time: `O(min(m, n))`
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip = zipWith Tuple

-- | Transforms a list of pairs into a list of first components and a list of
-- | second components.
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip = foldr (\(Tuple a b) (Tuple as bs) -> Tuple (a : as) (b : bs)) (Tuple nil nil)

--------------------------------------------------------------------------------
-- Transpose -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The 'transpose' function transposes the rows and columns of its argument.
-- | For example,
-- |
-- |     transpose ((1:2:3:Nil) : (4:5:6:Nil) : Nil) ==
-- |       ((1:4:Nil) : (2:5:Nil) : (3:6:Nil) : Nil)
-- |
-- | If some of the rows are shorter than the following rows, their elements are skipped:
-- |
-- |     transpose ((10:11:Nil) : (20:Nil) : Nil : (30:31:32:Nil) : Nil) ==
-- |       ((10:20:30:Nil) : (11:31:Nil) : (32:Nil) : Nil)
transpose :: forall a. List (List a) -> List (List a)
transpose l = case uncons l of
  Nothing -> nil
  Just { head: h0, tail: xss } ->
    case uncons h0 of
      Nothing -> transpose xss
      Just { head: x, tail: xs } -> (x : filterMap head xss) : transpose (xs : filterMap tail xss)

--------------------------------------------------------------------------------
-- Folding ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Perform a fold using a monadic step function.
foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM f a l = case uncons l of
  Nothing -> pure a
  Just { head: b, tail: bs } -> f a b >>= \a' -> foldM f a' bs
