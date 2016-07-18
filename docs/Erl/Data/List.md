## Module Erl.Data.List

#### `List`

``` purescript
data List :: * -> *
```

##### Instances
``` purescript
(Show a) => Show (List a)
(Eq a) => Eq (List a)
Semigroup (List a)
Monoid (List a)
Functor List
Foldable List
Unfoldable List
Traversable List
Apply List
Applicative List
Bind List
Monad List
Alt List
Plus List
Alternative List
MonadZero List
MonadPlus List
```

#### `nil`

``` purescript
nil :: forall a. List a
```

#### `cons`

``` purescript
cons :: forall a. a -> List a -> List a
```

#### `(:)`

``` purescript
infixr 6 cons as :
```

#### `toUnfoldable`

``` purescript
toUnfoldable :: forall f a. Unfoldable f => List a -> f a
```

Convert a list into any unfoldable structure.

Running time: `O(n)`

#### `fromFoldable`

``` purescript
fromFoldable :: forall f a. Foldable f => f a -> List a
```

Construct a list from a foldable structure.

Running time: `O(n)`

#### `singleton`

``` purescript
singleton :: forall a. a -> List a
```

Create a list with a single element.

Running time: `O(1)`

#### `(..)`

``` purescript
infix 8 range as ..
```

An infix synonym for `range`.

#### `range`

``` purescript
range :: Int -> Int -> List Int
```

Create a list containing a range of integers, including both endpoints.

#### `null`

``` purescript
null :: forall a. List a -> Boolean
```

Test whether a list is empty.

Running time: `O(1)`

#### `length`

``` purescript
length :: forall a. List a -> Int
```

Get the length of a list

Running time: `O(n)`

#### `head`

``` purescript
head :: forall a. List a -> Maybe a
```

Get the first element in a list, or `Nothing` if the list is empty.

Running time: `O(1)`.

#### `last`

``` purescript
last :: forall a. List a -> Maybe a
```

Get the last element in a list, or `Nothing` if the list is empty.

Running time: `O(n)`.

#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```

Get all but the first element of a list, or `Nothing` if the list is empty.

Running time: `O(1)`

#### `init`

``` purescript
init :: forall a. List a -> Maybe (List a)
```

Get all but the last element of a list, or `Nothing` if the list is empty.

Running time: `O(n)`

#### `uncons`

``` purescript
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
```

Break a list into its first element, and the remaining elements,
or `Nothing` if the list is empty.

Running time: `O(1)`

#### `reverse`

``` purescript
reverse :: forall a. List a -> List a
```

otherwise = go (n + 1) xs
Reverse a list.

Running time: `O(n)`

#### `concat`

``` purescript
concat :: forall a. List (List a) -> List a
```

Flatten a list of lists.

Running time: `O(n)`, where `n` is the total number of elements.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> List b) -> List a -> List b
```

Apply a function to each element in a list, and flatten the results
into a single, new list.

Running time: `O(n)`, where `n` is the total number of elements.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```

Filter a list, keeping the elements which satisfy a predicate function.

Running time: `O(n)`


