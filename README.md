# purescript-erl-lists

This library provides FFI definitions for native Erlang lists and associated helper functions and type class instances.

Many of the basic functions are implemented by the Erlang native versions.

## Usage

```purescript

-- Construct
list :: List Int
list = 1 : 2 : 3 : nil -- Erlang representation is [1, 2, 3], i.e. a standard list

-- Destructure

-- (note that length exists...)
evenLength :: forall a. List a -> Boolean
evenLength lst =
  case uncons lst of
    Nothing -> true
    Just { head, tail } -> not (evenLength tail)

```
Instances are provided as per `Data.List`. Conversion to other datatypes can be achived with `toUnfoldable` and `fromFoldable`.

## Licensing

Much of this code is derived from [purescript-lists](https://github.com/purescript/purescript-lists).

Some of this code is derived from GHC's standard libraries (`base`);
according to its terms, we have included GHC's license in the file
`LICENSE-GHC.md`.
