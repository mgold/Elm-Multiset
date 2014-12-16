module Multiset where

{-| A Multiset is a mapping from comparable keys to Int counts. What makes a
Multiset special is that you never have to worry about Maybe values; instead
you'll get zero. This simplifies the insert and query functions.

Multisets support equality checks with `(==)`, which requires that zeroes are
never stored explicitly. The implementation enforces this, though it means that
if you map or fold on a multiset, you won't see keys whose value is zero.

Multisets support negative counts.

# Definition
@docs Multiset

# Build
@docs empty, singleton, increment, decrement, set, update, remove

# Query
@docs get

# Lists
@docs keys, values, toList, fromList, fromAssocList

# Transform
@docs add, map, map2, foldl, foldr, filter, partition

-}

import Dict
import Maybe
import Set
import List

{-| A Multiset is a wrapper around
[Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict), but you
should treat the type as opaque. The only time you should think about the
underlying Dict implementation is when you're trying to solve type inference
errors. -}
type alias Multiset comparable = Dict.Dict comparable Int

{-| Create an empty multiset. -}
empty : Multiset comparable
empty = Dict.empty

{-| Create a multiset containing the key once. -}
singleton : comparable -> Multiset comparable
singleton x = Dict.singleton x 1

{-| Set the count of a key to a new integer, regardness of what it used to be. -}
set : comparable -> Int -> Multiset comparable -> Multiset comparable
set x count ms = if count == 0 then Dict.remove x ms else Dict.insert x count ms

{-| Increment the count of a key by one. -}
increment : comparable -> Multiset comparable -> Multiset comparable
increment x = update x (\c -> c + 1)

{-| Decrement the count of a key by one. -}
decrement : comparable -> Multiset comparable -> Multiset comparable
decrement x = update x (\c -> c - 1)

{-| Update the count of a comparable using the given function. It is safe to
return zero. -}
update : comparable -> (Int -> Int) -> Multiset comparable -> Multiset comparable
update x f = let toMaybe i = if i == 0 then Nothing else Just i
                 wrapper m_int = Maybe.withDefault 0 m_int |> f |> toMaybe
             in Dict.update x wrapper

{-| Set a key's count to zero. -}
remove : comparable -> Multiset comparable -> Multiset comparable
remove = Dict.remove

{-| Get the count of a key, which may be zero. -}
get : comparable -> Multiset comparable -> Int
get x ms = Dict.get x ms |> Maybe.withDefault 0

{- Add the counts of all keys in two multisets. -}
add : Multiset comparable -> Multiset comparable -> Multiset comparable
add = map2 (always (+))

{- Map over a multiset, changing the values of the keys. It is safe to return
zero. -}
map : (comparable -> Int -> Int) -> Multiset comparable -> Multiset comparable
map f ms = Dict.map f ms |> Dict.filter (\_ i -> i /= 0)

{- Map over two multisets, producing a new multiset. It is safe to return
zero. `add` is implemented using this function, and you can create subtract,
min, max, and so on with it.-}
map2 : (comparable -> Int -> Int-> Int) -> Multiset comparable -> Multiset comparable -> Multiset comparable
map2 f a b = let keySet = Dict.keys >> Set.fromList
                 allKeys = Set.union (keySet a) (keySet b) |> Set.toList
                 assocList = List.map (\x -> (x, f x (get x a) (get x b))) allKeys
                 assocList' = List.filter (\(x,c) -> c /= 0) assocList
             in Dict.fromList assocList'


{-| Get all of the keys in a multiset -}
keys : Multiset comparable -> List comparable
keys = Dict.keys

{-| Get all of the counts in a multiset -}
counts : Multiset comparable -> List Int
counts = Dict.values

{-| Convert a multiset into an association list of key-count pairs. -}
toList : Multiset comparable -> List (comparable, Int)
toList = Dict.toList

{-| Convert a list of values into a multiset, counting repeats. -}
fromList : List comparable -> Multiset comparable
fromList = List.foldl increment empty

{-| Convert an association list of key-count pairs into a multiset. -}
fromAssocList : List (comparable, Int) -> Multiset comparable
fromAssocList = Dict.fromList

{-| Fold over the key-count pairs in a dictionary, in order from lowest key to
highest key. -}
foldl : (comparable -> Int -> b -> b) -> b -> Multiset comparable -> b
foldl = Dict.foldl

{-| Fold over the key-count pairs in a dictionary, in order from highest key to
lowest key. -}
foldr : (comparable -> Int -> b -> b) -> b -> Multiset comparable -> b
foldr = Dict.foldr

{-| Keep a key-count pair when it satisfies a predicate. -}
filter : (comparable -> Int -> Bool) -> Multiset comparable -> Multiset comparable
filter = Dict.filter

{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest. -}
partition : (comparable -> Int -> Bool) -> Multiset comparable -> (Multiset comparable, Multiset comparable)
partition = Dict.partition
