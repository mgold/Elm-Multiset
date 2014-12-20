module Multiset where

{-| A Multiset is a mapping from comparable keys to Int counts. What makes a
Multiset special is that you never have to worry about Maybe values; instead
you'll get zero. This simplifies the insert and query functions.

Multisets do not store counts of zero explicitly, so don't expect them in `map`,
`filter`, `counts`, and the like. You should equate multisets with the given
`equals` function and not the built-in `(==)`. Multisets support negative
counts.

# Definition
@docs Multiset

# Create
@docs empty, singleton, fromList, fromAssocList

# Update
@docs add, remove, set, update

# Query
@docs get, equals, keys, counts, toList

# Transform
@docs map, map2, foldl, foldr, filter, partition

-}

import Dict
import Maybe
import Set
import List

{-| A Multiset is a wrapper around
[Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict), but you
should treat the type as opaque — except when you're trying to solve type
inference errors. -}
type alias Multiset comparable = Dict.Dict comparable Int

{-| Create an empty multiset. -}
empty : Multiset comparable
empty = Dict.empty

{-| Create a multiset containing the key once. -}
singleton : comparable -> Multiset comparable
singleton x = Dict.singleton x 1

{-| Set the count of a key to a new integer, regardness of what it used to be. -}
set : comparable -> Int -> Multiset comparable -> Multiset comparable
set x c ms = if c == 0 then Dict.remove x ms else Dict.insert x c ms

{-| Add a key to the multiset, incrementing its count by one. -}
add : comparable -> Multiset comparable -> Multiset comparable
add x = update x (\c -> c + 1)

{-| Remove a key from the multiset, decrementing its count by one. -}
remove : comparable -> Multiset comparable -> Multiset comparable
remove x = update x (\c -> c - 1)

{-| Update the count of a comparable using the given function. It is safe to
return zero. -}
update : comparable -> (Int -> Int) -> Multiset comparable -> Multiset comparable
update x f = let toMaybe c = if c == 0 then Nothing else Just c
             in Dict.update x <| Maybe.withDefault 0 >> f >> toMaybe

{-| Get the count of a key, which may be zero. -}
get : comparable -> Multiset comparable -> Int
get x ms = Dict.get x ms |> Maybe.withDefault 0

{- Map over a multiset, changing the counts of the keys. It is safe to return
zero. -}
map : (comparable -> Int -> Int) -> Multiset comparable -> Multiset comparable
map f ms = Dict.map f ms |> Dict.filter (\_ i -> i /= 0)

{- Map over two multisets, producing a new multiset. It is safe to return zero.
You can use this function to add two multisets: `map2 (always (+))`. Similar
approaches will substract or find the max or min of multisets. -}
map2 : (comparable -> Int -> Int-> Int) -> Multiset comparable -> Multiset comparable -> Multiset comparable
map2 f a b = let keySet = Dict.keys >> Set.fromList
                 allKeys = Set.union (keySet a) (keySet b) |> Set.toList
                 assocList = List.map (\x -> (x, f x (get x a) (get x b))) allKeys
                 assocList' = List.filter (\(x,c) -> c /= 0) assocList
             in Dict.fromList assocList'

{- Equate two multisets. You should use this function instead of `(==)`, which
equates the tree representation rather than the abstract container. You can also
use this function and `empty` for empty checking.

    empty `equals` (fromAssocList [("foo", 0)]) == True
    empty `equals` singleton "bar" == False
    (fromList [1,2,1]) `equals` (fromList [1,1,2]) == True

-}
equals : Multiset comparable -> Multiset comparable -> Bool
equals a b = let keySet = Dict.keys >> Set.fromList
                 allKeys = Set.union (keySet a) (keySet b) |> Set.toList
             in List.all (\x -> Dict.get x a  == Dict.get x b) allKeys

{-| Get all of the keys in a multiset. -}
keys : Multiset comparable -> List comparable
keys = Dict.keys

{-| Get all of the counts in a multiset. -}
counts : Multiset comparable -> List Int
counts = Dict.values

{-| Convert a multiset into an association list of key-count pairs. -}
toList : Multiset comparable -> List (comparable, Int)
toList = Dict.toList

{-| Convert a list of keys into a multiset, counting repeats. -}
fromList : List comparable -> Multiset comparable
fromList = List.foldl add empty

{-| Convert an association list of key-count pairs into a multiset. -}
fromAssocList : List (comparable, Int) -> Multiset comparable
fromAssocList = List.filter (\p -> snd p /= 0) >> Dict.fromList

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
contains all key-count pairs which satisfy the predicate, and the second
contains the rest. -}
partition : (comparable -> Int -> Bool) -> Multiset comparable -> (Multiset comparable, Multiset comparable)
partition = Dict.partition
