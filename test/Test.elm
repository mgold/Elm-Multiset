module Test (main) where

{-| Tests for elm-multiset.
@docs main
-}

import List
import Graphics.Element exposing (Element)

import Multiset as M
import ElmTest as Test

equals a b = Test.test "Multisets are equal" <| Test.assert <| M.equals a b

nativeEquals s a b = Test.test s <| Test.assertEqual a b

notEquals a b = Test.test "Multiset are NOT equal" <|
    Test.assert <| not <| M.equals a b

k = "foo"
k1 = "bar"

-- Using native equality to avoid Just 0 == Nothing
zeroRemoval = Test.suite "zero is removed by"
    [ nativeEquals "remove"  M.empty (M.singleton k |> M.remove k)
    , nativeEquals "set"  M.empty (M.singleton k |> M.set k 0)
    , nativeEquals "update"  M.empty (M.singleton k |> M.update k (always 0))
    , nativeEquals "map"  M.empty (M.singleton k |> M.map (\_ _->0))
    , nativeEquals "map2"  M.empty (M.map2 (\_ _ _->0) (M.singleton k) (M.singleton k1))
    ]

docsExamples = Test.suite "Examples from the docs"
    [ equals M.empty (M.fromAssocList [("foo", 0)])
    , notEquals M.empty (M.singleton "bar")
    , equals (M.fromList [1,2,1]) (M.fromList [1,1,2])
    , Test.suite "Negative numbers"
        [ equals (M.empty |> M.remove k) (M.fromAssocList [(k, -1)])
        , notEquals (M.empty |> M.remove k) M.empty
        , equals (M.empty |> M.remove k |> M.remove k) (M.fromAssocList [(k, -2)])
        , equals (M.empty |> M.remove k |> M.add k) M.empty
        ]
    ]

lists = Test.suite "Creation from List"
    [ equals (M.singleton k) (M.fromList [k])
    , equals (M.empty |> M.set k 2) (M.fromList [k, k])
    , equals (M.empty |> M.set k 7) (M.fromList <| List.repeat 7 k)
    , equals (M.empty |> M.set k 2 |> M.add k1) (M.fromList [k,k1,k])
    , Test.suite "fromList ordering"
        [ equals (M.fromList [k1, k, k]) (M.fromList [k,k1,k])
        , equals (M.fromList [k1, k, k]) (M.fromList [k,k,k1])
        , equals (M.fromList [k, k1, k]) (M.fromList [k,k,k1])
        ]
    ]

sum = M.map2 (always (+))

maps = Test.suite "Maps"
    [ equals (sum (M.singleton k) (M.singleton k1))  (M.fromList [k1, k])
    , equals (sum (M.singleton k) M.empty)  (M.singleton k)
    , equals (sum (M.singleton k |> M.add k1) (M.singleton k))  (M.fromList [k,k,k1])
    , equals (M.map2 (always min) (M.singleton k) M.empty) M.empty
    ]

allTests = Test.suite "All Tests" [zeroRemoval, docsExamples, lists, maps]

{-| -}
main : Element
main = Test.elementRunner allTests
