module Test where

import List

import Multiset as M
import ElmTest.Test as Test
import ElmTest.Runner.Element as Runner

k = "foo"
k1 = "bar"

zeroRemoval = Test.suite "zero removal for equality"
    [ Test.equals M.empty (M.singleton k |> M.remove k)
    , Test.equals M.empty (M.singleton k |> M.set k 0)
    , Test.equals M.empty (M.singleton k |> M.remove k)
    , Test.equals M.empty (M.singleton k |> M.update k (always 0))
    , Test.equals M.empty (M.singleton k |> M.map (always <| always 0))
    ]

lists = Test.suite "Creation from List"
    [ Test.equals (M.singleton k) (M.fromList [k])
    , Test.equals (M.empty |> M.set k 2) (M.fromList [k, k])
    , Test.equals (M.empty |> M.set k 7) (M.fromList <| List.repeat 7 k)
    , Test.equals (M.empty |> M.set k 2 |> M.add k1) (M.fromList [k,k1,k])
    , Test.suite "fromList ordering"
        [ Test.equals (M.fromList [k1, k, k]) (M.fromList [k,k1,k])
        , Test.equals (M.fromList [k1, k, k]) (M.fromList [k,k,k1])
        , Test.equals (M.fromList [k, k1, k]) (M.fromList [k,k,k1])
        ]
    ]

sum = M.map2 (always (+))

maps = Test.suite "Maps"
    [ Test.equals (sum (M.singleton k) (M.singleton k1))  (M.fromList [k1, k])
    , Test.equals (sum (M.singleton k) M.empty)  (M.singleton k)
    , Test.equals (sum (M.singleton k |> M.add k1) (M.singleton k))  (M.fromList [k,k,k1])
    , Test.equals (M.map2 (always min) (M.singleton k) M.empty) M.empty
    ]

allTests = Test.suite "All Tests" [zeroRemoval, lists, maps]

main = Runner.runDisplay allTests
