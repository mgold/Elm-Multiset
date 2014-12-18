module Test where

import List

import Multiset as M
import ElmTest.Test as Test
import ElmTest.Assertion as Assert
import ElmTest.Runner.Element as Runner

equals a b = Test.test "Multisets are equal" <|
    Assert.assert <| M.equals a b

notEquals a b = Test.test "Multiset are NOT equal" <|
    Assert.assert <| not <| M.equals a b

k = "foo"
k1 = "bar"

-- of questionable use now that zeroes aren't removed
zeroRemoval = Test.suite "zero removal for equality"
    [ equals M.empty (M.singleton k |> M.remove k)
    , equals M.empty (M.singleton k |> M.set k 0)
    , equals M.empty (M.singleton k |> M.remove k)
    , equals M.empty (M.singleton k |> M.update k (always 0))
    , equals M.empty (M.singleton k |> M.map (always <| always 0))
    ]

docsExamples = Test.suite "Examples from the docs"
    [ equals M.empty (M.fromAssocList [("foo", 0)])
    , notEquals M.empty (M.singleton "bar")
    , equals (M.fromList [1,2,1]) (M.fromList [1,1,2])
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

main = Runner.runDisplay allTests
