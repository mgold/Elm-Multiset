module Test where

import Multiset as M
import ElmTest.Test as Test
import ElmTest.Runner.Element as Runner

myTest = let ms = M.singleton "foo" |> M.remove "foo"
         in Test.equals ms M.empty

main = Runner.runDisplay myTest
