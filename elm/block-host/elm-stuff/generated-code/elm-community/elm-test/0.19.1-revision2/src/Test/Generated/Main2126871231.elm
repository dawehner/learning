module Test.Generated.Main2126871231 exposing (main)

import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 361434307357869, processes = 8, paths = ["/Users/dawehner/Documents/Projects/learning/elm/block-host/tests/Example.elm"]}