import Check as C
import Check.Runner.Browser as B
import Html as H
import HashDictTest exposing (..)

main : H.Html
main =
    [hashDictSuite]
    |> C.suite "elm-hash-collections suite"
    |> C.quickCheck
    |> B.display
