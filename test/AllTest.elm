import Check as C
import Check.Runner.Browser as B
import Html as H
import HashDictTest exposing (..)
import HashSetTest exposing (..)

main : H.Html
main =
    [ hashDictSuite
    , hashSetSuite
    ]
    |> C.suite "elm-hash-collections"
    |> C.quickCheck
    |> B.display
