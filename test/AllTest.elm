import Check as C
import Check.Test as T
import ElmTest as ET
import Graphics.Element as GE
import HashDictTest exposing (..)
import HashSetTest exposing (..)

main : GE.Element
main =
    [ hashDictSuite
    , hashSetSuite
    ]
    |> C.suite "elm-hash-collections"
    |> C.quickCheck
    |> T.evidenceToTest
    |> ET.elementRunner
