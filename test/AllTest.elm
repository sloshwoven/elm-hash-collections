import Check as C
import Check.Test as T
import ElmTest as ET
import Graphics.Element as GE
import HashDictTest as HDQC
import HashDictUnitTest as HDU
import HashSetTest as HSQC

main : GE.Element
main =
    let
        qcHashDictTest =
            C.quickCheck HDQC.hashDictSuite
            |> T.evidenceToTest
        qcHashSetTest =
            C.quickCheck HSQC.hashSetSuite
            |> T.evidenceToTest
    in
        ET.suite "elm-hash-collections"
            [ qcHashDictTest
            , qcHashSetTest
            , HDU.hashDictUnitSuite
            ]
        |> ET.elementRunner
