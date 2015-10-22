module HashDictTest (hashDictSuite) where

import Check as C

hashDictSuite : C.Claim
hashDictSuite =
    [ buildSuite
    , querySuite
    , combineSuite
    , listsSuite
    , transformSuite
    ]
    |> C.suite "HashDict suite"

buildSuite : C.Claim
buildSuite =
    C.suite "build suite" []

querySuite : C.Claim
querySuite =
    C.suite "query suite" []

combineSuite : C.Claim
combineSuite =
    C.suite "combine suite" []

listsSuite : C.Claim
listsSuite =
    C.suite "lists suite" []

transformSuite : C.Claim
transformSuite =
    C.suite "transform suite" []
