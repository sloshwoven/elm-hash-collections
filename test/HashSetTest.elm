module HashSetTest (hashSetSuite) where

import Check as C

hashSetSuite : C.Claim
hashSetSuite =
    [ buildSuite
    , querySuite
    , combineSuite
    , listsSuite
    , transformSuite
    ]
    |> C.suite "HashSet suite"

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
