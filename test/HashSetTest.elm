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
    |> C.suite "HashSet"

buildSuite : C.Claim
buildSuite =
    C.suite "build" []

querySuite : C.Claim
querySuite =
    C.suite "query" []

combineSuite : C.Claim
combineSuite =
    C.suite "combine" []

listsSuite : C.Claim
listsSuite =
    C.suite "lists" []

transformSuite : C.Claim
transformSuite =
    C.suite "transform" []
