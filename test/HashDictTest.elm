module HashDictTest (hashDictSuite) where

import Check as C
import Check.Investigator as I
import HashDict as HD
import Random as R
import Random.Bool as RB
import Random.Extra as RE
import Random.Int as RI
import Random.List as RL
import Shrink as S
import TestUtil as U

hashDictSuite : C.Claim
hashDictSuite =
    [ buildSuite
    , querySuite
    , combineSuite
    , listsSuite
    , transformSuite
    ]
    |> C.suite "HashDict"

-- ==== build ====

buildSuite : C.Claim
buildSuite =
    C.suite "build"
    [ claimEmptyIsEmpty
    , claimMemberFromEmptyIsFalse
    , claimGetFromEmptyIsNothing
    , claimSingletonNotEmpty
    , claimSingletonContainsKey
    , claimSingletonContainsValue
    , claimSingletonDoesNotContainOther
    , claimInsertMakesNonEmpty
    , claimKeyPresentAfterInsert
    , claimInsertAddsValue
    , claimKeyNotPresentAfterUpdateToNothing
    , claimKeyPresentAfterUpdateToJust
    , claimValuePresentAfterUpdateToJust
    , claimKeyNotPresentAfterRemove
    , claimRemoveAllIsEmpty
    ]

claimEmptyIsEmpty : C.Claim
claimEmptyIsEmpty =
    C.claim
        "empty produces a result that is empty"
    `C.true`
        (\() -> HD.empty hashBool |> HD.isEmpty)
    `C.for`
        I.void

claimMemberFromEmptyIsFalse : C.Claim
claimMemberFromEmptyIsFalse =
    C.claim
        "calling member with an empty HashDict always returns False"
    `C.false`
        (\k -> HD.empty hashBool |> HD.member k)
    `C.for`
        I.bool

claimGetFromEmptyIsNothing : C.Claim
claimGetFromEmptyIsNothing =
    C.claim
        "getting from an empty HashDict always returns Nothing"
    `C.that`
        (\k -> HD.empty hashBool |> HD.get k)
    `C.is`
        always Nothing
    `C.for`
        I.bool

claimSingletonNotEmpty : C.Claim
claimSingletonNotEmpty =
    C.claim
        "singletons are not empty"
    `C.false`
        (\(k, v) -> HD.singleton hashBool k v |> HD.isEmpty)
    `C.for`
        I.tuple (I.bool, I.int)

claimSingletonContainsKey : C.Claim
claimSingletonContainsKey =
    C.claim
        "singletons contain the key they were created with"
    `C.true`
        (\(k, v) -> HD.singleton hashBool k v |> HD.member k)
    `C.for`
        I.tuple (I.bool, I.int)

claimSingletonContainsValue : C.Claim
claimSingletonContainsValue =
    C.claim
        "singletons contain the value they were created with"
    `C.that`
        (\(k, v) -> HD.singleton hashBool k v |> HD.get k)
    `C.is`
        (Just << snd)
    `C.for`
        I.tuple (I.bool, I.int)

claimSingletonDoesNotContainOther : C.Claim
claimSingletonDoesNotContainOther =
    C.claim
        "singletons do not contain a key other than the one they were created with"
    `C.false`
        (\((k1, k2), v) -> HD.singleton hashBool k1 v |> HD.member k2)
    `C.for`
        I.tuple ((U.distinctPairInvestigator I.bool), I.int)

claimInsertMakesNonEmpty : C.Claim
claimInsertMakesNonEmpty =
    C.claim
        "after inserting, a HashDict is never empty"
    `C.false`
        (\(hdict, k, v) -> hdict |> HD.insert k v |> HD.isEmpty)
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimKeyPresentAfterInsert : C.Claim
claimKeyPresentAfterInsert =
    C.claim
        "after inserting, the key is present"
    `C.true`
        (\(hdict, k, v) -> HD.insert k v hdict |> HD.member k)
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimInsertAddsValue : C.Claim
claimInsertAddsValue =
    C.claim
        "after inserting, the value is present"
    `C.that`
        (\(hdict, k, v) -> HD.insert k v hdict |> HD.get k)
    `C.is`
        (\(hdict, k, v) -> Just v)
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimKeyNotPresentAfterUpdateToNothing : C.Claim
claimKeyNotPresentAfterUpdateToNothing =
    C.claim
        "after updating to Nothing, the key is no longer present"
    `C.false`
        (\(hdict, k) -> HD.update k (always Nothing) hdict |> HD.member k)
    `C.for`
        I.tuple (testHashDictInvestigator, I.bool)

claimKeyPresentAfterUpdateToJust : C.Claim
claimKeyPresentAfterUpdateToJust =
    C.claim
        "after updating to a Just, the key is present"
    `C.true`
        (\(hdict, k, v) -> HD.update k (always (Just v)) hdict |> HD.member k)
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimValuePresentAfterUpdateToJust : C.Claim
claimValuePresentAfterUpdateToJust =
    C.claim
        "after updating to a Just, the value is present"
    `C.that`
        (\(hdict, k, v) -> HD.update k (always (Just v)) hdict |> HD.get k)
    `C.is`
        (\(hdict, k, v) -> Just v)
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimKeyNotPresentAfterRemove : C.Claim
claimKeyNotPresentAfterRemove =
    C.claim
        "after removal, the key is no longer present"
    `C.false`
        (\(hdict, k, v) -> HD.insert k v hdict |> HD.remove k |> HD.member k)
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimRemoveAllIsEmpty : C.Claim
claimRemoveAllIsEmpty =
    C.claim
        "after all keys have been removed, the result is empty"
    `C.true`
        (\hdict -> HD.foldl (\k v r -> HD.remove k r) hdict hdict |> HD.isEmpty)
    `C.for`
        testHashDictInvestigator

-- ==== query ====

querySuite : C.Claim
querySuite =
    C.suite "query" []

-- ==== combine ====

combineSuite : C.Claim
combineSuite =
    C.suite "combine" []

-- ==== lists ====

listsSuite : C.Claim
listsSuite =
    C.suite "lists" []

-- ==== transform ====

transformSuite : C.Claim
transformSuite =
    C.suite "transform" []

-- ==== helpers ====

hashBool : HD.Hasher Bool comparable
hashBool b =
    if b then 1 else 0

testHashDictInvestigator : I.Investigator (HD.HashDict Bool Int Int)
testHashDictInvestigator =
    let generator =
            hashDictGenerator hashBool RB.bool RI.anyInt
        shrinker =
            hashDictShrinker S.bool S.int
    in I.investigator generator shrinker

hashDictGenerator : HD.Hasher k comparable -> R.Generator k -> R.Generator v -> R.Generator (HD.HashDict k comparable v)
hashDictGenerator hasher keyGenerator valueGenerator =
    R.pair keyGenerator valueGenerator
    |> RL.rangeLengthList 0 10
    |> RE.map (HD.fromList hasher)

-- the arguments to this function don't obviously look like they match the signature,
-- but keep in mind that Shrinker is a type alias for a -> List a
hashDictShrinker : S.Shrinker k -> S.Shrinker v -> S.Shrinker (HD.HashDict k comparable v)
hashDictShrinker keyShrinker valueShrinker hdict =
    let removeKey k v list =
            (HD.remove k hdict) :: list
        shrinkKey k v list =
            let smallerKeys = keyShrinker k
            in case smallerKeys of
                [] -> list
                smallerKey :: rest -> (HD.remove k hdict |> HD.insert smallerKey v) :: list
        shrinkValue k v list =
            let smallerValues = valueShrinker v
            in case smallerValues of
                [] -> list
                smallerValue :: rest -> (HD.insert k smallerValue hdict) :: list
    in
        (HD.foldl removeKey [] hdict)
        ++
        (HD.foldl shrinkKey [] hdict)
        ++
        (HD.foldl shrinkValue [] hdict)
