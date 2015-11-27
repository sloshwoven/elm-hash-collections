module HashDictTest (hashDictSuite) where

import Check as C
import Check.Investigator as I
import Hasher as H
import HashDict as HD
import List as L
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
        I.tuple (U.distinctPairInvestigator I.bool, I.int)

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

-- ==== combine ====

combineSuite : C.Claim
combineSuite =
    C.suite "combine"
        [ claimComponentKeysInUnion
        , claimUnionKeysInComponent
        , claimUnionValues
        , claimUnionEmptiness
        , claimUnionHasherFromFirst
        , claimComponentKeysInIntersect
        , claimIntersectKeysInBothComponents
        , claimIntersectValues
        , claimIntersectEmptiness
        , claimIntersectHasherFromFirst
        , claimComponentKeysInDiff
        , claimDiffKeysInFirst
        , claimDiffValues
        , claimDiffEmptiness
        , claimDiffHasherFromFirst
        ]

claimComponentKeysInUnion : C.Claim
claimComponentKeysInUnion =
    C.claim
        "all keys of the input HashDicts are in their union"
    `C.true`
        (\(hdict1, hdict2) ->
            let union = HD.union hdict1 hdict2
                inUnion k = HD.member k union
            in
                L.append (HD.keys hdict1) (HD.keys hdict2)
                |> L.all inUnion
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimUnionKeysInComponent : C.Claim
claimUnionKeysInComponent =
    C.claim
        "all keys of a union are in at least one of the HashDicts"
    `C.true`
        (\(hdict1, hdict2) ->
            let inComponent k = HD.member k hdict1 || HD.member k hdict2
            in HD.union hdict1 hdict2 |> HD.keys |> L.all inComponent
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimUnionValues : C.Claim
claimUnionValues =
    C.claim
        "union values come from the components (first if in both)"
    `C.true`
        (\(hdict1, hdict2) ->
            let union =
                    HD.union hdict1 hdict2
                correctValue k =
                    HD.get k union ==
                        if HD.member k hdict1
                        then HD.get k hdict1
                        else HD.get k hdict2
            in HD.keys union |> L.all correctValue
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimUnionEmptiness : C.Claim
claimUnionEmptiness =
    C.claim
        "unions are empty if and only if both components are empty"
    `C.that`
        (\(hdict1, hdict2) -> HD.union hdict1 hdict2 |> HD.isEmpty)
    `C.is`
        (\(hdict1, hdict2) -> HD.isEmpty hdict1 && HD.isEmpty hdict2)
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimUnionHasherFromFirst : C.Claim
claimUnionHasherFromFirst =
    claimHasherFromFirst "union" HD.union

claimComponentKeysInIntersect : C.Claim
claimComponentKeysInIntersect =
    C.claim
        "all keys that appear in both input HashDicts are in their intersect"
    `C.true`
        (\(hdict1, hdict2) ->
            let intersect = HD.intersect hdict1 hdict2
                inIntersect k = HD.member k intersect
                inHDict2 k = HD.member k hdict2
            in
                L.filter inHDict2 (HD.keys hdict1)
                |> L.all inIntersect
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimIntersectKeysInBothComponents : C.Claim
claimIntersectKeysInBothComponents =
    C.claim
        "all keys of an intersect are in both input HashDicts"
    `C.true`
        (\(hdict1, hdict2) ->
            let inBothComponents k = HD.member k hdict1 && HD.member k hdict2
            in HD.intersect hdict1 hdict2 |> HD.keys |> L.all inBothComponents
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimIntersectValues : C.Claim
claimIntersectValues =
    C.claim
        "intersect values come from the first component"
    `C.true`
        (\(hdict1, hdict2) ->
            let intersect =
                    HD.intersect hdict1 hdict2
                correctValue k =
                    HD.get k intersect == HD.get k hdict1
            in HD.keys intersect |> L.all correctValue
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimIntersectEmptiness : C.Claim
claimIntersectEmptiness =
    C.claim
        "intersections are empty if and only if the components have no keys in common"
    `C.that`
        (\(hdict1, hdict2) -> HD.intersect hdict1 hdict2 |> HD.isEmpty)
    `C.is`
        (\(hdict1, hdict2) ->
            let notInHDict2 k = not (HD.member k hdict2)
            in HD.keys hdict1 |> L.all notInHDict2
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimIntersectHasherFromFirst : C.Claim
claimIntersectHasherFromFirst =
    claimHasherFromFirst "intersection" HD.intersect

claimComponentKeysInDiff : C.Claim
claimComponentKeysInDiff =
    C.claim
        "all keys of the first HashDict that are not in the second are in the diff"
    `C.that`
        (\(hdict1, hdict2) -> HD.diff hdict1 hdict2 |> HD.keys)
    `C.is`
        (\(hdict1, hdict2) ->
            let notInHDict2 k = not (HD.member k hdict2)
            in hdict1 |> HD.keys |> L.filter notInHDict2
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimDiffKeysInFirst : C.Claim
claimDiffKeysInFirst =
    C.claim
        "all diff keys are in the first HashDict"
    `C.true`
        (\(hdict1, hdict2) ->
            let inHDict1 k = HD.member k hdict1
            in HD.diff hdict1 hdict2 |> HD.keys |> L.all inHDict1
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimDiffValues : C.Claim
claimDiffValues =
    C.claim
        "all diff values come from the first HashDict"
    `C.true`
        (\(hdict1, hdict2) ->
            let diff =
                    HD.diff hdict1 hdict2
                correctValue k =
                    HD.get k diff == HD.get k hdict1
            in HD.keys diff |> L.all correctValue
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimDiffEmptiness : C.Claim
claimDiffEmptiness =
    C.claim
        "diffs are empty if and only if all keys of the first are in the second"
    `C.that`
        (\(hdict1, hdict2) -> HD.diff hdict1 hdict2 |> HD.isEmpty)
    `C.is`
        (\(hdict1, hdict2) ->
            let inHDict2 k = HD.member k hdict2
            in HD.keys hdict1 |> L.all inHDict2
        )
    `C.for`
        I.tuple (testHashDictInvestigator, testHashDictInvestigator)

claimDiffHasherFromFirst : C.Claim
claimDiffHasherFromFirst =
    claimHasherFromFirst "diff" HD.diff

-- ==== lists ====

listsSuite : C.Claim
listsSuite =
    C.suite "lists"
        [ claimEmptyHasNoKeys
        , claimSingletonHasOneKey
        , claimKeyInKeyListAfterInsert
        , claimKeyNotInKeyListAfterUpdateToNothing
        , claimKeyInKeyListAfterUpdateToJust
        , claimEmptyHasNoValues
        , claimSingletonHasOneValue
        , claimValueInValueListAfterInsert
        , claimValueInValueListAfterUpdateToJust
        , claimToListIsKeysZippedWithValues
        , claimFromListToListIsSortedAssocList
        ]

claimEmptyHasNoKeys : C.Claim
claimEmptyHasNoKeys =
    C.claim
        "an empty HashDict has no keys"
    `C.true`
        (\() -> HD.empty hashBool |> HD.keys |> L.isEmpty)
    `C.for`
        I.void

claimSingletonHasOneKey : C.Claim
claimSingletonHasOneKey =
    C.claim
        "a singleton HashDict has the one key it was created with"
    `C.that`
        (\(k, v) -> HD.singleton hashBool k v |> HD.keys)
    `C.is`
        (\(k, v) -> [k])
    `C.for`
        I.tuple (I.bool, I.int)

claimKeyInKeyListAfterInsert : C.Claim
claimKeyInKeyListAfterInsert =
    C.claim
        "after inserting, the key is in the key list"
    `C.true`
        (\(hdict, k, v) -> HD.insert k v hdict |> HD.keys |> L.any ((==) k))
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimKeyNotInKeyListAfterUpdateToNothing : C.Claim
claimKeyNotInKeyListAfterUpdateToNothing =
    C.claim
        "after updating to Nothing, the key is no longer in the key list"
    `C.false`
        (\(hdict, k) -> HD.update k (always Nothing) hdict |> HD.keys |> L.any ((==) k))
    `C.for`
        I.tuple (testHashDictInvestigator, I.bool)

claimKeyInKeyListAfterUpdateToJust : C.Claim
claimKeyInKeyListAfterUpdateToJust =
    C.claim
        "after updating to a Just, the key is in the key list"
    `C.true`
        (\(hdict, k, v) -> HD.update k (always (Just v)) hdict |> HD.keys |> L.any ((==) k))
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimEmptyHasNoValues : C.Claim
claimEmptyHasNoValues =
    C.claim
        "an empty HashDict has no values"
    `C.true`
        (\() -> HD.empty hashBool |> HD.values |> L.isEmpty)
    `C.for`
        I.void

claimSingletonHasOneValue : C.Claim
claimSingletonHasOneValue =
    C.claim
        "a singleton HashDict has the one value it was created with"
    `C.that`
        (\(k, v) -> HD.singleton hashBool k v |> HD.values)
    `C.is`
        (\(k, v) -> [v])
    `C.for`
        I.tuple (I.bool, I.int)

claimValueInValueListAfterInsert : C.Claim
claimValueInValueListAfterInsert =
    C.claim
        "after inserting, the value is in the value list"
    `C.true`
        (\(hdict, k, v) -> HD.insert k v hdict |> HD.values |> L.any ((==) v))
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimValueInValueListAfterUpdateToJust : C.Claim
claimValueInValueListAfterUpdateToJust =
    C.claim
        "after updating to a Just, the value is in the value list"
    `C.true`
        (\(hdict, k, v) -> HD.update k (always (Just v)) hdict |> HD.values |> L.any ((==) v))
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimToListIsKeysZippedWithValues : C.Claim
claimToListIsKeysZippedWithValues =
    C.claim
        "toList is keys zipped with values"
    `C.that`
        (\(hdict) -> HD.toList hdict)
    `C.is`
        (\(hdict) -> L.map2 (,) (HD.keys hdict) (HD.values hdict))
    `C.for`
        testHashDictInvestigator

claimFromListToListIsSortedAssocList : C.Claim
claimFromListToListIsSortedAssocList =
    C.claim
        "fromList composed with toList produces a sorted association list"
    `C.that`
        (\(list) -> HD.fromList hashBool list |> HD.toList)
    `C.is`
        (\(list) -> L.foldr assocAppend [] list |> L.sortBy (hashBool << fst))
    `C.for`
        I.list (I.tuple (I.bool, I.int))

-- ==== transform ====

transformSuite : C.Claim
transformSuite =
    C.suite "transform"
        [ claimMapNegateNegatesValue
        , claimMapLeavesKeysUnchanged
        , claimFoldlToListMapReverse
        , claimFoldrToListMap
        ]

claimMapNegateNegatesValue : C.Claim
claimMapNegateNegatesValue =
    C.claim
        "mapping to negate negates values"
    `C.that`
        (\(hdict, k, v) -> HD.insert k v hdict |> HD.map negateValue |> HD.get k)
    `C.is`
        (\(hdict, k, v) -> Just (negate v))
    `C.for`
        I.tuple3 (testHashDictInvestigator, I.bool, I.int)

claimMapLeavesKeysUnchanged : C.Claim
claimMapLeavesKeysUnchanged =
    C.claim
        "mapping does not change the keys"
    `C.that`
        (\(hdict) -> HD.map negateValue hdict |> HD.keys)
    `C.is`
        (HD.keys)
    `C.for`
        testHashDictInvestigator

claimFoldlToListMapReverse : C.Claim
claimFoldlToListMapReverse =
    C.claim
        "building a list with foldl is toList mapped and reversed"
    `C.that`
        (\(hdict) -> HD.foldl prependKeyValueString [] hdict)
    `C.is`
        (\(hdict) -> HD.toList hdict |> L.map (uncurry appendStrings) |> L.reverse)
    `C.for`
        testHashDictInvestigator

claimFoldrToListMap : C.Claim
claimFoldrToListMap =
    C.claim
        "building a list with foldr is toList mapped"
    `C.that`
        (\(hdict) -> HD.foldr prependKeyValueString [] hdict)
    `C.is`
        (\(hdict) -> HD.toList hdict |> L.map (uncurry appendStrings))
    `C.for`
        testHashDictInvestigator

-- ==== helpers ====

hashBool : H.Hasher Bool comparable
hashBool b =
    if b then 1 else 0

altHashBool : H.Hasher Bool comparable
altHashBool b =
    if b then 5 else 7

assocAppend : (Bool, Int) -> List (Bool, Int) -> List (Bool, Int)
assocAppend (k, v) list =
    if assocMember k list
    then list
    else (k, v) :: list

assocMember : k -> List (k, v) -> Bool
assocMember k list =
    let keyMatch (k', v') =
        k' == k
    in L.any keyMatch list

negateValue : k -> number -> number
negateValue k v =
    negate v

prependKeyValueString : k -> v -> List String -> List String
prependKeyValueString k v list =
    appendStrings k v :: list

appendStrings : k -> v -> String
appendStrings k v =
    (toString k) ++ ":" ++ (toString v)

testHashDictInvestigator : I.Investigator (HD.HashDict Bool Int Int)
testHashDictInvestigator =
    makeTestHashDictInvestigator hashBool

altTestHashDictInvestigator : I.Investigator (HD.HashDict Bool Int Int)
altTestHashDictInvestigator =
    makeTestHashDictInvestigator altHashBool

makeTestHashDictInvestigator : H.Hasher Bool comparable -> I.Investigator (HD.HashDict Bool Int Int)
makeTestHashDictInvestigator hasher =
    let generator =
            hashDictGenerator hasher RB.bool RI.anyInt
        shrinker =
            hashDictShrinker S.bool S.int
    in I.investigator generator shrinker

hashDictGenerator : H.Hasher k comparable -> R.Generator k -> R.Generator v -> R.Generator (HD.HashDict k comparable v)
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

claimHasherFromFirst : String -> (HD.HashDict Bool comparable Int -> HD.HashDict Bool comparable Int -> HD.HashDict Bool comparable Int) -> C.Claim
claimHasherFromFirst name combiner =
    C.claim
        (name ++ " hasher comes from the first HashDict")
    `C.that`
        (\(hdict1, hdict2, k) -> combiner hdict1 hdict2 |> (\u -> u.hasher k))
    `C.is`
        (\(hdict1, hdict2, k) -> hdict1.hasher k)
    `C.for`
        I.tuple3 (testHashDictInvestigator, altTestHashDictInvestigator, I.bool)
