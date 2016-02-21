module HashSetTest (hashSetSuite) where

import Check as C
import Check.Investigator as I
import Hasher as H
import HashSet as HS
import Lazy.List as LL
import List as L
import Random as R
import Random.Bool as RB
import Random.Extra as RE
import Random.List as RL
import Set
import Shrink as S
import TestUtil as U

hashSetSuite : C.Claim
hashSetSuite =
    [ buildSuite
    , combineSuite
    , listsSuite
    , transformSuite
    ]
    |> C.suite "HashSet"

-- ==== build ====

buildSuite : C.Claim
buildSuite =
    C.suite "build"
    [ claimEmptyIsEmpty
    , claimMemberFromEmptyIsFalse
    , claimSingletonNotEmpty
    , claimSingletonContainsElement
    , claimSingletonDoesNotContainOther
    , claimFromSetIsToListFromList
    , claimInsertMakesNonEmpty
    , claimElementPresentAfterInsert
    , claimElementNotPresentAfterRemove
    , claimRemoveAllIsEmpty
    ]

claimEmptyIsEmpty : C.Claim
claimEmptyIsEmpty =
    C.claim
        "empty produces a result that is empty"
    `C.true`
        (\() -> HS.empty U.hashBool |> HS.isEmpty)
    `C.for`
        I.void

claimMemberFromEmptyIsFalse : C.Claim
claimMemberFromEmptyIsFalse =
    C.claim
        "calling member with an empty HashSet always returns False"
    `C.false`
        (\e -> HS.empty U.hashBool |> HS.member e)
    `C.for`
        I.bool

claimSingletonNotEmpty : C.Claim
claimSingletonNotEmpty =
    C.claim
        "singletons are not empty"
    `C.false`
        (\e -> HS.singleton U.hashBool e |> HS.isEmpty)
    `C.for`
        I.bool

claimSingletonContainsElement : C.Claim
claimSingletonContainsElement =
    C.claim
        "singletons contain the element they were created with"
    `C.true`
        (\e -> HS.singleton U.hashBool e |> HS.member e)
    `C.for`
        I.bool

claimSingletonDoesNotContainOther : C.Claim
claimSingletonDoesNotContainOther =
    C.claim
        "singletons do not contain an element other than the one they were created with"
    `C.false`
        (\(e1, e2) -> HS.singleton U.hashBool e1 |> HS.member e2)
    `C.for`
        U.distinctPairInvestigator I.bool

claimFromSetIsToListFromList : C.Claim
claimFromSetIsToListFromList =
    C.claim
        "fromSet is Set.toList >> HashSet.fromList identity"
    `C.that`
        HS.fromSet
    `C.is`
        (Set.toList >> HS.fromList identity)
    `C.for`
        U.intSetInvestigator

claimInsertMakesNonEmpty : C.Claim
claimInsertMakesNonEmpty =
    C.claim
        "after inserting, a HashSet is never empty"
    `C.false`
        (\(hset, e) -> HS.insert e hset |> HS.isEmpty)
    `C.for`
        I.tuple (testHashSetInvestigator, I.bool)

claimElementPresentAfterInsert : C.Claim
claimElementPresentAfterInsert =
    C.claim
        "after inserting, the element is present"
    `C.true`
        (\(hset, e) -> HS.insert e hset |> HS.member e)
    `C.for`
        I.tuple (testHashSetInvestigator, I.bool)

claimElementNotPresentAfterRemove : C.Claim
claimElementNotPresentAfterRemove =
    C.claim
        "after removal, the element is no longer present"
    `C.false`
        (\(hset, e) -> HS.insert e hset |> HS.remove e |> HS.member e)
    `C.for`
        I.tuple (testHashSetInvestigator, I.bool)

claimRemoveAllIsEmpty : C.Claim
claimRemoveAllIsEmpty =
    C.claim
        "after all elements have been removed, the result is empty"
    `C.true`
        (\hset -> HS.foldl (\e r -> HS.remove e r) hset hset |> HS.isEmpty)
    `C.for`
        testHashSetInvestigator

-- ==== combine ====

combineSuite : C.Claim
combineSuite =
    C.suite "combine"
        [ claimComponentElementsInUnion
        , claimUnionElementsInComponent
        , claimUnionEmptiness
        , claimUnionHasherFromFirst
        , claimComponentElementsInIntersect
        , claimIntersectElementsInBothComponents
        , claimIntersectEmptiness
        , claimIntersectHasherFromFirst
        , claimComponentElementsInDiff
        , claimDiffElementsInFirst
        , claimDiffEmptiness
        , claimDiffHasherFromFirst
        ]

claimComponentElementsInUnion : C.Claim
claimComponentElementsInUnion =
    C.claim
        "all elements of the input HashSets are in their union"
    `C.true`
        (\(hset1, hset2) ->
            let union = HS.union hset1 hset2
                inUnion e = HS.member e union
            in
                L.append (HS.toList hset1) (HS.toList hset2)
                |> L.all inUnion
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimUnionElementsInComponent : C.Claim
claimUnionElementsInComponent =
    C.claim
        "all elements of a union are in at least one of the HashSets"
    `C.true`
        (\(hset1, hset2) ->
            let inComponent e = HS.member e hset1 || HS.member e hset2
            in HS.union hset1 hset2 |> HS.toList |> L.all inComponent
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimUnionEmptiness : C.Claim
claimUnionEmptiness =
    C.claim
        "unions are empty if and only if both components are empty"
    `C.that`
        (\(hset1, hset2) -> HS.union hset1 hset2 |> HS.isEmpty)
    `C.is`
        (\(hset1, hset2) -> HS.isEmpty hset1 && HS.isEmpty hset2)
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimUnionHasherFromFirst : C.Claim
claimUnionHasherFromFirst =
    claimHasherFromFirst "union" HS.union

claimComponentElementsInIntersect : C.Claim
claimComponentElementsInIntersect =
    C.claim
        "all elements that appear in both input HashSets are in their intersect"
    `C.true`
        (\(hset1, hset2) ->
            let intersect = HS.intersect hset1 hset2
                inIntersect e = HS.member e intersect
                inHSet2 e = HS.member e hset2
            in
                L.filter inHSet2 (HS.toList hset1)
                |> L.all inIntersect
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimIntersectElementsInBothComponents : C.Claim
claimIntersectElementsInBothComponents =
    C.claim
        "all elements of an intersect are in both input HashSets"
    `C.true`
        (\(hset1, hset2) ->
            let inBothComponents e = HS.member e hset1 && HS.member e hset2
            in HS.intersect hset1 hset2 |> HS.toList |> L.all inBothComponents
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimIntersectEmptiness : C.Claim
claimIntersectEmptiness =
    C.claim
        "intersections are empty if and only if the components have no keys in common"
    `C.that`
        (\(hset1, hset2) -> HS.intersect hset1 hset2 |> HS.isEmpty)
    `C.is`
        (\(hset1, hset2) ->
            let notInHSet2 e = not (HS.member e hset2)
            in HS.toList hset1 |> L.all notInHSet2
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimIntersectHasherFromFirst : C.Claim
claimIntersectHasherFromFirst =
    claimHasherFromFirst "intersection" HS.intersect

claimComponentElementsInDiff : C.Claim
claimComponentElementsInDiff =
    C.claim
        "all elements of the first HashSet that are not in the second are in the diff"
    `C.that`
        (\(hset1, hset2) -> HS.diff hset1 hset2 |> HS.toList)
    `C.is`
        (\(hset1, hset2) ->
            let notInHSet2 e = not (HS.member e hset2)
            in hset1 |> HS.toList |> L.filter notInHSet2
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimDiffElementsInFirst : C.Claim
claimDiffElementsInFirst =
    C.claim
        "all diff elements are in the first HashSet"
    `C.true`
        (\(hset1, hset2) ->
            let inHSet1 e = HS.member e hset1
            in HS.diff hset1 hset2 |> HS.toList |> L.all inHSet1
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimDiffEmptiness : C.Claim
claimDiffEmptiness =
    C.claim
        "diffs are empty if and only if all elements of the first are in the second"
    `C.that`
        (\(hset1, hset2) -> HS.diff hset1 hset2 |> HS.isEmpty)
    `C.is`
        (\(hset1, hset2) ->
            let inHSet2 e = HS.member e hset2
            in HS.toList hset1 |> L.all inHSet2
        )
    `C.for`
        I.tuple (testHashSetInvestigator, testHashSetInvestigator)

claimDiffHasherFromFirst : C.Claim
claimDiffHasherFromFirst =
    claimHasherFromFirst "diff" HS.diff

-- ==== lists ====

listsSuite : C.Claim
listsSuite =
    C.suite "lists"
        [ claimEmptyHasNoElements
        , claimSingletonHasOneElement
        , claimElementInListAfterInsert
        , claimElementNotInListAfterRemove
        , claimFromListToListIsSortedList
        , claimToListLengthIsSize
        ]

claimEmptyHasNoElements : C.Claim
claimEmptyHasNoElements =
    C.claim
        "an empty HashSet converted to a list has no elements"
    `C.true`
        (\() -> HS.empty U.hashBool |> HS.toList |> L.isEmpty)
    `C.for`
        I.void

claimSingletonHasOneElement : C.Claim
claimSingletonHasOneElement =
    C.claim
        "a singleton HashSet converted to a list has only the one element it was created with"
    `C.that`
        (\e -> HS.singleton U.hashBool e |> HS.toList)
    `C.is`
        (\e -> [e])
    `C.for`
        I.bool

claimElementInListAfterInsert : C.Claim
claimElementInListAfterInsert =
    C.claim
        "after inserting, the element is in the list"
    `C.true`
        (\(hset, e) -> HS.insert e hset |> HS.toList |> L.any ((==) e))
    `C.for`
        I.tuple (testHashSetInvestigator, I.bool)

claimElementNotInListAfterRemove : C.Claim
claimElementNotInListAfterRemove =
    C.claim
        "after removing an element, it is no longer in the list"
    `C.false`
        (\(hset, e) -> HS.insert e hset |> HS.remove e |> HS.toList |> L.any ((==) e))
    `C.for`
        I.tuple (testHashSetInvestigator, I.bool)

claimFromListToListIsSortedList : C.Claim
claimFromListToListIsSortedList =
    C.claim
        "fromList composed with toList produces a sorted unique list"
    `C.that`
        (\list -> HS.fromList U.hashBool list |> HS.toList)
    `C.is`
        (\list -> U.unique list |> L.sortBy U.hashBool)
    `C.for`
        I.list I.bool

claimToListLengthIsSize : C.Claim
claimToListLengthIsSize =
    C.claim
        "toList produces a List whose length is the HashSet's size"
    `C.that`
        (\hset -> HS.toList hset |> L.length)
    `C.is`
        (\hset -> HS.size hset)
     `C.for`
        testHashSetInvestigator

-- ==== transform ====

transformSuite : C.Claim
transformSuite =
    C.suite "transform"
        [ claimMapToListIsToListMap
        , claimMapPrimeToListIsToListMap
        , claimMapPrimeHasher
        , claimFoldlIsToListMapReverse
        , claimFoldrIsToListMap
        , claimFilterFalseIsEmpty
        , claimFilterTrueLeavesUnchanged
        , claimFilterToListIsToListFilter
        , claimPartitionUnionLeavesUnchanged
        , claimPartitionIntersectionIsEmpty
        , claimPartitionTrueLeftFalseRight
        ]

claimMapToListIsToListMap : C.Claim
claimMapToListIsToListMap =
    C.claim
        "map toList is toList map sorted"
    `C.that`
        (\hset -> HS.map not hset.hasher hset |> HS.toList)
    `C.is`
        (\hset -> HS.toList hset |> L.map not |> L.sortBy U.hashBool)
    `C.for`
        testHashSetInvestigator

claimMapPrimeToListIsToListMap : C.Claim
claimMapPrimeToListIsToListMap =
    C.claim
        "map' toList is toList map sorted"
    `C.that`
        (\hset -> HS.map' not hset |> HS.toList)
    `C.is`
        (\hset -> HS.toList hset |> L.map not |> L.sortBy U.hashBool)
    `C.for`
        testHashSetInvestigator

claimMapPrimeHasher : C.Claim
claimMapPrimeHasher =
    C.claim
        ("map' hasher comes from the HashSet")
    `C.that`
        (\(hset, e) -> HS.map' not hset |> (\hset2 -> hset2.hasher e))
    `C.is`
        (\(hset, e) -> hset.hasher e)
    `C.for`
        I.tuple (testHashSetInvestigator, I.bool)

claimFoldlIsToListMapReverse : C.Claim
claimFoldlIsToListMapReverse =
    C.claim
        "building a list with foldl is toList mapped and reversed"
    `C.that`
        (\hset -> HS.foldl U.prependString [] hset)
    `C.is`
        (\hset -> HS.toList hset |> L.map toString |> L.reverse)
    `C.for`
        testHashSetInvestigator

claimFoldrIsToListMap : C.Claim
claimFoldrIsToListMap =
    C.claim
        "building a list with foldr is toList mapped"
    `C.that`
        (\hset -> HS.foldr U.prependString [] hset)
    `C.is`
        (\hset -> HS.toList hset |> L.map toString)
    `C.for`
        testHashSetInvestigator

claimFilterFalseIsEmpty : C.Claim
claimFilterFalseIsEmpty =
    C.claim
        "filtering with false produces an empty HashSet"
    `C.true`
        (\hset -> HS.filter (always False) hset |> HS.isEmpty)
    `C.for`
        testHashSetInvestigator

claimFilterTrueLeavesUnchanged : C.Claim
claimFilterTrueLeavesUnchanged =
    C.claim
        "filtering with true produces an identical HashSet"
    `C.that`
        (\hset -> HS.filter (always True) hset |> HS.toList)
    `C.is`
        (HS.toList)
    `C.for`
        testHashSetInvestigator

claimFilterToListIsToListFilter : C.Claim
claimFilterToListIsToListFilter =
    C.claim
        "filter the toList is the same as toList then filter"
    `C.that`
        (\hset -> HS.filter identity hset |> HS.toList)
    `C.is`
        (\hset -> HS.toList hset |> L.filter identity)
    `C.for`
        testHashSetInvestigator

claimPartitionUnionLeavesUnchanged : C.Claim
claimPartitionUnionLeavesUnchanged =
    C.claim
        "partition then union leaves the HashSet unchanged"
    `C.that`
        (\hset ->
            let part = HS.partition identity hset
            in HS.union (fst part) (snd part) |> HS.toList
        )
    `C.is`
        (HS.toList)
    `C.for`
        testHashSetInvestigator

claimPartitionIntersectionIsEmpty : C.Claim
claimPartitionIntersectionIsEmpty =
    C.claim
        "partition then intersection is empty"
    `C.true`
        (\hset ->
            let part = HS.partition identity hset
            in HS.intersect (fst part) (snd part) |> HS.isEmpty
        )
    `C.for`
        testHashSetInvestigator

claimPartitionTrueLeftFalseRight : C.Claim
claimPartitionTrueLeftFalseRight =
    C.claim
        "partition condition is true for all left, false for all right"
    `C.true`
        (\hset ->
            let part = HS.partition identity hset
            in
                ((fst part) |> HS.toList |> L.all identity)
                &&
                ((snd part) |> HS.toList |> L.all not)
        )
    `C.for`
        testHashSetInvestigator

-- ==== helpers ====

testHashSetInvestigator : I.Investigator (HS.HashSet Bool Int)
testHashSetInvestigator =
    makeTestHashSetInvestigator U.hashBool

altTestHashSetInvestigator : I.Investigator (HS.HashSet Bool Int)
altTestHashSetInvestigator =
    makeTestHashSetInvestigator U.altHashBool

makeTestHashSetInvestigator : H.Hasher Bool comparable -> I.Investigator (HS.HashSet Bool comparable)
makeTestHashSetInvestigator hasher =
    let generator =
            hashSetGenerator hasher RB.bool
        shrinker =
            hashSetShrinker S.bool
    in I.investigator generator shrinker

hashSetGenerator : H.Hasher e comparable -> R.Generator e -> R.Generator (HS.HashSet e comparable)
hashSetGenerator hasher elemGenerator =
    RL.rangeLengthList 0 10 elemGenerator
    |> RE.map (HS.fromList hasher)

-- the arguments to this function don't obviously look like they match the signature,
-- but keep in mind that Shrinker is a type alias for a -> LazyList a
hashSetShrinker : S.Shrinker e -> S.Shrinker (HS.HashSet e comparable)
hashSetShrinker elemShrinker hset =
    let removeElem e list =
            (HS.remove e hset) :: list
        shrinkElem e list =
            let smallerElems = elemShrinker e |> LL.toList
            in case smallerElems of
                [] -> list
                smallerElem :: rest -> (HS.remove e hset |> HS.insert smallerElem) :: list
    in
        (HS.foldl removeElem [] hset)
        ++
        (HS.foldl shrinkElem [] hset)
        |> LL.fromList

claimHasherFromFirst : String -> (HS.HashSet Bool Int -> HS.HashSet Bool Int -> HS.HashSet Bool Int) -> C.Claim
claimHasherFromFirst name combiner =
    C.claim
        (name ++ " hasher comes from the first HashSet")
    `C.that`
        (\(hset1, hset2, e) -> combiner hset1 hset2 |> (\u -> u.hasher e))
    `C.is`
        (\(hset1, hset2, e) -> hset1.hasher e)
    `C.for`
        I.tuple3 (testHashSetInvestigator, altTestHashSetInvestigator, I.bool)
