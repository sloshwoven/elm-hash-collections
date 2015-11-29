module HashSetTest (hashSetSuite) where

import Check as C
import Check.Investigator as I
import Hasher as H
import HashSet as HS
import Random as R
import Random.Bool as RB
import Random.Extra as RE
import Random.List as RL
import Shrink as S
import TestUtil as U

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
    C.suite "build"
    [ claimEmptyIsEmpty
    , claimMemberFromEmptyIsFalse
    , claimSingletonNotEmpty
    , claimSingletonContainsElement
    , claimSingletonDoesNotContainOther
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

-- ==== helpers ====

testHashSetInvestigator : I.Investigator (HS.HashSet Bool Int)
testHashSetInvestigator =
    makeTestHashSetInvestigator U.hashBool

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
-- but keep in mind that Shrinker is a type alias for a -> List a
hashSetShrinker : S.Shrinker e -> S.Shrinker (HS.HashSet e comparable)
hashSetShrinker elemShrinker hset =
    let removeElem e list =
            (HS.remove e hset) :: list
        shrinkElem e list =
            let smallerElems = elemShrinker e
            in case smallerElems of
                [] -> list
                smallerElem :: rest -> (HS.remove e hset |> HS.insert smallerElem) :: list
    in
        (HS.foldl removeElem [] hset)
        ++
        (HS.foldl shrinkElem [] hset)
