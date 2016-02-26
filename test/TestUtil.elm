module TestUtil
    ( none
    , prependString
    , isEven
    , equalExcludingOrder
    , distinctPairInvestigator
    , intSetInvestigator
    , personInvestigator
    , personGenerator
    , personShrinker
    ) where

import Check.Investigator as I
import Hasher as H
import Lazy.List as LL
import List as L
import Person as P
import Random as R
import Random.Extra as RE
import Random.Int as RI
import Random.Set as RS
import Random.String as RSt
import Set
import Shrink as S
import Util as U

none : (a -> Bool) -> List a -> Bool
none pred list =
    not <| L.any pred list

prependString : e -> List String -> List String
prependString e list =
    (toString e) :: list

distinctPairInvestigator : I.Investigator a -> I.Investigator (a, a)
distinctPairInvestigator inv =
    let generator =
            distinctPairOf inv.generator
        shrinker =
            S.tuple (inv.shrinker, inv.shrinker)
    in I.investigator generator shrinker

isEven : Int -> Bool
isEven n =
    n % 2 == 0

equalExcludingOrder : List a -> List a -> Bool
equalExcludingOrder list1 list2 =
    (L.length list1 == L.length list2)
    &&
    L.all (U.memberOf list1) list2

distinctPairOf : R.Generator a -> R.Generator (a, a)
distinctPairOf gen =
    R.pair gen gen
    |> RE.keepIf isDistinctPair

isDistinctPair : (a, a) -> Bool
isDistinctPair =
    U.mapPair (/=)

intSetInvestigator : I.Investigator (Set.Set Int)
intSetInvestigator =
    I.investigator (RS.set 10 RI.anyInt) setShrinker

setShrinker : S.Shrinker (Set.Set comparable)
setShrinker s =
    Set.toList s
    |> L.map (removeFrom s)
    |> LL.fromList

removeFrom : Set.Set comparable -> comparable -> Set.Set comparable
removeFrom =
    flip Set.remove

personInvestigator : I.Investigator P.Person
personInvestigator =
    I.investigator personGenerator personShrinker

personGenerator : R.Generator P.Person
personGenerator =
    toPersonGenerator RI.anyInt RSt.anyEnglishWord

toPersonGenerator : R.Generator Int -> R.Generator String -> R.Generator P.Person
toPersonGenerator idGen nameGen =
    R.pair idGen nameGen
    |> R.map (U.mapPair P.Person)

personShrinker : S.Shrinker P.Person
personShrinker {id, name} =
    P.Person
        `S.map` S.int id
        `S.andMap` S.string name
