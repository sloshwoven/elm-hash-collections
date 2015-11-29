module TestUtil
    ( hashBool
    , altHashBool
    , unique
    , none
    , prependString
    , isEven
    , distinctPairInvestigator
    ) where

import Check.Investigator as I
import Hasher as H
import List as L
import Random as R
import Random.Extra as RE
import Shrink as S

hashBool : H.Hasher Bool comparable
hashBool b =
    if b then 1 else 0

altHashBool : H.Hasher Bool comparable
altHashBool b =
    if b then 5 else 7

unique : List a -> List a
unique list =
    let add x l =
        if L.any ((==) x) l
        then l
        else x :: l
    in L.foldr add [] list

none : (a -> Bool) -> List a -> Bool
none pred list =
    not <| L.any pred list

prependString : e -> List String -> List String
prependString e list =
    (toString e) :: list

isEven : number -> Bool
isEven n =
    n % 2 == 0

distinctPairInvestigator : I.Investigator a -> I.Investigator (a, a)
distinctPairInvestigator inv =
    let generator =
            distinctPairOf inv.generator
        shrinker =
            S.tuple (inv.shrinker, inv.shrinker)
    in I.investigator generator shrinker

distinctPairOf : R.Generator a -> R.Generator (a, a)
distinctPairOf gen =
    let isDifferent (x, y) = x /= y
    in RE.keepIf isDifferent <| R.pair gen gen
