module TestUtil
    ( hashBool
    , altHashBool
    , unique
    , none
    , prependString
    , isEven
    , distinctPairInvestigator
    , intSetInvestigator
    ) where

import Check.Investigator as I
import Hasher as H
import Lazy.List as LL
import List as L
import Random as R
import Random.Extra as RE
import Random.Int as RI
import Random.Set as RS
import Set
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

distinctPairOf : R.Generator a -> R.Generator (a, a)
distinctPairOf gen =
    let isDifferent (x, y) = x /= y
    in RE.keepIf isDifferent <| R.pair gen gen

intSetInvestigator : I.Investigator (Set.Set Int)
intSetInvestigator =
    I.investigator (RS.set 10 RI.anyInt) setShrinker

setShrinker : S.Shrinker (Set.Set comparable)
setShrinker s =
    let without e = Set.remove e s
    in Set.toList s |> L.map without |> LL.fromList
