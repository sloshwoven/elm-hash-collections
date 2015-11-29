module TestUtil
    ( hashBool
    , altHashBool
    , distinctPairInvestigator
    ) where

import Check.Investigator as I
import Hasher as H
import Random as R
import Random.Extra as RE
import Shrink as S

hashBool : H.Hasher Bool comparable
hashBool b =
    if b then 1 else 0

altHashBool : H.Hasher Bool comparable
altHashBool b =
    if b then 5 else 7

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
