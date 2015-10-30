module TestUtil
    ( distinctPairInvestigator
    ) where

import Check.Investigator as I
import Random as R
import Random.Extra as RE
import Shrink as S

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
