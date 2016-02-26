module Util
    ( notF
    , prepend
    , memberOf
    , listToMaybe
    , maybeToBool
    , dictValMapSum
    , dictFilterMap
    , fstEq
    , mapPair
    , mapBothSnd
    ) where

import Dict as D
import List as L

notF : (a -> Bool) -> a -> Bool
notF f x =
    not <| f x

prepend : List a -> List a -> List a
prepend = flip L.append

memberOf : List e -> e -> Bool
memberOf = flip L.member

listToMaybe : List a -> Maybe (List a)
listToMaybe list =
    case list of
        [] -> Nothing
        _ -> Just list

maybeToBool : Maybe a -> Bool
maybeToBool m =
    case m of
        Nothing -> False
        Just _ -> True

dictValMapSum : (v -> number) -> D.Dict comparable v -> number
dictValMapSum f dict =
    let up k v sum =
        sum + f v
    in D.foldl up 0 dict

dictFilterMap : (comparable -> v1 -> Maybe v2) -> D.Dict comparable v1 -> D.Dict comparable v2
dictFilterMap f dict =
    let up k v acc =
        case f k v of
            Nothing -> acc
            Just res -> D.insert k res acc
    in D.foldl up D.empty dict

fstEq : a -> (a, b) -> Bool
fstEq x (l, _) =
    x == l

mapPair : (a -> b -> c) -> (a, b) -> c
mapPair f (l, r) =
    f l r

mapBothSnd : (a -> b -> c) -> (a, b) -> (a, c)
mapBothSnd f (l, r) =
    (l, f l r)
