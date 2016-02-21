module Util
    ( notF
    , listToMaybe
    , dictFilterMap
    ) where

import Dict as D

notF : (a -> Bool) -> a -> Bool
notF f x =
    not <| f x

listToMaybe : List a -> Maybe (List a)
listToMaybe list =
    case list of
        [] -> Nothing
        _ -> Just list

dictFilterMap : (comparable -> v1 -> Maybe v2) -> D.Dict comparable v1 -> D.Dict comparable v2
dictFilterMap f dict =
    let up k v acc =
        case f k v of
            Nothing -> acc
            Just res -> D.insert k res acc
    in D.foldl up D.empty dict
