module HashSet
    ( HashSet
    , empty, singleton, insert, remove
    , member
    , foldl, foldr, map
    , filter, partition
    , union, intersect, diff
    , toList, fromList
    ) where

{-|
# Definition
@docs HashSet

# Build
@docs empty, singleton, insert, remove

# Query
@docs member

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition
-}

import Dict as D
import List as L

{-|-}
type HashSet a comparable = HashSet (a -> comparable) (D.Dict comparable a)

{-|-}
empty : (a -> comparable) -> HashSet a comparable
empty hasher = HashSet hasher <| D.empty

{-|-}
singleton : a -> (a -> comparable) -> HashSet a comparable
singleton elem hasher = HashSet hasher <| D.singleton (hasher elem) elem

{-|-}
insert : a -> HashSet a comparable -> HashSet a comparable
insert elem (HashSet hasher dict) = HashSet hasher <| D.insert (hasher elem) elem dict

{-|-}
remove : a -> HashSet a comparable -> HashSet a comparable
remove elem (HashSet hasher dict) = HashSet hasher <| D.remove (hasher elem) dict

{-|-}
member : a -> HashSet a comparable -> Bool
member elem (HashSet hasher dict) = D.member (hasher elem) dict

{-|-}
union : HashSet a comparable -> HashSet a comparable -> HashSet a comparable
union (HashSet hasher1 dict1) (HashSet hasher2 dict2) = HashSet hasher1 <| D.union dict1 dict2

{-|-}
intersect : HashSet a comparable -> HashSet a comparable -> HashSet a comparable
intersect (HashSet hasher1 dict1) (HashSet hasher2 dict2) = HashSet hasher1 <| D.intersect dict1 dict2

{-|-}
diff : HashSet a comparable -> HashSet a comparable -> HashSet a comparable
diff (HashSet hasher1 dict1) (HashSet hasher2 dict2) = HashSet hasher1 <| D.diff dict1 dict2

{-|-}
toList : HashSet a comparable -> List a
toList (HashSet hasher dict) = D.values dict

{-|-}
fromList : List a -> (a -> comparable) -> HashSet a comparable
fromList elems hasher =
    let toPair elem = (hasher elem, elem)
    in HashSet hasher <| D.fromList (L.map toPair elems)

{-|-}
map : (a -> b) -> (b -> comparable) -> HashSet a comparable -> HashSet b comparable
map f hasher (HashSet h dict) = fromList (L.map f <| D.values dict) hasher

{-|
uses hash order
-}
foldl : (a -> r -> r) -> r -> HashSet a comparable -> r
foldl update acc (HashSet hasher dict) = L.foldl update acc <| D.values dict

{-|
uses hash order
-}
foldr : (a -> r -> r) -> r -> HashSet a comparable -> r
foldr update acc (HashSet hasher dict) = L.foldr update acc <| D.values dict

{-|-}
filter : (a -> Bool) -> HashSet a comparable -> HashSet a comparable
filter pred (HashSet hasher dict) = fromList (L.filter pred <| D.values dict) hasher

{-|-}
partition : (a -> Bool) -> HashSet a comparable -> (HashSet a comparable, HashSet a comparable)
partition pred (HashSet hasher dict) =
    let pred' k v = pred v
        parts = D.partition pred' dict
    in (HashSet hasher (fst parts), HashSet hasher (snd parts))
