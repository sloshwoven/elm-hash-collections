module HashDict
    ( HashDict
    , empty, singleton, insert, update
    , get, remove, member
    , filter
    , partition
    , foldl, foldr, map
    , union, intersect, diff
    , keys, values
    , toList, fromList
    ) where

{-|
# Definition
@docs HashDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs member, get

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition
-}

import Dict as D
import List as L
import Maybe as M

{-|-}
type HashDict k comparable v = HashDict (k -> comparable) (D.Dict comparable (k, v))

{-|-}
empty : (k -> comparable) -> HashDict k comparable v
empty hasher = HashDict hasher <| D.empty

{-|-}
singleton : k -> v -> (k -> comparable) -> HashDict k comparable v
singleton k v hasher = HashDict hasher <| D.singleton (hasher k) (k, v)

{-|-}
insert : k -> v -> HashDict k comparable v -> HashDict k comparable v
insert k v (HashDict hasher h2kv) = HashDict hasher <| D.insert (hasher k) (k, v) h2kv

{-|-}
update : k -> (Maybe v -> Maybe v) -> HashDict k comparable v -> HashDict k comparable v
update k alter (HashDict hasher h2kv) =
    let alter' mkv = M.map snd mkv |> alter |> M.map (\v -> (k, v))
    in HashDict hasher <| D.update (hasher k) alter' h2kv

{-|-}
remove : k -> HashDict k comparable v -> HashDict k comparable v
remove k (HashDict hasher h2kv) = HashDict hasher <| D.remove (hasher k) h2kv

{-|-}
member : k -> HashDict k comparable v -> Bool
member k (HashDict hasher h2kv) = D.member (hasher k) h2kv

{-|-}
get : k -> HashDict k comparable v -> Maybe v
get k (HashDict hasher h2kv) = D.get (hasher k) h2kv |> M.map snd

{-|-}
union : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
union (HashDict hasher1 h2kv1) (HashDict hasher2 h2kv2) = HashDict hasher1 <| D.union h2kv1 h2kv2

{-|-}
intersect : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
intersect (HashDict hasher1 h2kv1) (HashDict hasher2 h2kv2) = HashDict hasher1 <| D.intersect h2kv1 h2kv2

{-|-}
diff : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
diff (HashDict hasher1 h2kv1) (HashDict hasher2 h2kv2) = HashDict hasher1 <| D.diff h2kv1 h2kv2

{-|-}
keys : HashDict k comparable v -> List k
keys = mapKVs fst

{-|-}
values : HashDict k comparable v -> List v
values = mapKVs snd

mapKVs : ((k, v) -> r) -> HashDict k comparable v -> List r
mapKVs f (HashDict hasher h2kv) = D.values h2kv |> L.map f

{-|-}
toList : HashDict k comparable v -> List (k, v)
toList (HashDict hasher h2kv) = D.values h2kv

{-|-}
fromList : List (k, v) -> (k -> comparable) -> HashDict k comparable v
fromList pairs hasher =
    let toHashPair kv = (hasher <| fst kv, kv)
    in HashDict hasher <| D.fromList <| L.map toHashPair pairs

{-|-}
map : (k -> v1 -> v2) -> HashDict k comparable v1 -> HashDict k comparable v2
map f (HashDict hasher h2kv) =
    let applyF hash kv =
        let k = fst kv
        in (k, f k (snd kv))
    in HashDict hasher <| D.map applyF h2kv

{-|
uses hash order
-}
foldl : (k -> v -> r -> r) -> r -> HashDict k comparable v -> r
foldl update acc (HashDict hasher h2kv) =
    let update' hash kv acc' = update (fst kv) (snd kv) acc'
    in D.foldl update' acc h2kv

{-|
uses hash order
-}
foldr : (k -> v -> r -> r) -> r -> HashDict k comparable v -> r
foldr update acc (HashDict hasher h2kv) =
    let update' hash kv acc' = update (fst kv) (snd kv) acc'
    in D.foldr update' acc h2kv

{-|-}
filter : (k -> v -> Bool) -> HashDict k comparable v -> HashDict k comparable v
filter pred (HashDict hasher h2kv) =
    let pred' hash kv = pred (fst kv) (snd kv)
    in HashDict hasher <| D.filter pred' h2kv

{-|-}
partition : (k -> v -> Bool) -> HashDict k comparable v -> (HashDict k comparable v, HashDict k comparable v)
partition pred (HashDict hasher h2kv) =
    let pred' hash kv = pred (fst kv) (snd kv)
        parts = D.partition pred' h2kv
    in (HashDict hasher (fst parts), HashDict hasher (snd parts))
