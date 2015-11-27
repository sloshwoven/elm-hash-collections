module HashDict
    ( HashDict
    , Hasher
    , empty, singleton, insert, update
    , isEmpty, get, remove, member
    , filter
    , partition
    , foldl, foldr, map
    , union, intersect, diff
    , keys, values
    , toList, fromList
    ) where

{-|
# Definition
@docs HashDict, Hasher

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get

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
type alias HashDict k comparable v =
    { hasher   : Hasher k comparable
    , hashToKV : D.Dict comparable (k, v)
    }

{-|-}
type alias Hasher k comparable = k -> comparable

-- build

{-|-}
empty : Hasher k comparable -> HashDict k comparable v
empty hasher =
    { hasher   = hasher
    , hashToKV = D.empty
    }

{-|-}
singleton : Hasher k comparable -> k -> v -> HashDict k comparable v
singleton hasher k v =
    { hasher   = hasher 
    , hashToKV = D.singleton (hasher k) (k, v)
    }

{-|-}
insert : k -> v -> HashDict k comparable v -> HashDict k comparable v
insert k v hdict =
    { hdict |
        hashToKV <- D.insert (hdict.hasher k) (k, v) hdict.hashToKV
    }

{-|-}
update : k -> (Maybe v -> Maybe v) -> HashDict k comparable v -> HashDict k comparable v
update k alter hdict =
    let alter' mkv =
        M.map snd mkv |> alter |> M.map (\v -> (k, v))
    in
        { hdict |
            hashToKV <- D.update (hdict.hasher k) alter' hdict.hashToKV
        }

{-|-}
remove : k -> HashDict k comparable v -> HashDict k comparable v
remove k hdict =
    { hdict |
        hashToKV <- D.remove (hdict.hasher k) hdict.hashToKV
    }

-- query

{-|-}
isEmpty : HashDict k comparable v -> Bool
isEmpty hdict =
    D.isEmpty hdict.hashToKV

{-|-}
member : k -> HashDict k comparable v -> Bool
member k hdict =
    D.member (hdict.hasher k) hdict.hashToKV

{-|-}
get : k -> HashDict k comparable v -> Maybe v
get k hdict =
    D.get (hdict.hasher k) hdict.hashToKV |> M.map snd

-- combine

{-|-}
union : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
union hdict1 hdict2 =
    { hdict1 |
        hashToKV <- D.union hdict1.hashToKV hdict2.hashToKV
    }

{-|-}
intersect : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
intersect hdict1 hdict2 =
    { hdict1 |
        hashToKV <- D.intersect hdict1.hashToKV hdict2.hashToKV
    }

{-|-}
diff : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
diff hdict1 hdict2 =
    { hdict1 |
        hashToKV <- D.diff hdict1.hashToKV hdict2.hashToKV
    }

-- lists

{-|-}
keys : HashDict k comparable v -> List k
keys = mapKVs fst

{-|-}
values : HashDict k comparable v -> List v
values = mapKVs snd

mapKVs : ((k, v) -> r) -> HashDict k comparable v -> List r
mapKVs f hdict =
    D.values hdict.hashToKV |> L.map f

{-|-}
toList : HashDict k comparable v -> List (k, v)
toList hdict =
    D.values hdict.hashToKV

{-|-}
fromList : Hasher k comparable -> List (k, v) -> HashDict k comparable v
fromList hasher pairs =
    let toHashPair kv =
        (hasher <| fst kv, kv)
    in
        { hasher   = hasher
        , hashToKV = D.fromList <| L.map toHashPair pairs
        }

-- transform

{-|-}
map : (k -> v -> v') -> HashDict k comparable v -> HashDict k comparable v'
map f hdict =
    let applyF hash kv =
        let k = fst kv
        in (k, f k (snd kv))
    in
        { hdict |
            hashToKV <- D.map applyF hdict.hashToKV
        }

{-|
uses hash order
-}
foldl : (k -> v -> r -> r) -> r -> HashDict k comparable v -> r
foldl update acc hdict =
    let update' hash kv acc' =
        update (fst kv) (snd kv) acc'
    in D.foldl update' acc hdict.hashToKV

{-|
uses hash order
-}
foldr : (k -> v -> r -> r) -> r -> HashDict k comparable v -> r
foldr update acc hdict =
    let update' hash kv acc' =
        update (fst kv) (snd kv) acc'
    in D.foldr update' acc hdict.hashToKV

{-|-}
filter : (k -> v -> Bool) -> HashDict k comparable v -> HashDict k comparable v
filter pred hdict =
    let pred' hash kv =
        pred (fst kv) (snd kv)
    in
        { hdict |
            hashToKV <- D.filter pred' hdict.hashToKV
        }

{-|-}
partition : (k -> v -> Bool) -> HashDict k comparable v -> (HashDict k comparable v, HashDict k comparable v)
partition pred hdict =
    let pred' hash kv = pred (fst kv) (snd kv)
        parts = D.partition pred' hdict.hashToKV
    in
        ( { hdict | hashToKV <- fst parts }
        , { hdict | hashToKV <- snd parts }
        )
