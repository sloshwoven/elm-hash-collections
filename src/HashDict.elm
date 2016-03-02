module HashDict
    ( HashDict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , union, intersect, diff
    , keys, values , toList, fromList
    , map, foldl, foldr, filter, partition
    ) where

{-|
# Definition
@docs HashDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition
-}

import Dict as D
import Hasher as H
import List as L
import ListSet as LS
import Maybe as M
import Util as U

{-| A dictionary mapping keys to values.

- `k`: key type
- `comparable`: key hash type
- `v`: value type

- `hasher`: a function that turns keys into `comparable` hashes
- `hashToKV`: a `Dict` of hashes to values
-}
type alias HashDict k comparable v =
    { hasher   : H.Hasher k comparable
    , hashToKV : D.Dict comparable (List (k, v))
    }

-- build

{-| Create an empty `HashDict`.

Usage: `empty hasher`

- `hasher`: a function that turns keys into `comparable` hashes
-}
empty : H.Hasher k comparable -> HashDict k comparable v
empty hasher =
    { hasher   = hasher
    , hashToKV = D.empty
    }

{-| Create a `HashDict` containing a single key/value pair.

Usage: `singleton hasher k v`

- `hasher`: a function that turns keys into `comparable` hashes
- `k`: key
- `v`: value
-}
singleton : H.Hasher k comparable -> k -> v -> HashDict k comparable v
singleton hasher k v =
    { hasher   = hasher
    , hashToKV = D.singleton (hasher k) [(k, v)]
    }

{-| Create a `HashDict` by adding a key/value pair to another `HashDict`.

Usage: `insert k v hdict`

- `k`: key
- `v`: value
- `hdict`: starting `HashDict`
-}
insert : k -> v -> HashDict k comparable v -> HashDict k comparable v
insert k v hdict =
    update k (always <| Just v) hdict

{-| Create a `HashDict` by updating a key/value pair of another `HashDict`.

Usage: `update k up hdict`

- `k`: key
- `up`: updating function
- `hdict`: starting `HashDict`

Example:

    alice = { id = 1, name = "Alice" }
    bob = { id = 2, name = "Bob" }
    eve = { id = 3, name = "Eve" }
    scores = HD.fromList .id [(alice, 5), (bob, 3)]

    -- removes Alice
    HD.update alice (\maybeScore -> Nothing) scores

    -- sets Alice's score to 9
    HD.update alice (\maybeScore -> Just 9) scores

    -- adds Eve with a score of 6
    HD.update eve (\maybeScore -> Just 6) scores
-}
update : k -> (Maybe v -> Maybe v) -> HashDict k comparable v -> HashDict k comparable v
update k up hdict =
    let
        sing v =
            [(k, v)]
        up' mkvs =
            case mkvs of
                Nothing ->
                    up Nothing
                    |> M.map sing
                Just kvs ->
                    let up'' kvs' =
                        case kvs' of
                            [] ->
                                up Nothing
                                |> M.map sing
                                |> M.withDefault []
                            (k', v') :: rest ->
                                if (k' == k)
                                then
                                    up (Just v')
                                    |> M.map sing
                                    |> M.withDefault []
                                    |> U.prepend rest
                                else
                                    (k', v') :: up'' rest
                    in
                        up'' kvs
                        |> U.listToMaybe
    in
        { hdict |
            hashToKV = D.update (hdict.hasher k) up' hdict.hashToKV
        }

{-| Create a `HashDict` by removing a key/value pair from another `HashDict`.

Usage: `remove k hdict`

- `k`: key
- `hdict`: starting `HashDict`
-}
remove : k -> HashDict k comparable v -> HashDict k comparable v
remove k hdict =
    update k (always Nothing) hdict

-- query

{-| Determine if a `HashDict` is empty.

Usage: `isEmpty hdict`

- `hdict`: the `HashDict` to check
-}
isEmpty : HashDict k comparable v -> Bool
isEmpty hdict =
    D.isEmpty hdict.hashToKV

{-| Determine if a key is a member of a `HashDict`.

Usage: `member k hdict`

- `k`: the key to check for
- `hdict`: the `HashDict` to check
-}
member : k -> HashDict k comparable v -> Bool
member k hdict =
    case D.get (hdict.hasher k) hdict.hashToKV of
        Nothing -> False
        Just els -> L.any (U.fstEq k) els

{-| Retrieve a value from a `HashDict`, or `Nothing` if the key is not a
member.

Usage: `get k hdict`

- `k`: key
- `hdict`: the `HashDict` to retrieve from
-}
get : k -> HashDict k comparable v -> Maybe v
get k hdict =
    D.get (hdict.hasher k) hdict.hashToKV
    |> M.withDefault []
    |> U.filterFirst (U.fstEq k)
    |> M.map snd

{-| Get the size of a `HashDict` - the number of key/value pairs.

Usage: `size hdict`

- `hdict`: the `HashDict`
-}
size : HashDict k comparable v -> Int
size hdict =
    U.dictValMapSum L.length hdict.hashToKV

-- combine

{-| Create a `HashDict` as the union of two other `HashDict`s. The new
`HashDict` will use the hasher from the first `HashDict`.

Usage: `union hdict1 hdict2`

- `hdict1`: first `HashDict`
- `hdict2`: second `HashDict`

Example:

    alice  = {id = 1, name = "Alice"}
    bob    = {id = 2, name = "Bob"}
    hdict1 = HD.singleton .id alice 5
    hdict2 = HD.singleton .id bob 3
    HD.union hdict1 hdict2 -- contains both alice and bob
-}
union : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
union hdict1 hdict2 =
    let up k v un =
        insert k v un
    in foldl up hdict1 hdict2

{-| Create a `HashDict` as the intersection of two other `HashDict`s. The new
`HashDict` will use the hasher from the first `HashDict`.

Usage: `intersect hdict1 hdict2`

- `hdict1`: first `HashDict`
- `hdict2`: second `HashDict`

Example:

    alice = {id = 1, name = "Alice"}
    bob   = {id = 2, name = "Bob"}
    eve   = {id = 3, name = "Eve"}
    hdict1 = HD.fromList .id [(alice, 5), (eve, 8)]
    hdict2 = HD.fromList .id [(bob, 3), (eve, 7)]
    HD.intersect hdict1 hdict2 -- contains (eve, 8)
-}
intersect : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
intersect hdict1 hdict2 =
    let hdict2Member k v =
        member k hdict2
    in filter hdict2Member hdict1

{-| Create a `HashDict` as the difference of two other `HashDict`s. The new
`HashDict` will use the hasher from the first `HashDict`.

Usage: `diff hdict1 hdict2`

- `hdict1`: first `HashDict`
- `hdict2`: second `HashDict`

Example:

    alice = {id = 1, name = "Alice"}
    bob   = {id = 2, name = "Bob"}
    eve   = {id = 3, name = "Eve"}
    hdict1 = HD.fromList .id [(alice, 5), (eve, 8)]
    hdict2 = HD.fromList .id [(bob, 3), (eve, 7)]
    HD.diff hdict1 hdict2 -- contains (alice, 5)
-}
diff : HashDict k comparable v -> HashDict k comparable v -> HashDict k comparable v
diff hdict1 hdict2 =
    let up k v df =
        remove k df
    in foldl up hdict1 hdict2

-- lists

{-| Get a `List` of keys in a `HashDict`.

Usage: `keys hdict`

- `hdict`: the `HashDict`
-}
keys : HashDict k comparable v -> List k
keys = mapKVs fst

{-| Get a `List` of values in a `HashDict`.

Usage: `values hdict`

- `hdict`: the `HashDict`
-}
values : HashDict k comparable v -> List v
values = mapKVs snd

mapKVs : ((k, v) -> r) -> HashDict k comparable v -> List r
mapKVs f hdict =
    D.values hdict.hashToKV
    |> L.concatMap (L.map f)

{-| Convert a `HashDict` to a `List` of key/value pairs.

Usage: `toList hdict`

`hdict`: the `HashDict`
-}
toList : HashDict k comparable v -> List (k, v)
toList hdict =
    D.values hdict.hashToKV
    |> L.concat

{-| Create a `HashDict` from a `List` of key/value pairs.

Usage: `fromList hasher pairs`

- `hasher`: a function that turns keys into `comparable` hashes
- `pairs`: key/value pairs
-}
fromList : H.Hasher k comparable -> List (k, v) -> HashDict k comparable v
fromList hasher pairs =
    let add (k, v) hdict =
        insert k v hdict
    in L.foldl add (empty hasher) pairs

-- transform

{-| Create a new `HashDict` by mapping every value of another `HashDict`.

Usage: `map f hdict`

- `f`: mapping function
- `hdict`: starting `HashDict`

Example:

    alice = { id = 1, name = "Alice" }
    bob = { id = 2, name = "Bob" }
    scores = HD.fromList .id [(alice, 5), (bob, 3)]
    HD.map (\person score -> score + 1) scores -- add 1 to all scores
-}
map : (k -> v -> v') -> HashDict k comparable v -> HashDict k comparable v'
map f hdict =
    let applyF hash kvs =
        LS.map (U.mapBothSnd f) kvs
    in
        { hdict |
            hashToKV = D.map applyF hdict.hashToKV
        }

{-| Left fold over a `HashDict` to combine its key/values pairs into one result.

Key/value pairs are processed in order of their hashes.

Usage: `foldl update acc hdict`

- `update`: updating function
- `acc`: initial accumulator
- `hdict`: the `HashDict`

Example:

    alice = { id = 1, name = "Alice" }
    bob = { id = 2, name = "Bob" }
    scores = HD.fromList .id [(alice, 5), (bob, 3)]

    -- sum all scores: 8
    HD.foldl (\person score total -> total + score) 0 scores

    -- names in reverse-id order: ["Bob", "Alice"]
    HD.foldl (\person score names -> person.name :: names) [] scores
-}
foldl : (k -> v -> r -> r) -> r -> HashDict k comparable v -> r
foldl update acc hdict =
    let update' hash kvs acc' =
        let update'' (k, v) acc'' =
            update k v acc''
        in L.foldl update'' acc' kvs
    in D.foldl update' acc hdict.hashToKV

{-| Right fold over a `HashDict` to combine its key/values pairs into one result.

Key/value pairs are processed in reverse order of their hashes.

Usage: `foldr update acc hdict`

- `update`: updating function
- `acc`: initial accumulator
- `hdict`: the `HashDict`

Example:

    alice = { id = 1, name = "Alice" }
    bob = { id = 2, name = "Bob" }
    scores = HD.fromList .id [(alice, 5), (bob, 3)]

    -- sum all scores: 8
    HD.foldr (\person score total -> total + score) 0 scores

    -- names in id order: ["Alice", "Bob"]
    HD.foldr (\person score names -> person.name :: names) [] scores
-}
foldr : (k -> v -> r -> r) -> r -> HashDict k comparable v -> r
foldr update acc hdict =
    let update' hash kvs acc' =
        let update'' (k, v) acc'' =
            update k v acc''
        in L.foldr update'' acc' kvs
    in D.foldr update' acc hdict.hashToKV

{-| Filter a `HashDict` to a new `HashDict` whose key/value pairs match a predicate.

Usage: `filter pred hdict`

- `pred`: the predicate to test key/value pairs with
- `hdict`: starting `HashDict`

Example:

    alice = { id = 1, name = "Alice" }
    bob = { id = 2, name = "Bob" }
    scores = HD.fromList .id [(alice, 5), (bob, 3)]

    HD.filter (\person score -> score < 4) scores -- contains only Bob
-}
filter : (k -> v -> Bool) -> HashDict k comparable v -> HashDict k comparable v
filter pred hdict =
    let pred' hash kvs =
        L.filter (U.mapPair pred) kvs
        |> U.listToMaybe
    in
        { hdict |
            hashToKV = U.dictFilterMap pred' hdict.hashToKV
        }

{-| Partition a `HashDict` in two: one whose key/value pairs meet a predicate,
and one whose key/value pairs don't.

Usage: `partition pred hdict`

`pred`: the predicate to test key/value pairs with
`hdict`: starting `HashDict`

Example:

    alice = { id = 1, name = "Alice" }
    bob = { id = 2, name = "Bob" }
    scores = HD.fromList .id [(alice, 5), (bob, 3)]

    -- first contains Bob, second contains Alice
    HD.partition (\person score -> score < 4) scores
-}
partition : (k -> v -> Bool) -> HashDict k comparable v -> (HashDict k comparable v, HashDict k comparable v)
partition pred hdict =
    let up k v (left, right) =
        if pred k v
        then (insert k v left, right)
        else (left, insert k v right)
    in foldl up (empty hdict.hasher, empty hdict.hasher) hdict
