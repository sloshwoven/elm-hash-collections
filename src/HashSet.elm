module HashSet
    ( HashSet
    , empty, singleton, insert, remove
    , isEmpty, member
    , foldl, foldr, map, map'
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
@docs isEmpty, member

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, map', foldl, foldr, filter, partition
-}

import Dict as D
import Hasher as H
import List as L

{-| A collection of elements without order or duplicates.

- `e`: element type
- `comparable`: hash type

- `hasher`: a function that turns elements into unique `comparable` values
- `hashToElem`: a `Dict` of hashes to elements -}
type alias HashSet e comparable =
    { hasher     : H.Hasher e comparable
    , hashToElem : D.Dict comparable e
    }

-- build

{-| Create an empty `HashSet`.

Usage: `empty hasher`

- `hasher`: a function that turns elements into unique `comparable` values
-}
empty : H.Hasher e comparable -> HashSet e comparable
empty hasher =
    { hasher     = hasher
    , hashToElem = D.empty
    }

{-| Create a `HashSet` containing a single element.

Usage: `singleton hasher elem`

- `hasher`: a function that turns elements into unique `comparable` values
- `elem`: an element
-}
singleton : H.Hasher e comparable -> e -> HashSet e comparable
singleton hasher elem =
    { hasher     = hasher
    , hashToElem = D.singleton (hasher elem) elem
    }

{-| Create a `HashSet` by adding an element to another `HashSet`.

Usage: `insert elem hset`

- `elem`: element to add
- `hset`: starting `HashSet`
-}
insert : e -> HashSet e comparable -> HashSet e comparable
insert elem hset =
    { hset |
        hashToElem <- D.insert (hset.hasher elem) elem hset.hashToElem
    }

{-| Create a `HashSet` by removing an element from another `HashSet`.

Usage: `remove elem hset`

- `elem`: element to remove
- `hset`: starting `HashSet`
-}
remove : e -> HashSet e comparable -> HashSet e comparable
remove elem hset =
    { hset |
        hashToElem <- D.remove (hset.hasher elem) hset.hashToElem
    }

-- query

{-| Determine if a `HashSet` is empty.

Usage: `isEmpty hset`

- `hset`: the `HashSet` to check
-}
isEmpty : HashSet e comparable -> Bool
isEmpty hset =
    D.isEmpty hset.hashToElem

{-| Determine if an element is a member of a `HashSet`.

Usage: `member elem hset`

- `elem`: element to check for
- `hset`: the `HashSet` to check
-}
member : e -> HashSet e comparable -> Bool
member elem hset =
    D.member (hset.hasher elem) hset.hashToElem

-- combine

{-| Create a `HashSet` as the union of two other `HashSet`s. The new `HashSet`
will use the hasher from the first `HashSet`.

Usage: `union hset1 hset2`

- `hset1`: first `HashSet`
- `hset2`: second `HashSet`

Example:

    alice = {id = 1, name = "Alice"}
    bob   = {id = 2, name = "Bob"}
    hset1 = HS.singleton .id alice
    hset2 = HS.singleton .id bob
    HS.union hset1 hset2 -- contains both alice and bob
-}
union : HashSet e comparable -> HashSet e comparable -> HashSet e comparable
union hset1 hset2 =
    { hset1 |
        hashToElem <- D.union hset1.hashToElem hset2.hashToElem
    }

{-| Create a `HashSet` as the intersection of two other `HashSet`s. The new
`HashSet` will use the hasher from the first `HashSet`.

Usage: `intersect hset1 hset2`

- `hset1`: first `HashSet`
- `hset2`: second `HashSet`

Example:

    alice = {id = 1, name = "Alice"}
    bob   = {id = 2, name = "Bob"}
    eve   = {id = 3, name = "Eve"}
    hset1 = HS.fromList .id [alice, eve]
    hset2 = HS.fromList .id [bob, eve]
    HS.intersect hset1 hset2 -- contains eve
-}
intersect : HashSet e comparable -> HashSet e comparable -> HashSet e comparable
intersect hset1 hset2 =
    { hset1 |
        hashToElem <- D.intersect hset1.hashToElem hset2.hashToElem
    }

{-| Create a `HashSet` as the difference of two other `HashSet`s. The new
`HashSet` will use the hasher from the first `HashSet`.

Usage: `diff hset1 hset2`

- `hset1`: first `HashSet`
- `hset2`: second `HashSet`

Example:

    alice = {id = 1, name = "Alice"}
    bob   = {id = 2, name = "Bob"}
    eve   = {id = 3, name = "Eve"}
    hset1 = HS.fromList .id [alice, eve]
    hset2 = HS.fromList .id [bob, eve]
    HS.diff hset1 hset2 -- contains alice
-}
diff : HashSet e comparable -> HashSet e comparable -> HashSet e comparable
diff hset1 hset2 =
    { hset1 |
        hashToElem <- D.diff hset1.hashToElem hset2.hashToElem
    }

-- lists

{-| Convert a `HashSet` to a `List` of its elements.

Usage: `toList hset`

- `hset`: the `HashSet`
-}
toList : HashSet e comparable -> List e
toList hset =
    D.values hset.hashToElem

{-| Create a `HashSet` from a `List` of elements.

Usage: `fromList hasher elems`

- `hasher`: a function that turns elements into unique `comparable` values
- `elems`: elements to include
-}
fromList : H.Hasher e comparable -> List e -> HashSet e comparable
fromList hasher elems =
    let toPair elem = (hasher elem, elem)
    in
        { hasher     = hasher
        , hashToElem = D.fromList <| L.map toPair elems
        }

-- transform

{-| Map every element of a `HashSet` to a new `HashSet`.

Note: the hash type of the new `HashSet` must be the same as the hash type of
the original `HashSet` (since there is no way to distinguish two different
`comparable` types in an Elm function signature).

Usage: `map f hasher hset`

- `f`: mapping function
- `hasher`: hasher for the new `HashSet`
- `hset`: starting `HashSet`

Example:

    people = HS.fromList .id [{id = 1, name = "Alice"}, {id = 2, name = "Bob"}]
    HS.map .id identity people -- contains 1 and 2
-}
map : (e -> e') -> H.Hasher e' comparable -> HashSet e comparable -> HashSet e' comparable
map f hasher hset =
    fromList hasher (L.map f <| D.values hset.hashToElem)

{-| The same as `map`, but the output element type is the same as the
input element type, and the hasher of the input is reused.

Usage: `map' f hset`

- `f`: mapping function
- `hset`: starting `HashSet`

Example:

    people = HS.fromList .id [{id = 1, name = "Alice"}, {id = 2, name = "Bob"}]
    HS.map (\p -> {p | name <- toUpper p.name}) people
-}
map' : (e -> e) -> HashSet e comparable -> HashSet e comparable
map' f hset =
    map f hset.hasher hset

{-| Left fold over a `HashSet` to combine its elements into one result.

Elements are processed in order of their hashes.

Usage: `foldl update acc hset`

- `update`: updating function
- `acc`: initial accumulator
- `hset`: the `HashSet`

Example:

    people = HS.fromList .id [{id = 1, name = "Alice"}, {id = 2, name = "Bob"}]
    HS.foldl (\e r -> L.append r [e.name]) [] people -- ["Alice", "Bob"]
-}
foldl : (e -> r -> r) -> r -> HashSet e comparable -> r
foldl update acc hset =
    L.foldl update acc <| D.values hset.hashToElem

{-| Right fold over a `HashSet` to combine its elements into one result.

Elements are processed in reverse order of their hashes.

Usage: `foldr update acc hset`

- `update`: updating function
- `acc`: initial accumulator
- `hset`: the `HashSet`

Example:

    people = HS.fromList .id [{id = 1, name = "Alice"}, {id = 2, name = "Bob"}]
    HS.foldr (\e r -> L.append r [e.name]) [] people -- ["Bob", "Alice"]
-}
foldr : (e -> r -> r) -> r -> HashSet e comparable -> r
foldr update acc hset =
    L.foldr update acc <| D.values hset.hashToElem

{-| Filter a `HashSet` to a new `HashSet` whose elements match a predicate.

Usage: `filter pred hset`

- `pred`: the predicate to test elements with
- `hset`: starting `HashSet`

Example:

    people = HS.fromList .id [{id = 1, name = "Alice"}, {id = 2, name = "Bob"}]
    HS.filter (\p -> p.id == 2) people -- contains only Bob
-}
filter : (e -> Bool) -> HashSet e comparable -> HashSet e comparable
filter pred hset =
    fromList hset.hasher (L.filter pred <| D.values hset.hashToElem)

{-| Partition a `HashSet` in two: one whose elements meet a predicate, and one
whose elements don't.

Usage: `partition pred hset`

`pred`: the predicate to test elements with
`hset`: starting `HashSet`

Example:

    people = HS.fromList .id [{id = 1, name = "Alice"}, {id = 2, name = "Bob"}]
    HS.partition (\p -> p.id == 2) people -- first contains Bob, second contains Alice
-}
partition : (e -> Bool) -> HashSet e comparable -> (HashSet e comparable, HashSet e comparable)
partition pred hset =
    let pred' k v = pred v
        parts = D.partition pred' hset.hashToElem
    in
        ( { hset | hashToElem <- fst parts }
        , { hset | hashToElem <- snd parts }
        )
