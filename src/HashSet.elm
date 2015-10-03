module HashSet
    ( HashSet
    , empty, singleton, insert, remove
    , isEmpty, member
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
@docs isEmpty, member

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
type alias HashSet e comparable =
    { hasher     : e -> comparable
    , hashToElem : D.Dict comparable e
    }

-- build

{-|-}
empty : (e -> comparable) -> HashSet e comparable
empty hasher =
    { hasher     = hasher
    , hashToElem = D.empty
    }

{-|-}
singleton : (e -> comparable) -> e -> HashSet e comparable
singleton hasher elem =
    { hasher     = hasher
    , hashToElem = D.singleton (hasher elem) elem
    }

{-|-}
insert : e -> HashSet e comparable -> HashSet e comparable
insert elem hset =
    { hset |
        hashToElem <- D.insert (hset.hasher elem) elem hset.hashToElem
    }

{-|-}
remove : e -> HashSet e comparable -> HashSet e comparable
remove elem hset =
    { hset |
        hashToElem <- D.remove (hset.hasher elem) hset.hashToElem
    }

-- query

{-|-}
isEmpty : HashSet e comparable -> Bool
isEmpty hset =
    D.isEmpty hset.hashToElem

{-|-}
member : e -> HashSet e comparable -> Bool
member elem hset =
    D.member (hset.hasher elem) hset.hashToElem

-- combine

{-|-}
union : HashSet e comparable -> HashSet e comparable -> HashSet e comparable
union hset1 hset2 =
    { hset1 |
        hashToElem <- D.union hset1.hashToElem hset2.hashToElem
    }

{-|-}
intersect : HashSet e comparable -> HashSet e comparable -> HashSet e comparable
intersect hset1 hset2 =
    { hset1 |
        hashToElem <- D.intersect hset1.hashToElem hset2.hashToElem
    }

{-|-}
diff : HashSet e comparable -> HashSet e comparable -> HashSet e comparable
diff hset1 hset2 =
    { hset1 |
        hashToElem <- D.diff hset1.hashToElem hset2.hashToElem
    }

-- lists

{-|-}
toList : HashSet e comparable -> List e
toList hset =
    D.values hset.hashToElem

{-|-}
fromList : (e -> comparable) -> List e -> HashSet e comparable
fromList hasher elems =
    let toPair elem = (hasher elem, elem)
    in
        { hasher     = hasher
        , hashToElem = D.fromList <| L.map toPair elems
        }

-- transform

{-|-}
map : (e -> e') -> (e' -> comparable) -> HashSet e comparable -> HashSet e' comparable
map f hasher hset =
    fromList hasher (L.map f <| D.values hset.hashToElem)

{-|
uses hash order
-}
foldl : (e -> r -> r) -> r -> HashSet e comparable -> r
foldl update acc hset =
    L.foldl update acc <| D.values hset.hashToElem

{-|
uses hash order
-}
foldr : (e -> r -> r) -> r -> HashSet e comparable -> r
foldr update acc hset =
    L.foldr update acc <| D.values hset.hashToElem

{-|-}
filter : (e -> Bool) -> HashSet e comparable -> HashSet e comparable
filter pred hset =
    fromList hset.hasher (L.filter pred <| D.values hset.hashToElem)

{-|-}
partition : (e -> Bool) -> HashSet e comparable -> (HashSet e comparable, HashSet e comparable)
partition pred hset =
    let pred' k v = pred v
        parts = D.partition pred' hset.hashToElem
    in
        ( { hset | hashToElem <- fst parts }
        , { hset | hashToElem <- snd parts }
        )
