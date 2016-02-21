module ListSet
    ( ListSet
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    ) where

import List as L
import Util as U

type alias ListSet e = List e

empty : ListSet e
empty =
    []

singleton : e -> ListSet e
singleton elem =
    [elem]

insert : e -> ListSet e -> ListSet e
insert elem lset =
    if L.member elem lset
    then lset
    else elem :: lset

remove : e -> ListSet e -> ListSet e
remove elem lset =
    L.filter ((/=) elem) lset

isEmpty : ListSet e -> Bool
isEmpty lset =
    L.isEmpty lset

member : e -> ListSet e -> Bool
member elem lset =
    L.member elem lset

size : ListSet e -> Int
size lset =
    L.length lset

union : ListSet e -> ListSet e -> ListSet e
union lset1 lset2 =
    diff lset1 lset2
    |> L.append lset2

intersect : ListSet e -> ListSet e -> ListSet e
intersect lset1 lset2 =
    L.filter (memberOf lset2) lset1

diff : ListSet e -> ListSet e -> ListSet e
diff lset1 lset2 =
    L.filter (U.notF <| memberOf lset2) lset1

memberOf : List e -> e -> Bool
memberOf = flip L.member

toList : ListSet e -> List e
toList lset =
    lset

fromList : List e -> ListSet e
fromList list =
    L.foldr insert empty list

map : (e -> e') -> ListSet e -> ListSet e'
map f lset =
    let insertMapped elem lset' =
        insert (f elem) lset'
    in L.foldr insertMapped empty lset

foldl : (e -> b -> b) -> b -> ListSet e -> b
foldl = L.foldl

foldr : (e -> b -> b) -> b -> ListSet e -> b
foldr = L.foldr

filter : (e -> Bool) -> ListSet e -> ListSet e
filter = L.filter

partition : (e -> Bool) -> ListSet e -> (ListSet e, ListSet e)
partition = L.partition
