module ListSet
    ( insert, remove
    , union, intersect, diff
    , setize
    , map
    ) where

import List as L
import Util as U

insert : e -> List e -> List e
insert elem lset =
    if L.member elem lset
    then lset
    else elem :: lset

remove : e -> List e -> List e
remove elem lset =
    L.filter ((/=) elem) lset

union : List e -> List e -> List e
union lset1 lset2 =
    diff lset1 lset2
    |> L.append lset2

intersect : List e -> List e -> List e
intersect lset1 lset2 =
    L.filter (U.memberOf lset2) lset1

diff : List e -> List e -> List e
diff lset1 lset2 =
    L.filter (U.notF <| U.memberOf lset2) lset1

setize : List e -> List e
setize list =
    L.foldr insert [] list

map : (e -> e') -> List e -> List e'
map f lset =
    let insertMapped elem lset' =
        insert (f elem) lset'
    in L.foldr insertMapped [] lset
