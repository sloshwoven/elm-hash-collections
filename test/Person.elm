module Person
    ( Person
    , different
    , hash1
    , hash2
    , evenId
    , oddId
    , order
    ) where

import String as S
import Util as U

type alias Person =
    { id : Int
    , name : String
    }

different : Person -> Person
different p =
    { id = p.id + 1
    , name = S.reverse p.name
    }

hash1 : Person -> Int
hash1 p =
    p.id % 3

-- sometimes the same as hash1, sometimes different
hash2 : Person -> Int
hash2 p =
    let h1 = hash1 p
    in
        if h1 == 0
        then h1
        else h1 + 1

evenId : Person -> Bool
evenId p =
    p.id % 2 == 0

oddId : Person -> Bool
oddId =
    U.notF evenId

order : Person -> Person -> Order
order p1 p2 =
    case compare p1.id p2.id of
        LT -> LT
        GT -> GT
        EQ -> compare p1.name p2.name
