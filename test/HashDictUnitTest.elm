module HashDictUnitTest ( hashDictUnitSuite) where

import ElmTest as ET
import HashDict as HD

hashDictUnitSuite : ET.Test
hashDictUnitSuite =
    ET.suite "HashDict unit tests"
        [ hashCollisionDupValues ]

hashCollisionDupValues : ET.Test
hashCollisionDupValues =
    HD.fromList .id
        [ ({id = 1, name = "a"}, 8)
        , ({id = 2, name = "b"}, 9)
        , ({id = 2, name = "c"}, 9)
        ]
    |> HD.values
    |> ET.equals [8, 9, 9]
