module Hasher (Hasher) where

{-|
# Definition
@docs Hasher
-}

{-| A function that hashes a value to a `comparable`.

`v`: value type
`comparable`: hash type
-}
type alias Hasher v comparable =
    v -> comparable
