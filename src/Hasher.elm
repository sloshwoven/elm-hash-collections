module Hasher (Hasher) where

{-|
# Definition
@docs Hasher
-}

{-| A function that hashes a value to a comparable.
-}
type alias Hasher k comparable = k -> comparable
