module Hasher (Hasher) where

{-|
# Definition
@docs Hasher
-}

{-|-}
type alias Hasher k comparable = k -> comparable
