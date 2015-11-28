# elm-hash-collections

`elm-core`'s `Set` requires elements to be `comparable`, and `Dict` requires
keys to be `comparable`. This library provides `HashSet` and `HashDict`
implementations that allow elements/keys of any type, as long as you provide
a hashing function that can turn an element/key into a unique `comparable`.

The API is identical to `Set` and `Dict`, except that some functions have
a hashing function as an additional argument.
