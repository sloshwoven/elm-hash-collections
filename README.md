# elm-hash-collections

`elm-core`'s `Set` requires elements to be `comparable`, and `Dict` requires
keys to be `comparable`. This library provides `HashSet` and `HashDict`
implementations that allow elements/keys of any type, as long as you provide
a hashing function that can turn an element/key into a unique `comparable`.

The API is identical to `Set` and `Dict`, except that some functions have
a hashing function as an additional argument.

## Examples

### HashSet

```elm
import HashSet as HS

alice = { id = 1, name = "Alice" }
bob = { id = 2, name = "Bob" }

people = HS.singleton .id alice

HS.member alice people -- returns True
HS.member bob people -- returns False
```

### HashDict

```elm
import HashDict as HD

alice = { id = 1, name = "Alice" }
bob = { id = 2, name = "Bob" }

scores = HD.fromList .id [(alice, 10), (bob, 5)]

HD.get alice scores -- returns Just 10
HD.get bob scores -- returns Just 5
```
