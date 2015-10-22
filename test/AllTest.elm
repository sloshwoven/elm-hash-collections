import Check as C
import Check.Runner.Browser as B
import Html as H

main : H.Html
main =
    []
    |> C.suite "elm-hash-collections suite"
    |> C.quickCheck
    |> B.display
