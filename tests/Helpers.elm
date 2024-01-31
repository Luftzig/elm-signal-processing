module Helpers exposing (..)

import Expect exposing (Expectation, fail, pass)
import List.Extra


listsEqualBy : (a -> a -> Expectation) -> List a -> List a -> Expectation
listsEqualBy check lst1 lst2 =
    let
        equalAtIndex index =
            \( l1, l2 ) ->
                let
                    item1 =
                        List.Extra.getAt index l1

                    item2 =
                        List.Extra.getAt index l2
                in
                case ( item1, item2 ) of
                    ( Nothing, Nothing ) ->
                        pass

                    ( Just v1, Just v2 ) ->
                        check v1 v2

                    _ ->
                        fail ("Lists do not match in length from index " ++ String.fromInt index)

        maxSize =
            max (List.length lst1) (List.length lst2)
    in
    Expect.all (List.range 0 (maxSize - 1) |> List.map equalAtIndex) ( lst1, lst2 )
