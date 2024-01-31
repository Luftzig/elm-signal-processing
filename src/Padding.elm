module Padding exposing (..)

import Complex exposing (Complex, zero)


zeroPadTo : Int -> List Complex -> List Complex
zeroPadTo n xs =
    if List.length xs >= n then
        xs

    else
        xs ++ List.repeat (n - List.length xs) zero


zeroPadToNextPowerOf2 : List Complex -> List Complex
zeroPadToNextPowerOf2 zs =
    let
        length =
            List.length zs

        nextPowerOf2 : Int
        nextPowerOf2 =
            2 ^ (ceiling <| logBase 2 (toFloat length))
    in
    zeroPadTo nextPowerOf2 zs
