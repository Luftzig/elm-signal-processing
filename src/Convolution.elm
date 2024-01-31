module Convolution exposing (convolve, convolveDirectly)

{-| Functions for convolution
See: <https://en.wikipedia.org/wiki/Convolution>


# Functions:

@docs convolve, convolveDirectly

-}

import Complex exposing (multiply)
import FFT exposing (inverseFft, realFft)
import List.Extra


{-| Convolve two real number lists, using FFT for lists longer than 128 elements or direct convolution otherwise.
Threshold was determined by benchmarks.

    convolve [ 1, 0, 0, 0 ] [ 0, 0, 1, 1 ] --> [0, 0, 1, 1]

-}
convolve : List Float -> List Float -> List Float
convolve xs ys =
    if List.length xs >= 128 || List.length ys >= 128 then
        let
            fxs =
                realFft xs

            fys =
                realFft ys
        in
        List.map2 multiply fxs fys
            |> inverseFft
            |> List.map .re

    else
        convolveDirectly xs ys


{-| Convolve two lists of numbers directly by
$$ zs\_j = \\sum\_i^n {xs\_i \* ys\_{j - i}} $$
-}
convolveDirectly : List Float -> List Float -> List Float
convolveDirectly xs ys =
    List.Extra.initialize (List.length xs)
        (\n ->
            List.map2 (*) xs (List.drop n ys)
                |> List.sum
        )
