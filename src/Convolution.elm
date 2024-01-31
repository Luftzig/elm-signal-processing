module Convolution exposing (..)

import Complex exposing (multiply)
import FFT.FFT exposing (inverseFft, realFft)
import List.Extra


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


convolveDirectly : List Float -> List Float -> List Float
convolveDirectly xs ys =
    List.Extra.initialize (List.length xs)
        (\n ->
            List.map2 (*) xs (List.drop n ys)
                |> List.sum
        )
