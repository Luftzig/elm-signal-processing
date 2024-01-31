module FFT exposing
    ( fft, inverseFft
    , realFft, realInverseFft
    )

{-| Functions for calculating the discrete finite fourier transform using the fast fourier transform algorithms.


# Complex values

Functions that work on complex values

@docs fft, inverseFft


# Real values

Functions that work real (`Float`) values

@docs realFft, realInverseFft

-}

import Complex exposing (..)
import Padding exposing (zeroPadToNextPowerOf2)


splitEvensAndOdds : List a -> ( List a, List a )
splitEvensAndOdds list =
    case list of
        [] ->
            ( [], [] )

        x :: [] ->
            ( [ x ], [] )

        x :: y :: xs ->
            let
                ( xs_, ys_ ) =
                    splitEvensAndOdds xs
            in
            ( x :: xs_, y :: ys_ )


{-| compute the fourier transform of a list of complex values

    fft [ real 1, zero, real 1, imaginary 0.5 ]

-}
fft : List Complex -> List Complex
fft array =
    let
        fftInner xs =
            case List.length xs of
                0 ->
                    []

                1 ->
                    xs

                n ->
                    let
                        ( evens, odds ) =
                            splitEvensAndOdds xs

                        ys =
                            fft evens

                        zs =
                            fft odds

                        ts =
                            List.indexedMap
                                (\i z ->
                                    multiply (coefficient i) z
                                )
                                zs

                        coefficient k =
                            exp (imaginary <| -2 * pi * (toFloat k / toFloat n))
                    in
                    List.map2 add ys ts
                        ++ List.map2 subtract ys ts
    in
    fftInner (zeroPadToNextPowerOf2 array)


{-| compute the inverse fourier transform of a list of complex values
The inverse fourier transform of list Z is equal to $$(conjugate (fft (conjugate Z)) / (length Z))$$
where Z\* is the conjugate of Z.
-}
inverseFft : List Complex -> List Complex
inverseFft list =
    let
        paddedInput =
            zeroPadToNextPowerOf2 list
    in
    paddedInput
        |> List.map conjugate
        |> fft
        |> List.map conjugate
        |> List.map (divideByReal (List.length paddedInput |> toFloat))


{-| compute the fourier transform of a list of real values. The output is a list of complex values.

    realFft [ 1, 0, 1, 0.5 ]

-}
realFft : List Float -> List Complex
realFft floats =
    floats
        |> List.map real
        |> fft


{-| compute the inverse fourier transform of a list of real values
The inverse fourier transform of list Z is equal to $$(conjugate (fft (conjugate Z)) / (length Z))$$
where Z\* is the conjugate of Z.
-}
realInverseFft : List Float -> List Complex
realInverseFft floats =
    floats
        |> List.map real
        |> inverseFft
