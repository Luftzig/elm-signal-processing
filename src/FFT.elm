module FFT.FFT exposing (fft, inverseFft, realFft, realInverseFft, zeroPadTo, zeroPadToNextPowerOf2)

import Complex exposing (..)


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


realFft : List Float -> List Complex
realFft floats =
    floats
        |> List.map real
        |> fft


realInverseFft : List Float -> List Complex
realInverseFft floats =
    floats
        |> List.map real
        |> inverseFft
