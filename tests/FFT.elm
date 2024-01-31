module FFT exposing (..)

import Complex exposing (Complex, real, zero)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import FFT.FFT as FFT exposing (zeroPadToNextPowerOf2)
import Fuzz
import Helpers exposing (listsEqualBy)
import List.Extra
import Test exposing (Test, describe, fuzz, test)


complexWithin : FloatingPointTolerance -> Complex -> Complex -> Expectation
complexWithin tolerance expected z =
    Expect.all
        [ \{ re } -> Expect.within tolerance expected.re re
        , \{ im } -> Expect.within tolerance expected.im im
        ]
        z


fftTestSuit : Test
fftTestSuit =
    describe "FFT algorithm"
        [ test "on impulse [1, 0...]" <|
            \() ->
                FFT.fft [ real 1, zero ]
                    |> Expect.equalLists
                        [ real 1, real 1 ]
        , fuzz (Fuzz.intRange 1 4) "impulse powers of 2" <|
            \n ->
                FFT.fft (real 1 :: List.repeat ((2 ^ n) - 1) zero)
                    |> Expect.equalLists
                        (List.repeat (2 ^ n) (real 1))
        , fuzz (Fuzz.intRange 1 4) "alternating powers of 2" <|
            \n ->
                FFT.fft
                    (List.Extra.interweave
                        (List.repeat (2 ^ (n - 1)) (real 1))
                        (List.repeat (2 ^ (n - 1)) (real -1))
                    )
                    |> Expect.equalLists
                        (List.repeat (2 ^ n) zero
                            |> List.Extra.setAt (2 ^ (n - 1)) (real (2 ^ toFloat n))
                        )
        , fuzz (Fuzz.intRange 1 30) "padding" <|
            \n ->
                let
                    length =
                        List.length (zeroPadToNextPowerOf2 (List.repeat n (real 1)))

                    nextPowerOf2 =
                        2 ^ (ceiling <| logBase 2 (toFloat n))
                in
                length
                    |> Expect.equal nextPowerOf2
        , test "zeroes" <|
            \() ->
                FFT.fft [ zero, zero ]
                    |> Expect.equalLists
                        [ zero, zero ]
        , test "simple FFT test case" <|
            \() ->
                let
                    inputs =
                        [ 1, 0, 1, 0 ]
                            |> List.map real

                    expected =
                        [ 2, 0, 2, 0 ]
                            |> List.map real
                in
                FFT.fft inputs
                    |> Expect.equalLists
                        expected
        , test "cosine" <|
            \() ->
                let
                    cosineList =
                        List.Extra.initialize 32 (\i -> real <| cos (toFloat i / 32 * pi))

                    result =
                        FFT.fft cosineList
                in
                List.Extra.getAt 16 result
                    |> Maybe.map Complex.abs
                    |> Maybe.withDefault (1 / 0)
                    |> Expect.within (Relative 0.0001) 1.0
        , fuzz (Fuzz.listOfLengthBetween 4 64 (Fuzz.floatRange -1.0e10 1.0e10) |> Fuzz.map (List.map real))
            "inverse FFT"
          <|
            \zs ->
                FFT.fft zs
                    |> FFT.inverseFft
                    |> listsEqualBy (complexWithin (Absolute 1.0e-5)) (zeroPadToNextPowerOf2 zs)
        ]
