module BenchmarksMain exposing (..)

import Array
import Benchmark exposing (Benchmark, benchmark, describe, scale)
import Benchmark.Runner exposing (program)
import FFT.FFT as FFT exposing (real)
import Random


main =
    program fftbenchmarks


fftbenchmarks : Benchmark
fftbenchmarks =
    let
        input128 =
            List.range 0 127
                |> List.map (toFloat >> (/) 128 >> (*) pi >> cos >> real)

        input128NoiseFloat =
            Random.step
                (Random.list 128 (Random.float -1 1))
                (Random.initialSeed 154324632)
                |> Tuple.first

        input128NoiseComplex =
            input128NoiseFloat
                |> List.map real

        input256NoiseFloat =
            input128NoiseFloat ++ input128NoiseFloat

        input512NoiseFloat =
            input256NoiseFloat ++ input256NoiseFloat

        input256 =
            List.range 0 255
                |> List.map (toFloat >> (/) 128 >> (*) pi >> cos >> real)

        input512 =
            List.range 0 511
                |> List.map (toFloat >> (/) 128 >> (*) pi >> cos >> real)

        input1024 =
            List.append input512 input512

        inputArray128 =
            Array.fromList input128

        inputArray256 =
            Array.fromList input256

        inputArray512 =
            Array.fromList input512

        inputArray1024 =
            Array.append inputArray512 inputArray512

        makeInput index =
            ( (2 ^ toFloat index |> round |> String.fromInt) ++ " samples"
            , List.range 0 (2 ^ toFloat index |> round)
                |> List.map (toFloat >> (/) 128 >> (*) pi >> cos >> real)
            )
    in
    describe "FFT"
        [ --[ scale "list based" <|
          --    (List.range 5 8
          --        |> List.map makeInput
          --        |> List.map (Tuple.mapSecond (\input -> \() -> List.length input))
          --     --|> List.map (Tuple.mapSecond (\input -> \() -> FFT.fft input))
          --    )
          --,
          describe "convolve"
            [ Benchmark.benchmark "128 samples"
                (\() ->
                    FFT.convolve input128NoiseFloat input128NoiseFloat
                )
            , Benchmark.benchmark "256 samples"
                (\() ->
                    FFT.convolve input256NoiseFloat input256NoiseFloat
                )
            ]

        --,
        --Benchmark.compare "128 samples List vs Array"
        --  "List"
        --  (\_ -> FFT.fft input128)
        --  "Array"
        --  (\_ -> FFT.fftArray inputArray128)
        ]
