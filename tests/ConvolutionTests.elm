module ConvolutionTests exposing (..)

import Convolution
import Expect
import Test exposing (describe, test)


convolutionTestSuit : Test.Test
convolutionTestSuit =
    describe "Convolution"
        [ test "convolve" <|
            \() ->
                let
                    in1 =
                        [ 1, 0, 0, 0 ]

                    in2 =
                        [ 1, 1, 1, 1 ]

                    expected =
                        [ 1, 1, 1, 1 ]
                in
                Convolution.convolve in1 in2
                    |> Expect.equalLists expected
        ]
