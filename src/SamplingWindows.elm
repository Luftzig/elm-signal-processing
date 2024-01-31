module SamplingWindows exposing (..)

import List.Extra


blackmanWindow : Int -> List Float
blackmanWindow size =
    let
        a0 =
            0.42

        a1 =
            0.5

        a2 =
            0.08
    in
    List.Extra.initialize size
        (\i ->
            a0
                - (a1 * cos (2 * pi * toFloat i / toFloat size))
                + (a2 * cos (4 * pi * toFloat i / toFloat size))
        )
