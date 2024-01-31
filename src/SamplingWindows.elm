module SamplingWindows exposing (..)

import List.Extra


{-| Different windows used for sampling when working with FFT and convolution algorithms.
See: <https://en.wikipedia.org/wiki/Window_function>
-}


{-| The impulse window is a window in which the first element is 1, and all other elements are zero
-}
impulse : Int -> List Float
impulse size =
    if size <= 0 then
        []

    else
        1 :: List.repeat (size - 1) 0


{-| The square window is 1 everywhere
-}
square : Int -> List Float
square size =
    List.repeat size 1


{-| The centered square window is 1 for `width` number of elements around the middle of `size`.
If `size - width` is odd, than the returned window will be smaller than `size` by 1.

    centeredSquare 3 1 --> [0, 1, 0]

    centeredSquare 5 2 -> [0, 1, 1, 0]

-}
centeredSquare : Int -> Int -> List Float
centeredSquare size width =
    if size <= 0 then
        []

    else if width >= size then
        square size

    else
        let
            difference =
                size - width

            offset =
                difference // 2
        in
        List.repeat offset 0 ++ List.repeat width 1 ++ List.repeat offset 0


{-| returns a Blackman window of size n.
See: <https://en.wikipedia.org/wiki/Window_function#Blackman_window>
-}
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
