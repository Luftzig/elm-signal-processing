module Complex exposing (..)

{-| Simple representation of a complex number
-}


type alias Complex =
    { re : Float
    , im : Float
    }


{-| Construct a `Complex` number with imaginary value only

    imaginary 1 == { re = 0, im = 1 }

-}
imaginary : Float -> Complex
imaginary im =
    { re = 0, im = im }


{-| Construct a `Complex` number with real value only

    real 1 == { re = 1, im = 0 }

-}
real : Float -> Complex
real re =
    { re = re, im = 0 }


{-| The complex zero
-}
zero : Complex
zero =
    { re = 0, im = 0 }


{-| Check equality of two complex numbers
-}
equal : Complex -> Complex -> Bool
equal a b =
    a.re == b.re && a.im == b.im


{-| Add two complex numbers

    add (real 1) (imaginary 2) == { re = 1, im = 2 }

-}
add : Complex -> Complex -> Complex
add a b =
    { re = a.re + b.re, im = a.im + b.im }


{-| Subtract two complex numbers

    subtract { re = 1, im = 2 } (real 2) == { re = -1, im = 2 }

-}
subtract : Complex -> Complex -> Complex
subtract a b =
    { re = a.re - b.re, im = a.im - b.im }


{-| Multiply two complex numbers

    multiply (imaginary 1) (real 2) == { re = 0, im = 2 }

-}
multiply : Complex -> Complex -> Complex
multiply a b =
    { re = (a.re * b.re) - (a.im * b.im)
    , im = (a.re * b.im) + (a.im * b.re)
    }


{-| Divide a complex number by a real number.
Notice that the divisor is given first.

    real 2 |> divideByReal 2 == { re = 0, im = 2 }

-}
divideByReal : Float -> Complex -> Complex
divideByReal divisor z =
    { re = z.re / divisor, im = z.im / divisor }


{-| Given complex number z, computes e^z
-}
exp : Complex -> Complex
exp { re, im } =
    { re = (Basics.e ^ re) * cos im
    , im = (e ^ re) * sin im
    }


{-| The absolute value of complex number, which is also its distance from zero
-}
abs : Complex -> Float
abs { re, im } =
    sqrt ((re ^ 2) + (im ^ 2))


{-| computes the conjugate of a complex number

    conjugate { re = 1, im = 1 } == { re = 1, im = -1 }

-}
conjugate : Complex -> Complex
conjugate z =
    { re = z.re, im = -z.im }
