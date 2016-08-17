# Ratio

A simple module for working with rational numbers.

This is forked from imeckler/Ratio in order to achieve the following:

1.  Normalisation of rationals with negative denominators

    Rational 1 (-3) becomes Rational (-1) 3
    
    Rational (-1) (-3) becomes Rational 1 3
    
2.  Further combinators - __eq, ne, gt, ge, lt, le, max, min__ etc.

3.  Beginnings of experimental arithmetic infix combinators


Behaviour of Infinity is interesting:

Unlike in Haskell, you can produce (say) Rational 1 0.  This can be used in arithmetic and so, for example

    multiply (over 5 1) (over 1 0)
    
gives a Rational 1 0.  Also, divide by infinity:

    divide (over 5 1) (over 1 0)
    
gives a Rational 0 1 as expected.  Also (of course), if you test _toFloat (over 1 0)_ with _isInfinite_ then it returns True as expected.  And finally

    multiply (over max.int 1) (over max.int 1)
    
gives a Rational 1 0.  So, in summary, Rational 1 0 seems to consistently represent infinity. 

