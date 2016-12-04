# Ratio

deprecated in favour of https://github.com/elm-community/ratio
--------------------------------------------------------------

An elm 0.18 module for working with rational numbers.

This is forked from imeckler/Ratio in order to achieve the following:

1.  Normalisation of rationals with negative denominators

    Rational 1 (-3) becomes Rational (-1) 3

    Rational (-1) (-3) becomes Rational 1 3

2.  Further combinators for comparing two Rationals - __eq, ne, gt, ge, lt, le, compare, max, min__ etc.

3.  Further arithmetic combinators - __floor, ceiling, truncate, round, invert__ etc.

4.  Optional infix forms of the comparison operators - __|==|, |/=|__ etc.

5.  Infix operators for mixed type arithmetic incorporating both Rationals and Ints.  Where both operands are Rationals, we surround the operator with the _'|'_ symbol.  Where one operand is a Rational and the other an Int, we place the _'|'_ symbol at the side of the operator adjacent to the Rational.  For example (where r is a Rational and i an Int):

```elm
    r |+| r

    i +| r

    r |+ i
```

### Notes

Behaviour of Infinity is interesting:

Unlike in Haskell, you can produce (say) Rational 1 0.  This can be used in arithmetic and so, for example

```elm
multiply (over 5 1) (over 1 0)
```

gives a Rational 1 0.  Also, divide by infinity:

```elm
divide (over 5 1) (over 1 0)
```

gives a Rational 0 1 as expected.  Also (of course), if you test _toFloat (over 1 0)_ with _isInfinite_ then it returns True as expected.  And finally

```elm
multiply (over max.int 1) (over max.int 1)
```

gives a Rational 1 0.  So, in summary, Rational 1 0 seems consistently to represent infinity.
