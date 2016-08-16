# Ratio

A simple module for working with rational numbers.

This is forked from imeckler/Ratio in order to achieve the following:

1.  Normalisation of rationals with negative denominators

    Rational 1 (-3) becomes Rational (-1) 3
    Rational (-1) (-3) becomes Rational 1 3
    
2.  Further combinators - __eq, ne, gt, ge, lt, le, max, min__ etc.

Note, although the original mentions Rationals of arbitrary precision up to _max.int_, it also supports (for example) addition and multiplication.  It is unclear (from the documentation) how this will behave for large values.  My purposes are for uses where the sizes are relatively small.  It is also unclear how

    Rational 1 0
    
will behave.


