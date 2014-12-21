# Multiset for Elm

Represent a collection of counted elements in [Elm](elm-lang.org), by Max Goldstein.

Documentation is provided for each function. Submit bugs, requests, and the like through GitHub.

Multisets are wrappers around Dicts, which (as of 0.14) do not support
equality. Therefore the library provides an `equals` function. It considers
missing values to equal values of zero. However, values of zero are removed by
the library anyway. If you can create a value of zero without using the Dict
library directly, it's a bug so please report it.

