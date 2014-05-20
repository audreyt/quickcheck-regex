quickcheck-regex
================

This module exports a `matching` function that turns a regular expression into a generator for strings matching that regex.

For example:

    >>> import Test.QuickCheck.Regex (matching)
    >>> import Test.QuickCheck (generate)
    >>> generate (matching "[-a-z0-9._%]+@[-a-z0-9.]+\\.[a-z]{3,18}\\.(asia|eu|today)")
    "9%az4rmek@rar1d8qvo04jkd1.agzy.asia"

# CC0 1.0 Universal

To the extent possible under law, 唐鳳 has waived all copyright
and related or neighboring rights to quickcheck-regex.

This work is published from Taiwan.

http://creativecommons.org/publicdomain/zero/1.0
