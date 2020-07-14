# `fused-effects-profile`

This is a Haskell package providing a `fused-effects` effect for profiling your programs.

<img width="486" alt="Screen Shot 2020-01-05 at 10 54 22 PM" src="https://user-images.githubusercontent.com/59671/71844813-04fa6300-3095-11ea-9d7b-add63e7a6026.png">

This isn’t a replacement for GHC’s profiling; but rather, it allows you to enable and disable profiling in a program without recompiling it or its dependencies. (In the above screenshot, the `--profile` flag controls whether or not profiling is run.) Like `SCC` pragmas, it also involves marking up portions of the program with `measure` blocks, allowing you to indicate precisely the portions you want to measure.

It can also be enabled or disabled in different parts of a program separately, and a custom algebra could even allow it to be toggled on the fly in a running process.
