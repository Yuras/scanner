# scanner
Fast non-backtracking incremental combinator parsing for bytestrings

[![Build Status](https://travis-ci.org/Yuras/scanner.svg?branch=master)](https://travis-ci.org/Yuras/scanner)

On hackage: http://hackage.haskell.org/package/scanner

It is often convinient to use backtracking to parse some sofisticated
input. Unfortunately it kills performance, so usually you should avoid
backtracking.

Often (actually always, but it could be too hard sometimes) you can
implement your parser without any backtracking. It that case all the
bookkeeping usuall parser combinators do becomes unnecessary. The
scanner libarary is designed for such cases. It is often 2 times faster
then attoparsec.

As an example, please checkout redis protocol parser included into the
repo, both using attoparsec and scanner libraries:
https://github.com/Yuras/scanner/tree/master/examples/Redis

Benchmark results:

![Bechmark results](https://raw.githubusercontent.com/Yuras/scanner/master/bench/bench.png)
