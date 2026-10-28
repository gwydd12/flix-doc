
<h1 align="center">
  flix-doc
  <br>
</h1>

An reimplementation of the pretty printing algorithm of flixfmt in Haskell.

## Description

This project is based on my implementation in Scala for the source code formatter flixfmt which is the canonical source code formatter for the Flix programming language. I had some spare time and implemented the pretty printing, which is a simplified version of Wadler and Leijen in Haskell, solely for the purpose of learning Haskell.

*It might not be perfect but it does it's honest work*
## Concepts

* Wadler based `Document`
* Leijen based layout aware combinators
* Futhark formatter based line preservation with SingleLine and MultiLine

This is not the almighty pretty printing algorithm, it is way less expressive than most. However, it allows for line preservation and simplifies the implementation for these types of source code formatter.

## How to use

This is intended to be used as a library.

## Acknowledgments

* flixfmt (PS: I wrote that one :))
* Wadler for his prettier algorithm
* Leijen for his extension of the prettier algorithm
* Futhark formatter for their amazing master project.


