
## YAPB: Yet Another Parser Builder

### A programmable parser builder system
- Allows to write LALR(1) parser specifications in Haskell
- Provides an automatic syntax completion method

### Library, tools, and examples
- yapb: a library for a programmable parser builder system
- yapb-exe: a wrapper interface to YAPB
- conv-exe: a grammar format utility for conversion of a readable grammar (.lgrm) format into the Haskell data format (.grm)
- syncomp-exe: a syntax completion server for Emacs
- Examples: 
  - parser-exe: an arithmetic parser
  - polyrpc-exe: a polyrpc programming language system including a parser, a poly rpc type checker, a slicing compiler, a poly cs type checker, and a poly cs interpter.

### Download and build
~~~
  $ git clone https://github.com/kwanghoon/yapb
  $ cd yapb
  $ stack build
~~~

### Tutorial
- [How to write and run a parser](https://github.com/kwanghoon/swlab_parser_builder/blob/master/doc/Tutorial-parser.md)
- [How to write and run a syntax completion server for Emacs](https://github.com/kwanghoon/swlab_parser_builder/blob/master/doc/Tutorial-syntax-completion.md)
- [A top-down approach to writing a compiler for arithmetic expressions](https://github.com/kwanghoon/swlab_parser_builder/blob/master/doc/tutorial_swlab_parser_builder.txt) Written in Korean.

### Reference
- [References](https://github.com/kwanghoon/swlab_parser_builder/blob/master/doc/Reference.md)

