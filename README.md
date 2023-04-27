
## YAPB: Yet Another Parser Builder

### A programmable parser builder system
- Allows to write LALR(1) parser specifications in Haskell
- Provides an automatic syntax completion method

### Library, tools, and examples
- yapb: a library for a programmable parser builder system
- yapb-exe: a wrapper interface to YAPB
- conv-exe: a grammar format utility for conversion of a readable grammar (.lgrm) format into the Haskell data format (.grm)
- Examples: 
  - parser-exe: an arithmetic parser
  - ambiguous-exe: an arithmetic parser with the ambiguous grammar but backed by precedence attributes
  - error-exe: a demo for error recovery by an interplay between a lexer and a parser through a monadic interface
  - syncomp-exe: a syntax completion server for Emacs

### Applications using YAPB:
  - Microsoft SmallBasic parser: https://github.com/kwanghoon/sbparser
  - Polyrmorphic RPC calculus parser: https://github.com/kwanghoon/polyrpc
  - C11 parser: https://github.com/kwanghoon/c11parser
  - Haskell parser with a GHC lexer and a tweaked GHC parser libraries: https://github.com/kwanghoon/haskellparser
  - EOPL interpreters: https://github.com/kwanghoon/Lecture_EOPL_Exercise (Its solution Lecture_EOPL also available in private)

### Download, build, and test
~~~
  $ git clone https://github.com/kwanghoon/yapb
  $ cd yapb
  $ stack build
  $ stack test
~~~

### Tutorial
- As a tutorial, the most up-to-date examples are available in app/{parser,ambiguous,error,syntaxcompletion}.
- [For parser: Parser generators sharing LR automaton generators and accepting general purpose programming language-based specifications](http://swlab.jnu.ac.kr/paper/kiise202001.pdf) Written in Korean.
- [For syntax complection with YAPB-0.1.2:  A text-based syntax completion method using LR parsing (PEPM 2021)](http://swlab.jnu.ac.kr/paper/pepm2021final.pdf).
- For syntax complection with YAPB-0.2.6:  A text-based syntax completion method using LR parsing and its evaluation (SCP 2023, A journal version of PEPM 2021). Will be available soon.


### A list of benchmarks and YAPB version For the SCP paper (Science of Computer Programming 2023)
- [YAPB-0.2.6](https://github.com/kwanghoon/yapb/releases/tag/v0.2.6)
- A list of benchmarks
  * [arith](https://github.com/kwanghoon/arith/commit/2c008287902df38ea7429f61990d05a8558eefe9)
  * [smllike](https://github.com/kwanghoon/smllike/commit/ea5f3dcbbf0535110b1041b58aa12b1100c6ed13)
  * [sbparser](https://github.com/kwanghoon/sbparser/commit/7c1a714de20d613b705e440246c2b637b5abcbc3)
  * [c11parser](https://github.com/kwanghoon/c11parser/commit/daa6a4b617669583ff8fbd21e2c48b8fbf78d3a6)
  * [haskellparser](https://github.com/kwanghoon/haskellparser/commit/95af10e75f4fd45e0e33f5f885f757b8c466a90b)
- To download, git-clone the urls and then git-checkout specific commit numbers.
- Note YAPB-0.2.6 is available in Hackage


### Reference
- [References](https://github.com/kwanghoon/yapb/blob/master/doc/Reference.md)

