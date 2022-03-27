
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
  - syncomp-exe: a syntax completion server for Emacs

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




### Reference
- [References](https://github.com/kwanghoon/yapb/blob/master/doc/Reference.md)

