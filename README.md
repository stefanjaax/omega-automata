omega-automata
==============

A Haskell module for processing omega automata

It currently only supports state-based Büchi automata.

Library features
----------------

* Input and Output of automata in [Hanoi Omega Automata Format](https://github.com/adl/hoaf)
* Boolean operations (Intersection, Union, Complementation) on Büchi automata
* Check if a given Büchi automaton is deterministic in the limit
* Conversion of non-deterministic to limit-deterministic Büchi automaton

Programs
--------

Omega-automata comes with an example program `ldba-tool`.
To see all options supported by `ldba-tool`, enter:
```
stack build
stack exec -- ldba-tool --help
```
This will give you the following output:
```
ldba-tool - A tool for limit-deterministic Buchi automata. 

Usage:  ldba-tool [args]

args:
 -isldba [fname]                  Tell if NBA is limit-deterministic
 -2ldba [fname]                   Convert NBA to LDBA
 -complement [fname]              Complement NBA to LDBA
 -intersection fname [fname]      Intersection-LDBA of two LDBAs
 -union fname [fname]             Union-LDBA of two LDBAs

 All automata must be specified in Hanoi-Omega-Automata format.
```
