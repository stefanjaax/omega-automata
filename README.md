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

