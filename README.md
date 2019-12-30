# pprint-strict

A reference implementation of Daan Leijen's PPrint with Ralph Becket and David Herman's strictness modifications.

This is not intended for serious use - the `layout` function is not written in CPS so it will blow the stack on 
non-trivial input - but created as a reference to help develop other versions. 

It was used to identify a longstanding bug in project `sl-format` that was hard to identify because the pretty 
printer in that project is intended for serious use (and is written in CPS).
