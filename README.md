[Project Website](http://mk270.github.io/whitakers-words/)

WORDS
=====

This is a cleaned-up version of the port of William Whitaker's WORDS
programme, a Latin-English dictionary with inflectional morphology
support; the original author passed away in 2010, so any and all help
maintaining the software as development and execution environments evolve
would be greatly appreciated.

Effectively, this is an exercise in digital preservation.

Contributing
============

Help is needed maintaining the code for future users; in particular, it
does not currently support vowel length, so it may be necessary to gather
a group of Latin experts to adjust its lexicon of several thousand words.

If you contribute, please be sure to indicate your assent to redistributing
your contributions under the same terms as the existing software; this
will minimise copyright hassles in the future.

Usage
=====

    $ make
    $ bin/words

Documentation
=============

See the HOWTO.txt file included, 
and documentation on the [Project Website](http://mk270.github.io/whitakers-words/operational.html)


Build-time Dependencies
=======================

* GPRBuild
* gnat

On a Debian-like system, you can install these roughly as follows:

    $ apt-get install gprbuild gnat

GNAT versions before 4.9 are believed to link against a buggy runtime on
64-bit platforms, so should be avoided.

Licensing
=========

WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)

Copyright William A. Whitaker (1936–2010)

This is a free program, which means it is proper to copy it and pass
it on to your friends. Consider it a developmental item for which
there is no charge. However, just for form, it is Copyrighted
(c). Permission is hereby freely given for any and all use of program
and data.

This version is distributed without obligation, but the developer
would appreciate comments and suggestions.

The source and data are freely available for anyone to use for any 
purpose. It may be converted to other languages, used in pieces, or 
modified in any way without further permission or notification. 

The program source (in Ada) and dictionary are freely available for
rehosting.

This license grant is taken verbatim from the Development/Rehosting 
information in the wordsdev.htm file accompanying the original 
author's 1.97FC release, on which the code in this repository is based.
