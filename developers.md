---
layout: default
title: Developers and Rehosting
---

[Development Plan](plan.html) |
[Programming](developers.html) |
[Testing](tests.html)

Development and Rehosting
=========================


Program source code and data
----------------------------


The program is written in Ada, and is machine independent.  Ada source
code is available for compiling onto other machines.

* Source code URL: [https://github.com/mk270/whitakers-words](https://github.com/mk270/whitakers-words)
* Issue tracker URL: [https://github.com/mk270/whitakers-words/issues](https://github.com/mk270/whitakers-words/issues)


Licence
-------

<B>All parts of the WORDS system, source code and data files, are made freely
available to anyone who wishes to use them, for whatever purpose.</B>

Rehosting WORDS
---------------

There is an [archive](https://github.com/mk270/whitakers-words/) 
of all the Ada source files to port WORDS, and
support programs and data to generate the necessary dictionaries and
inflections for re-hosting the WORDS Latin dictionary
parsing/translation system on any machine with an Ada 95 compiler.  (It
can be made to work with Ada 83 also by replacing just tha short driver routine.)


This a console program (keyboard entry), without fancy Windows GUI, and has
thereby been made system independent.

There are four supporting programs

  * makedict.adb
  * makestem.adb
  * makeinfl.adb
  * makeefil.adb

and DOS ASCII data files for them to act upon to produce WORDS data files

  * DICTLINE.GEN
  * STEMLIST.GEN
  * EWDSLIST.GEN
  * INFLECTS.LAT

and other WORDS DOS ASCII supporting files

  * ADDONS.LAT
  * UNIQUES.LAT

The wordy file names are for
compliance with the restrictions of the GNAT system.

The system produces executables for WORDS, MAKEDICT, MAKESTEM,
MAKEEFIL, and MAKEINFL.  Executing the latter four against the input
respectively of

  * DICTLINE.GEN
  * STEMLIST.GEN
  * EWDSLIST.GEN
  * INFLECTS.LAT

(when they ask for DICTIONARY say G) producing

  * DICTFILE.GEN
  * STEMFILE.GEN
  * INDXFILE.GEN
  * EWDSFILE.GEN
  * INFLECTS.SEC

Along with ADDONS.LAT and UNIQUES.LAT, this is the set of data for WORDS.

The only problem that has appeared on porting so far is that one must be
careful of file names.  Problems sometimes turn up but have been easily
rectified by inspection.  All of my systems are case-independent on file
names.  If one is running in a case-dependent system (UNIX), this is a
point to check.  Note that the data files are capitalized, source files
are not.


The source is in Ada and therefore very readable, which is not claimed for the
logic which is my, not Ada's, fault.  The source and data are freely available
for anyone to use for any purpose.  It may be converted to other
languages, used in pieces, or modified in any way without further permission
or notification.

There is one oddity that the reader may remark upon.  The code is loaded
with PUT/print statements which are now commented out.   These were used at
some time for debug purposes and were just left in.  They (mostly) are left
justified and may fairly easily be removed for a cleaner presentation.
Further there are many blocks of code which during development have been
moved or removed, but have in their previous place been left commented.
This is also messy.  I cannot really justify not having fixed this, but there it is.

Feedback
--------

Feedback is invited.  If there is a problem in installing or operating, in
the results or their display, or if your favorite word is omitted from the
dictionary, please let us know.

All comments are appreciated.  Check back for new version releases.
