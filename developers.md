---
layout: default
title: Developers and Rehosting
---

Development and Rehosting
=========================


Program source code and data
----------------------------


The program is written in Ada, and is machine independent.  Ada source
code is available for compiling onto other machines.


Licence
-------

<B>All parts of the WORDS system, source code and data files, are made freely
available to anyone who wishes to use them, for whatever purpose.</B>

Rehosting WORDS
---------------

There is a <A HREF="wordsall.zip"><B>wordsall.zip</B></A>
zip of all the Ada source files to port WORDS, and
support programs and data to generate the necessary dictionaries and
inflections for re-hosting the WORDS Latin dictionary
parsing/translation system on any machine with an Ada 95 compiler.  (It
can be made to work with Ada 83 also by replacing just tha short driver routine.)
<P>
This a console program (keyboard entry), without fancy Windows GUI, and has
thereby been made system independent.
<P>
wordsall contains the Ada source files for WORDS, and complete details for rehosting
summarized below:
<P>
Ada source files for the WORDS system are:
<PRE>
strings_package.ads
strings_package.adb
latin_file_names.ads
latin_file_names.adb
config.ads
preface.ads
word_parameters.ads
developer_parameters.ads
preface.adb
put_stat.adb
word_parameters.adb
inflections_package.adb
inflections_package.ads
dictionary_package.ads
dictionary_package.adb
addons_package.ads
addons_package.adb
uniques_package.ads
word_support_package.ads
word_support_package.adb
english_support package.ads
english_support package.adb
word_package.ads
line_stuff.ads
line_stuff.adb
developer_parameters.adb
tricks_package.ads
word_package.adb
tricks_package.adb
list_package.ads
list_sweep.adb
dictionary_form.adb
search_english.adb
put_example_line.adb
list_package.adb
parse.adb
words.adb
</PRE>

<P>
There are four supporting programs

<PRE>
makedict.adb
makestem.adb
makeinfl.adb
makeefil.adb
</PRE>

<P>
and DOS ASCII data files for them to act upon to produce WORDS data files

<PRE>
DICTLINE.GEN
STEMLIST.GEN
EWDSLIST.GEN
INFLECTS.LAT
</PRE>

<P>
and other WORDS DOS ASCII supporting files

<PRE>
ADDONS.LAT
UNIQUES.LAT
</PRE>

<P>
<P>
The process is to download the 197fall.zip and unzip into a
subdirectory.  (If the zip form is unsuitable for your system, I can
provide the files in an uncompressed form.) The wordy file names are for
compliance with the restrictions of the GNAT system.  They may be renamed,
and I can provide an alternative.  However, the long file names demand an
UNZIP that preserves them, if GNAT is to be used.

<P>For example, in a GNAT
environment (-O3 optimizes if your system supports it):

<PRE>
gnatmake -O3 words
gnatmake makedict
gnatmake makestem
gnatmake ewdsefil
gnatmake makeinfl
</PRE>

<P>
This produces executables for WORDS, MAKEDICT, MAKESTEM, MAKEEFIL, and MAKEINFL.
Executing the latter four against the input respectively of

<PRE>
DICTLINE.GEN
STEMLIST.GEN
EWDSLIST.GEN
INFLECTS.LAT
</PRE>

<P>
(when they ask for DICTIONARY say G) producing

<PRE>
DICTFILE.GEN
STEMFILE.GEN
INDXFILE.GEN
EWDSFILE.GEN
INFLECTS.SEC
</PRE>

<P>
Along with ADDONS.LAT and UNIQUES.LAT, this is the set of data for WORDS.
<P>
The only problem that has appeared on porting so far is that one must be
careful of file names.  Problems sometimes turn up but have been easily
rectified by inspection.  All of my systems are case-independent on file
names.  If one is running in a case-dependent system (UNIX), this is a
point to check.  Note that the data files are capitalized, source files
are not.


<P>The source is in Ada and therefore very readable, which is not claimed for the
logic which is my. not Ada's, fault.  The source and data are freely available
for anyone to use for any purpose.  It may be converted to other
languages, used in pieces, or modified in any way without further permission
or notification.

<P>There is one oddity that the reader may remark upon.  The code is loaded
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
dictionary, please let me know.
<P>
All comments are appreciated.  Check back for new version releases at<BR>
<BR>
<A HREF="http://www.erols.com/whitaker/words.htm">
http://www.erols.com/whitaker/words.htm</A>
<P>
Contact e-mail <A HREF="mailto:whitaker@erols.com"> <B>whitaker@erols.com</B></A>,
<P>
or William Whitaker, PO Box 51225 Midland TX 79710 USA.  <BR>
