---
layout: default
title: Dictionary
---

# Dictionary

## Dictionary Codes


Several codes are associated with each dictionary entry (e.g., AGE,
AREA, GEO, FREQ, SOURCE).  Initially these were provided against the possibility of
the program using them to make a better interpretation, however
this additional information may be of some help to the reader.
It is carried in codes because it is not available to the program in any
other way.  Other codes, like the KIND code for nouns, may be
used, others may not.  The program is still in development and these are
put in to experiment with a possible capability.  Later versions may use
them, omit them, or provide others.

The program covers a combination of time periods and applications areas.
This is certainly not the way in which dictionaries are usually prepared.
Usually there is a clear limit to the time or area of coverage, and with
good reason.  A computer dictionary may have capabilities that mitigate
those reasons.  Time or area can be coded into each entry, so that one
could return only classical words, even though matching medieval entries
existed.  (The program has that capability now, but it is not yet clear
how to apply it.)

There is some measure of period and frequency that can be used to
discriminate between identical forms, but if there is only one possible
match to an input word, it will be displayed no matter its era or rarity.
The user can choose to display age and frequency warnings associated with
stems and meanings, but the present default is not to, although inflectios
are so identified by default.

So far these codes have not been of much use, especially since the only
significant exercises have been with classical Latin.  Other situations
may change this.  Perhaps the only impact now is for those words which
have different meanings in different applications or periods.  For these
the warning may be useful.  Otherwise, if there is only one interpretation
for a word, that is given.

Rare and age specific inflection forms are also displayed, but there is a
warning associated with each such.


### AGE

The designation of time period is very rough.  It is presently based on
dictionary information.  If the quotes cited are in the 4th century, and
none earlier, then the word is assumed to be late Latin, and one might
conclude that it was not current earlier.  One flaw in this argument could
be that the citation given was just the best illustration from a large
number covering a wide period.  On the other hand, the word could have
been well known in classical times but did not appear in any surviving
classical writings.  In such a case, it is reasonable to warn the reader
of Cicero that this is not likely the correct interpretation for his
example.  This capability is still developmental, and its usefulness is
still an open question.

If there is a classical citation, the word could be designated as
classical, but unless there is some reason to conclude otherwise, it is
expected that classical words are valid for use in all periods (X), are
universal for well considered (published) Latin.

A designation of Early (B) means that there are not classical citations,
except for poetry, in which the poet is invoking the past (or just
straining for meter).  Obsolete words occur similarly in English
literature and poetry.

Much which is designated late or medieval may be vulgar Latin, in common
use in classical times but not thought suitable for literary works.

In all periods the target is Latin.  Archaic Latin, for purposes of the
program, is still Latin, not Etruscan or Greek.  Medieval Latin is that
which was written by scholars as the universal Latin, not versions of
early French or Italian.

<PRE><TT>  type AGE_TYPE is (
    X,   --              --  In use throughout the ages/unknown -- the default
    A,   --  archaic     --  Very early forms, obsolete by classical times
    B,   --  early       --  Early Latin, pre-classical, used for effect/poetry
    C,   --  classical   --  Limited to classical (~150 BC - 200 AD)
    D,   --  late        --  Late, post-classical (3rd-5th centuries)
    E,   --  later       --  Latin not in use in Classical times (6-10) Christian
    F,   --  medieval    --  Medieval (11th-15th centuries)
    G,   --  scholar     --  Latin post 15th - Scholarly/Scientific   (16-18)
    H    --  modern      --  Coined recently, words for new things (19-20)
                             );</TT></PRE>
### AREA

While the reader can make his own interpretation of the area of
application from the given meaning, there may be some cases in which the
program can also use that information (which it can only get from a direct
coding).  This has not yet been used in the program, but the possibility
exists.  If the reader were doing a medical text, then higher priority
should be given to words coded B, if a farming book, then A coded words
should be given preference.

The area need not apply to all the meanings, just that there is some part
of the meaning that is specialized to or applies specifically to that area
and so is called out.

<PRE><TT>type AREA_TYPE is (
                        X,      --  All or none
                        A,      --  Agriculture, Flora, Fauna, Land, Equipment, Rural
                        B,      --  Biological, Medical, Body Parts
                        D,      --  Drama, Music, Theater, Art, Painting, Sculpture
                        E,      --  Ecclesiastic, Biblical, Religious
                        G,      --  Grammar, Retoric, Logic, Literature, Schools
                        L,      --  Legal, Government, Tax, Financial, Political, Titles
                        P,      --  Poetic
                        S,      --  Science, Philosophy, Mathematics, Units/Measures
                        T,      --  Technical, Architecture, Topography, Surveying
                        W,      --  War, Military, Naval, Ships, Armor
                        Y       --  Mythology
                             );</TT></PRE>
### GEO

This code was included to enable the program to distinguish between
different usages of a word depending on where it was used or what country
was the subject of the text.  This is a dual usage, origin or subject.

<PRE><TT>type GEO_TYPE is (
                       X,      --  All or none
                       A,      --  Africa
                       B,      --  Britian
                       C,      --  China
                       D,      --  Scandinavia
                       E,      --  Egypt
                       F,      --  France, Gaul
                       G,      --  Germany
                       H,      --  Greece
                       I,      --  Italy, Rome
                       J,      --  India
                       K,      --  Balkans
                       N,      --  Netherlands
                       P,      --  Persia
                       Q,      --  Near East
                       R,      --  Russia
                       S,      --  Spain, Iberia
                       U       --  Eastern Europe
                       );
</TT></PRE>

### FREQ

There is an indication of relative frequency for each entry.  These codes
also apply to inflections, with somewhat different meaning.  If there were
several matches to an input word, this key may be used to sort the output,
or to exclude rare interpretations.  The first problem is to provide the
score.  The initial method is to grade each word by how much column space
is allocated to it in the Oxford Latin Dictionary, or the number of
citations, on the assumption that many citations mean a word is common.
This is not the main intent of the compilers of existing dictionaries, but it
is almost the only indication of frequency that can be inferred from the
dictionaries.  In many cases it seems to be a reasonable guess, certainly
for those most common words, and for those that are very rare.

FREQ guessed from the relative number of citations given by sources
need not be valid, but seems to work.
If the compiler's purpose were just to give sufficient
examples to clarify the use of the word,
perhaps a single reference would serve for a simple word.
However one might observe that dictionary people seem to be enamored
with filling up this section whenever possible.
('et' has more than a page in OLD.)
If there is only one citation, they could only find one.
(This assertion can now easily be verified by searching the texts
available on the Internet.)


With the
understanding that adjustments can be made when additional information is
available, the initial numeric criteria are:

<PRE>
A   full column or more, more than 50 citations - very frequent
B   half column, more than 20 citations - frequent
C   more then 5 citations - common
D   4-5 citations - lesser
E   2-3 citations - uncommon
F   only 1 citation - very rare
</PRE>



In the case of late Latin in Lewis and Short, these frequencies may be
significant underestimates, since the volume of applicable texts
considered seems to be much smaller than for classical Latin resulting in
fewer opportunities for citations.  Nevertheless, barring additional
information, the system is generally followed.

For the situation where there are several slightly different spellings
given for a word, they all are given the same initial frequency.  The
theory is that the spelling is author's choice while the frequency is
attached to the word no matter how it is spelled.  I presume that for a
specific text the author always spells the word the same way, that there
is no distribution of spellings within a individual text.  One exception
to this rule is the case where a variant spelling is cited only for
inscriptions.  There may be some significance to this and a FREQ of I is
assigned.  The logic of this choice is debatable.  However, for some
variations there is clearly a difference in application and this can be
reflected in the frequency code.  Likewise, there are situations wherein
words of the same spelling but different meanings may have different
frequencies.  This may help to select the most likely interpretation.

One has a check against the frequency list of Diederich for the most
common, and those are probably the only ones that matter.  But the
frequency depends on the application, and it should be possible to run a
new set of frequencies if one had a reasonable volume of applicable text.
The mechanical verification of word frequency codes is a long-term goal of
the development, but must wait until the dictionary data is complete.

Inscription and Graffiti are designations of frequency only in that the
only citations found were of that nature.  One might suppose that if
literary examples were known they would have been used.  So one might
expect that such words would not be found in a student's text.  There is
no implication that they were not common in the spoken language.

A very special case has been created for 'N' words, words for which the
only dictionary citation is Pliny's Natural History.  It seems, from
reading of dictionaries, that this work may be the only source for these
words, that they do not appear in any other surviving texts.  They are
usually names for animals, plants or stones, many without identification.
Such words may appear only in Lewis and Short and the Oxford Latin
Dictionary, the unabridged Latin classical dictionaries.  These words are
omitted from most other Latin dictionaries and, although they fall in the
classical period and are from a very well known writer, there is no
mention of the omission.  So there may be an argument to disparage these
words, unless one is reading Pliny.

Most of these words are of Greek origin (although that is also true for
much of Latin).  For many, the dictionaries report different forms or
declensions for the word giving the same citation.  Often one dictionary
will give a Greek-like form (-os, -on) where another gives a Latinized
form (-us).  There is no consistency.  Both OLD and L+S disagree on Latin
and Greek forms, with no overwhelming favoritism to one form attached to
either dictionary.  This may be a reflection of the fact that the
dictionaries grew over a long time with several editors, many workers, and
no rigid enforcement of standards.

I have made it a point to try to complete (give M, F, N) Greek adjectives
where other dictionaries give only a single form.  To do this I have referred
to the base Greek in Liddell + Scott Greek-English Lexicon, assuming that
any Roman scholar pedantic enough to use a Greek form knew the Greek and
would draw on that knowledge.

There is another problem that is found chiefly in connection with
Pliny-type words.  Since the literature is very sparse on examples, it is
often uncertain whether a particular usage is appropriately listed as a
noun, as an adjective, or as adjective used as a substantive.  The present
dictionary, in blessed innocence, records all forms without bias.

<PRE><TT>    type FREQUENCY_TYPE is (     --  For dictionary entries
    X,    --              --  Unknown or unspecified
    A,    --  very freq   --  Very frequent, in all Elementry Latin books, top 1000+ words
    B,    --  frequent    --  Frequent, next 2000+ words
    C,    --  common      --  For Dictionary, in top 10,000 words
    D,    --  lesser      --  For Dictionary, in top 20,000 words
    E,    --  uncommon    --  2 or 3 citations
    F,    --  very rare   --  Having only single citation in OLD or L+S
    I,    --  inscription --  Only citation is inscription
    M,    --  graffiti    --  Presently not much used
    N     --  Pliny       --  Things that appear only in Pliny Natural History
                      );</TT></PRE>


For inflections, the same type is used with different weights

<PRE><TT>
--  X,    --              --  Unknown or unspecified
--  A,    --  most freq   --  Very frequent, the most common
--  B,    --  sometimes   --  sometimes, a not unusual VARIANT
--  C,    --  uncommon    --  occasionally seen
--  D,    --  infrequent  --  recognizable variant, but unlikely
--  E,    --  rare        --  for a few cases, very unlikely
--  F,    --  very rare   --  singular examples,
--  I,    --              --  Presently not used
--  M,    --              --  Presently not used
--  N     --              --  Presently not used

</TT></PRE>

### SOURCE

Source is the dictionary or grammar which is the source of the
information, not the Cicero or Caesar text in which it is found.


For a number of entries, X is now given as Source.  This is primarily from
the vocabulary (about 13000 words) which was in place before the Source
parameter was put in, and some have not been updated.  They are
from no particular Source, just general vocabulary picked up in various
texts and readings.  Although, during the dictionary update beginning in
1998, all entries are being checked against sources, it may be improper to
credit (blame?) a Source when that was not the origin of the entry,
remembering that the actual entries are of my generation entirely and may
not correspond exactly to any other view.  However, in the second pass (as
far as it has progressed) all classical entries have been verified with
the Oxford Latin Dictionary (OLD).  (By that I mean that I have checked,
not to imply that I have not made errors.) This does not mean that the
entry copies or agrees with the OLD, but that I read the OLD entry with
great respect and put down what I did anyway.  Newer entries, added in
this process, and those checked later in the process, if found in the OLD,
have the O code.  Words added from Lewis and Short, but not in OLD, have
the S code, etc.

All entries for which there is a Source will be found in
some form in that Source, but the details of the interpretation of
declension and meaning is mine.
Each entry is
my responsibility alone, and there are significant differences and
elaborations.  They may not necessarily be found as
primary entries, or even directly referrenced, but they will have been
constructed from information in that source.  For instance, the remark 'adp see app'
in a source dictionary may generate 'adp' WORDS entries that are not explicitlt mentioned in the
source dictionary.
There might be occasions where the source gives a noun
but on my own initiative I have also introduced the corresponding adjective
(or the converse), particularly if that usage was found in a text.
In such a case the source would be the same.  If I have
done a proper job, the reader will not often be surprised.

An important implication of the SOURCE is age.  OLD contains words
from the classical period of Latin, and these are carried forward to all ages.
Thus AGE for OLD entries will be X (all ages).  Those in L+S, but not in OLD,
might be checked against the premise that they were late/post-classical Latin (D),
citations being the determining factor.  Souter (SOURCE=P) is a wordlist of
later Latin (AGE=E), so his entries might be presumed not to be common in
classical times.  Other sources, indicated by AGE flags or by parenthesized
comments, may also indicate to the user the age appropriate for the entry.
Calepinus Novus (Cal) (SOURCE=K) is especially noteworthy  in that it is
of modern, 20th century Latin and its meanings should probably not be applied to
earlier texts.


OLD is taken as the most authorative source
and if it is in OLD then it was used in classical times within a very limited period.
An entry with source O will have AGE X (or C if it is unique to classical).
This also define good Latin and the usage should be valid for all ages.


Lewis and Short (S) is next in authority and also somewhat in time.
It covers, in addition to classical, a later period.  That a word appears in S but not in O
may mean it is a somewhat later usage.
If that poiint is well established, the AGE is D.
But most often the main source is OLD and there are additional meanings
indicated as L+S.  The user is warned that this may be a case of modified meaning
coming into use at a later age.
But it may be that, after review of L+S, OLD differs for reason and has a better interpretation.


A formation from a classical word with the natural meaning is usually assumed
to also be appropriately classical/general - X.
Such a word with an enhanced, specialized or modified meaning might
indicate a later usage, and is so labeled.
It may be that the word was in use earlier but no reference is available,
In some cases, an additional meaning is identified as (L+S)
just to give credit, without implying anything further.


In time, Souter (P) is next.
Again if it is in Souter but not O of S it is very likely later Latin
The date may reflect this, but the source is a hint to the user, not an firm promise.


Next in line in time is Latham (M) for medieval Latin.


Souter and Latham are poorly represented.  There is no attempt to include these sources
with the throughness of the OLD and L+S effort.
Entries from these sources come up only when a particular
word is submitted from a text and no other source serves, giving credance to the assumption that
such entries belong to a later AGE..


Stelten (Ecc) is more fully represented (goal to complete) since it specializes in an area not well
covered by other sources.  While it is a complete dictionary, with all the general words, it has
a number of entries specifically or solely applicable to the Christian Church.
These are from later (non-classical) times, chiefly medieval.


Licoppe (K) is modern.  An additional meaning on a word from an earlier AGE is likely to be uniquely modern.


Note that there are examples in which different sources at different ages give contrary meanings.
This may reflect a real and not uncommon shift in meaning, or there may be errors in the sources.
At least in such cases the sources (and their implied ages) are identified.

The list of sources goes far beyond what has been directly used so far.
There should be no expectation at this point in the development that
all these sources have even been used.  They are listed as I have copies
and as they might be consulted.  They are encoded so that the program might
recognize and process the source should it come up.
I have sought and received permission for those which have been
extensively used.  Others have only been used for an occasional check
(fair use).



<PRE><TT>  type SOURCE_TYPE is (
    X,      --  General or unknown or too common to say
    A,
    B,      --  C.H.Beeson, A Primer of Medieval Latin, 1925 (Bee)
    C,      --  Charles Beard, Cassell's Latin Dictionary 1892 (CAS)
    D,      --  J.N.Adams, Latin Sexual Vocabulary, 1982 (Sex)
    E,      --  L.F.Stelten, Dictionary of Eccles. Latin, 1995 (Ecc)
    F,      --  Roy J. Deferrari, Dictionary of St. Thomas Aquinas, 1960 (DeF)
    G,      --  Gildersleeve + Lodge, Latin Grammar 1895 (G+L)
    H,      --  Collatinus Dictionary by Yves Ouvrard
    I,      --  Leverett, F.P., Lexicon of the Latin Language, Boston 1845
    J,
    K,      --  Calepinus Novus, modern Latin, by Guy Licoppe (Cal)
    L,      --  Lewis, C.S., Elementary Latin Dictionary 1891
    M,      --  Latham, Revised Medieval Word List, 1980
    N,      --  Lynn Nelson, Wordlist
    O,      --  Oxford Latin Dictionary, 1982 (OLD)
    P,      --  Souter, A Glossary of Later Latin to 600 A.D., Oxford 1949
    Q,      --  Other, cited or unspecified dictionaries
    R,      --  Plater & White, A Grammar of the Vulgate, Oxford 1926
    S,      --  Lewis and Short, A Latin Dictionary, 1879 (L+S)
    T,      --  Found in a translation  --  no dictionary reference
    U,      --  Du Cange
    V,      --  Vademecum in opus Saxonis - Franz Blatt (Saxo)
    W,      --  My personal guess
    Y,      --  Temp special code
    Z       --  Sent by user --  no dictionary reference
            --  Mostly John White of Blitz Latin

    --  Consulted but used only indirectly
    --  Liddell + Scott Greek-English Lexicon

    --  Consulted but used only occasionally, seperately referenced
    --  Allen + Greenough, New Latin Grammar, 1888 (A+G)
    --  Harrington/Pucci/Elliott, Medieval Latin 2nd Ed 1997 (Harr)
    --  C.C./C.L. Scanlon Latin Grammar/Second Latin, TAN 1976 (SCANLON)
    --  W. M. Lindsay, Short Historical Latin Grammar, 1895 (Lindsay)
                        );
</TT></PRE>


## Current Distribution of DICTLINE Flags

<PRE><TT>
Number of lines in DICTLINE GENERAL  1.97F    39187

AGE
X         28858
A         61
B         446
C         58
D         3937
E         1718
F         1996
G         1920
H         193

AREA
X         29181
A         2955
B         912
D         410
E         1916
G         504
L         1221
P         181
S         730
T         382
W         722
Y         73

GEO
X         38147
A         64
B         52
C         1
D         3
E         49
F         67
G         20
H         278
I         141
J         4
K         6
N         8
P         9
Q         312
R         1
S         25
U         0

FREQ
X         11
A         2133
B         2711
C         10757
D         2678
E         11218
F         7982
I         424
M         0
N         1273

SOURCE
X         7554
A         0
B         41
C         1751
D         14
E         1417
F         119
G         59
H         0
I         4
J         116
K         2100
L         60
M         759
N         84
O         16039
P         296
Q         24
R         12
S         8094
T         88
U         0
V         47
W         316
Y         35
Z         158
</TT></PRE>



## Dictionary Conventions

There are a few special conventions in setting codes.

### Proper Names

Proper names are often identified by the AGE in which the person lived,
not the age of the text in which he is referenced, the AREA of his fame or
occupation, and the GEO from which he hailed.  This refers to some
most-likely person of this name.  A name may be shared by others in
different ages.  Thus Jason, the Argonaut, is Archaic, Myth, Greek (A Y
H).  (It is not likely that a Latin text would refer to a TV star.)
Tertullian, an early 3rd century Church Father from Carthage, author of
the first Christian writings in Latin, is Late, Ecclesiastic, Africa (D E
A).  Jupiter is (A E I), which is a bit sloppy since he is present later.
Today he may be a myth, but then he was a god.  But even gods are not
eternal (X) in language, and an initial place is found for them.  Place
names are likewise coded, although with less confidence.

### Vertical Bar

While not visible to the user, the dictionary contains certain meanings
starting with a vertical bar (|).  This is a code used to identify meanigs
that run beyond the conventional 80 characters.  One or more vertical bars
leading the meaning allows tools to recognize that they are additional
meanings to an entry already encountered, usually the entry immediately
before when the sort is for that reason.  This is only of concern to those
dealing with the raw dictionary who have asked.  <BR>

## Evolution of the Dictionary

The stem list was originally put together from what might be called
'common knowledge', those words that most Latin texts have.  The first
version had about 5000 dictionary entries, giving up to 95% coverage of
simple classical texts.  This grew to about 13000 entries with specific
additions when gaps were found.  With this number it was possible to get
better than a 99% hit rate on Caesar (an area from which the dictionary
was built).  Parse of other works fell to 95-97%, which may be
mathematically attractive but leaves a lot to be desired in a dictionary,
since a translator is usually familiar with the vast bulk of the language
and just needs help on the obscure words.  Having just the common words is
not enough, indeed not much help at all.  So an attempt is made to make
the dictionary as complete as possible.  All possible spellings found in
dictionaries are included.

Starting with the 13000, the expansion project beginning in 1998 sought to
verify the existing words and supplement with any new found ones.  Thus
all classical Latin words are consistent with the OLD (not to say taken
from, because most were not, but checked against).  Any significant
deviation is indicated, either as from another source, or in the
definition itself.

L+S is used for later Latin and to check OLE work.  This started with the
thought that if a word was in L+S but not in OLE it must be later Latin,
beyond the range of OLD.  I was surprised at how many words with classical
citations were in L+S but not in OLD, and how many are of different
spelling.

The refinement is proceeding one letter at a time, as is the tradition for
all great dictionaries.  First stage refinement has proceeded through DI.
<BR>
<BR>

## Text Dictionary - DICTPAGE.TXT


In response to many requests, a simple ASCII text list has been created of
the WORDS dictionary, in what might be called the paper dictionary form.
Each coded dictionary entry has been expanded to its dictionary form
(nominitive and genitive for nouns, four principle parts for verbs, etc.).
In content it is like a paper dictionary, but each entry is on one long line
and the headwords are in all capitals, convenient for case-sensitive search.
The headwords are listed alphabetically (not the same as the coded file)
and offered in an ASCII/DOS text file
<A HREF="http://www/erols/com/whitaker/dictpage.txt"><B>DICTPAGE.TXT</B></A>
which may be searched from the user's browser,
or best downloaded and searched by any editor off-line.
To make it possible to search on-line, the file is not compressed and so is
about 3 MB.
<BR><BR>

## Latin Spellchecking - Text Processor List - LISTALL.ZIP


I have done a lot of Latin spell checking directly with WORDS.
All you have to do is put the text in a file,
run WORDS with a text file (@) input,
and require output of an WORD.UNK file (see # parameters).
It is sometimes useful to run without FIXes and TRICKS first,
then run the resulting first-pass UNKNOWNs
and look at the full WORD.OUT to make sure the modifications are reasonable.


There are other techniques.
As I understand it, WORD2000 and other processors take a simple list of valid spellings
and use that for spellchecking.
I am speaking on secondhand information.
I have not tried to do the WORD2000 job.
However several people have proposed to use my dictionary files to do so.

Since Latin is an inflected language, each dictionary entry expands
to many "words", often hundreds.
The present WORDS raw dictionary would expand to an enormous number of simple words,
but that is not the end of it.
Each of those words might have attached prefixes and suffixes, enclitics, and spelling variations.
Literaly billions of different words can be parsed and analyzed by the WORDS program.
These are legal Latin words, whether any Roman actually spoke them.
Of course, one could make a list of all the words in Cicero, or in the Vulgate,
and make a dictionary of those (and we are close to that),
but the body of medieval Latin is enormously greater
than that of classical Latin on which most dictionaries are based.


In response to several requests, a simple ASCII text list has been created of
the two million primary words
that the WORDS program  and dictionary can form by adding inflections to stems.
This list has been reduced to half by eliminating duplicates.
The downloadable
<A HREF="http://www/erols/com/whitaker/listall.zip"><B>ZIP</B></A>
of this file is over 2 MB.

The purpose of such a list is to provide data for conventional
word processor spell checking.

Currently there are some omissions.

1. Latin has a widely used enclitic, -que, also -ne and -ve.
In principle these could be tacked on to almost any word.
If the spell checking system had the capability of recognizing
them, that would be the most convenient way of handling this problem.
Otherwise, completeness would require their addition to every word,
quadrupling the size of the list.

2. Many Latin verb forms are subject to syncope, contracting the
form for pronounciation.  In WORDS this is handled by a process.
For the list another method must be used and the contracted words
generated by modifing both stem and ending.

3. There are some common combined words in Latin in which the first
part of the word is declined, followed by a fixed form.  Unlike the
enclitic situation, these forms are limited and should be generated
seperately (quidam).  Other qu- pronouns are handled seperately
in WORDS and need special processing here also.

4. Uniques have not yet been added.  This is a trivial
matter.

5. There is the problem of prefixes and suffixes.  WORDS provides for
hundreds of these.  It would be impractical to multiply the list
by mechanically including all such possibilities.  Fortunately,
this may not be a significant problem.  The philosophy for the
dictionary has been to include all words, even those which could
be easily generated by a base and fixes, as they occur or are found
in sources.  This means that the most common compound words are
in the system, but that coverage is mostly concentrates on classical Latin.

6. In later times especially,
there came some more or less common spelling variations.
These are handled in WORDS by TRICKS.
They can be relatively expensive, but are only applied to words
which otherwise have failed, are these are becoming rarer.
This process, if generally applied, would not only expand the
list enormously, the added words would not advance the goal
of spell checking.  They are, in some sense, misspelled words.
For a reader, it can be useful to have a guess at the word.
He can examine the form and context and judge whether it makes
sense.  It is not a process to be applied mechanically.

7. There is a divergence in the way editors treat the non-Latin
characters J and V.  These are the consonant forms of I and U.
They are explicit in English, so for convenience, familiarity, and
pronounciation general practice in the past has been to use them.
More recently, some academic purists have rejected this and eliminated
J and V altogether.  (Note that the same purists use lower case
letters, in spite of the fact that the Romans had only the upper case.)
WORDS keeps the variant characters in the dictionary and maps them
to a single character in processing.  A list could include both
expressions, and it would only add a few percent in size.  However,
that would allow inconsistent spelling choices in a text.  This
seems to be contrary to the goals of a spell checker.
It is probably better eventually to offer two seperate lists so that the user
may select the option appropriate for his work.



All the above factors are applied by processes in the WORDS program.
Running WORDS looking for UNKNOWNS will give a superior spell check,
but the list can be useful in conjunction with common editors.
Experience will determine its effectiveness.
<BR><BR>
