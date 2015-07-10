---
layout: default
title: Guiding philosophy
---

# Guiding philosophy

## Purpose

The dictionary is intended as a help to someone who knows roughly enough
Latin for the document under study.  It gives the accidence and meanings
possible for an input Latin word.  It is for someone reading Latin text.


This is a translation dictionary.  Mostly it provides individual words in
English that correspond to, and might be used in a translation of, words
in Latin text.  The program assumes a fair command of English.  This is in
contrast to a conventional same-language desktop dictionary which would
explain the meanings of words in the same language.  The distinction may
be obvious but it is important.  A Latin dictionary in medieval times
would have explanations in Latin of Latin words.

There are various approaches to the preparation of a dictionary.  The most
scholarly might be to select only proper and correct entries, only correct
derivations, grammar, and spelling.  This would be a dictionary for one
who wished to write 'correct' Latin.  (Correct being defined as the way
Cicero, or your favorite writer or grammarian, used it.) The current
project has a different goal.  This program is successful if a word found in
text is given an appropriate meaning, whether or not that word is spelled
in the generally approved way, or is 'good Latin'.  Thus the program
includes various words and forms that may have been rejected by recent
scholars, but still appear in some texts.  Philosophically, thus program
deals with Latin as it was, not as it should have been.  I make no
corrections to Cicero, which some might have been tempted to do if
producing an academic dictionary instead of a program.  Moreover I make no
corrections of St Jerome.  If your copy of the Vulgate has a particular
spelling, that may be recognized by the program, either through a TRICK or
as a dictionary entry that I have generated.

A philosophical difference from many dictionary projects is that this one
has no firm model of the user or application.  It is not limited to
classical Latin, or to 'good practice', or to common words, or to words
appearing in certain texts.  As a result there will be a lot of chaff in
the output.  Some of this may be trimmed out automatically if desired, but
it is there and available.

However inadequately, I hope to document decisions that went into the
arrangement of the program and dictionary.  I am surprised that there is
little or no such information to the user of published dictionaries.  If
others generate similar products, or use the data from this one, they can
do so in knowledge of how and why processes and forms were constructed.

I make few value judgments and those are mechanical, not scholarly, and
are documented herein.  Nevertheless some may be inappropriate, in spite of
good intentions.


## Method

The program subtracts possible endings from an input words and searches a
list of stems, trying to make a match.  If no exact match is possible, it
tries various modifications, beginning with prefixes and suffixes, and
eventually involving various regular spelling variations (or 'tricks')
common in classical and medieval Latin.

A choice was made that the base was classical Latin as defined by the
Oxford Latin Dictionary (OLD).  Their primary time period is
arbitrary/roughly 100 BC to 100 AD.

The classical form of words is taken as the base.  Modifications are in
such a way to correct to this base.  Further additions to local
dictionaries should keep this in mind.  Modifications are made to the
input words, not to the dictionary stems.  It could be done the other way,
but the present situation was initially much easier.  There are some
consequences of this approach.  For instance, it is easy to remove an 'h'
from an input word to match with a stem.  It is much more difficult (but
not impossible) to add 'h' in all possible positions to check against
stems.

It would be possible to match most words with a relatively smaller list of
stems (or roots) and generous application of word construction.  This
approach is not followed.  One difficulty is that while words may be
constructed correctly, and the underlying meaning to be found from this
construction, the common usage may be obscured by a formal interpretation
of the parts.  In practice this occurs in 20-40% of the cases.  This
method is still very useful in approaching a word for which there has been
no dictionary interpretation, but it puts a considerable burden on the
normal user.  Further, in about 10% of constructions, the result is just
wrong.

In normal usage, if the program finds a simple match, it does not go
further and consider what constructed words might also be valid.  (One can
override and force prefix/suffix construction with a switch, but one might
not want to force all possible tricks.)

For instance, if there is an adjective that matches, a corresponding
identically spelled, logically valid noun will not be reported unless it
is explicitly found in the dictionary, even though it could be constructed
or inferred from the adjective or constructed with a suffix from a verb in
the dictionary.

An exception to this is that enclitics (eg., -que) are always considered.
Coloque can be a verb or collo-que.  The latter is in Virgil and should
not be omitted.  Verb syncope is also favored.  In the vast majority of
cases, if there is a possible syncope it is the correct parse.  This is
given preference over word construction with suffix.  Audii is syncope of
audivi, but it could also be aud-i-i.  The latter is considered very
unlikely.

There are a large number of paths and possibilities.  Choices have been
made in the code that result in the exclusion of some.  It is hoped that
they were the best choices.  The method was constructed by taking a number
of primary procedures and combining/assembling them in such a way as to
give reasonable parses for a number of test cases.  Basicly, this is
hacking, but it might be considered and emperical starting point from
which one could construct a logical rationale.

Therefore, the philosophy is to populate the stem list as densely as
possible.  Even easily resolved differences are included redundantly
(adligo as well as alligo - ad- is most of duplicates).  The advantage is
that while regular single-letter modifications are fairly easy, and two
letter differences are possible (but more expensive), further deviations
are problematical.  The better populated the stem list, the better the
chance of a result.

Even in easy cases the overpopulation is helpful.  Antebasis is easily
parsed as ante-basis ('pedestal before', which is reasonable), but
inclusion as a separate word allows the additional information that it is
the hindmost pillar of the pedestal of a ballista.

The stem list is also populated with variants suggested by different
sources.  The problem is that the remains of classical Latin have gone
through many monks along the way.  These copyists may have made simple
mistakes (typos!), or have made what they thought were proper corrections
(spell checkers!).  And twenty centuries later scholars work hard to
reassemble the best Latin to present in the dictionary.  But a particular
document in the form presented to the reader may have have a variety of
spellings for exactly the same word in the same referenced passage
(Pliny's Natural History is often subject to this problem).  (It may even
be that modern texts and dictionaries have misprints!) All forms found in
various dictionaries can be included, with the exception of those
explicitly labeled 'misread' (and the argument probably could mandate
their inclusion also).  However, a single example of a variant in one case
will not be included as a dictionary entry.  If such a word is
sufficiently important, if it is used frequently or by several authors, it
will be entered as a UNIQUE.

Lewis and Short seem to be more willing than the more recent Oxford Latin
Dictionary to raise a few examples of variation to an entry (at least an
alternate).  Generally, I make an entry if some dictionary does so.  But
within an entry I generate additional possible stems not noted elsewhere,
e.g., I expand first declension verbs with '-av' perfect stems, even
though no example exists in classical Latin.  This is often the practice
in other dictionaries also.


Verb parts omitted from source dictionaries are mechanically added where it is clear,
(ex. where the base verb is documented, but parts are omitted in compounds).
Whether Cicero used them or not, some later text might.


In some cases I also have expanded adjectives and adverbs to include comparative
and superlative stems where they seem reasonable or have corresponding English
instances, even when there is no specific dictionary citation.
This effort was modivated primarily by finding examples of such comparisons
in processing of large amounts of text beyond the classical
works upon which authoritative dictionaries are based, but even classical
works yielded examples.  The point is that, while these forms would usually be
caught by the word formation (prefix/suffix) process in the program,
the process is limited to how many operations can be done serially.
Having more/expended stems allows another level of word modification to be
implemented.

Adjectives are extrapolated to COMP and SUPER where it makes sense
(when those meanings are reasonable, and in many cases they are not)
even if the source dictionary only lists POS.
They are expanded fully especially even when the source lists a COMP but no SUPER.


Perhaps a bit out of context, consider the common question of SECLORUM in
the Great Seal of the USA.  This pure word in not in any dictionary I know of,
not the OLS or L+S.  A simple trick gives seculorum (seculum = world),
but the favored translation is from the twice modified saeculorum (saeculum = age),
which would not be found by a minimalistic system.

It is often the practice in paper dictionaries to double up on an entry
that may be either adjective or noun, usually by leading with the
adjective and mentioning its use as a noun.  A much larger set of
adjective/noun pairs is favored with separate entries.  It is the
philosophy of this program to make separate entries whenever there is an
example in any reference dictionary.  This might faciliate the task of a
larger translation program which would handle phrases or sentences.
However there has been no effort to explicitly generate such pair
expansion if there is no precedent, and the user must still recognize the
possibility of unexpanded multiple possibilities for substantives.

An argument against a large stem list is that it increases the storage
required (but this is extremely modest by current standards) and increases
processing time for search of the stems (this is far offset by the
processing which would be required to construct or analyze words working
from a smaller stem list).

A significant objection is that artifically generated stems may conflict with
real/common ones and produce false output confusing to the user.  A certain
amount of this is eliminated by trimming the output to emphisize the most
probable results, but it is still a problem.

Perhaps a counterexample would be an inferred fourth stem to no/nare (swim).
Natus conflicts with the fourth stem of nascor (be produced/born) and the
nouns and adjectives stemming from it.  The nare natus does not appear in
dictionaries, nor does it occur in compounds of nare, so it has been omitted
from the WORDS dictionary.

Additional parts of verbs are included (first conjugation is easily filled
out, even eccentric verbs if they are compounds of known parts), although
they may not have been found in any well known texts.  Cases can be
logically constructed that are 'missing' in classical Latin.  Verbs with
prefix can be expanded when the base is known.  That a form has not been
found in surviving copies of classical texts does not mean that it was
not on the lips of every centurion and his girl friend, or that it might
not find its way into medieval texts.

It may be argued in some cases that forms are missing because their
pronounciation would be awkward.  This may well be true when Cicero is the
arbiter, but others may not be so elegant.  Moreover, much of the texts
are represented by medieval documents, Latin the was written but may not
have been spoken, so the problem did not arise.
However, I might be willing to accept this argument for considering carefully
some perfect stems of first conjugation verbs which otherwise would end
in -avav.  In the end, the only one I found that I could not support
was lavo (wash), and its compounds, for which the perfect is lavi.

In some cases there are good reasons not to do the mathematical expansion,
and these are pointedly avoided.  There is no mechanical generation of,
for instance, conl- words for every coll- word, unless there is some
citation or reasonable rationale.  They may be paired in almost every
case, but, for instance, collis and collyra are not.  However, forms that
are mentioned in dictionaries explicitly, or implicitly by being derived
from words having variant forms, are included in order to reduce the
dependence on 'tricks'.  OLD has a conp- for almost every comp- (except
derivatives from como).  Rare exceptions seem to be rare words for which
few examples (or only one) exist.  Even in some of these cases, OLD
(mechanically?) gives two forms.  L+S follows the same pattern, except for
words of late Latin (which would not be found in OLD).  It is presumed
that the general practice in later times was always to use comp-, and the
program dictionary follows that.  There are many acc-/adc- pairs, but OLD
has a fair number of acc- words without mention of a corresponding adc-,
and so the possible generation of these words has been resisted.  If an
example turns up in text, the appropriate trick procedure should suffice

One suspects that some amount of analytical expansion is present even in
the best dictionaries.  Otherwise how can one explain four alternate
spellings for a word which apparently only appears in citation as a single
inscription.

In a some few cases I have infered a declension to certain very obscure Greek words
which other dictionaries have treated as indeclinable
(having only a single classical example of its use).
My argument is that some later writer, using this word, might attempt
to decline in it in a conventional manner,
no matter what Vitruvius thought.  I have indicated the indecl. option in the meaning.

Adjectives from participles are included if an entry is found in some
reference dictionary.  In some case the adjective has a special meaning
not obvious from the verb.  The program will return both the adjective and
the participle with its verb meaning.  The user should give some
additional consideration to the adjective meaning in this case.  If the
adjective is marked rare while the verb is common, it is likely there is
reference to a special meaning.

Tricks are expensive in processing time.  Each possible modification is
made, then the resulting word goes through the full recognition process.
If it passed, that is reported as the answer.  If it fails, another trick
is tried.  This is effective if very few words get this far.  It is
expected that application of single tricks will solve most of the
resolvable difficulties.  It would be impractical to mechanically apply
several tricks in series to a word.  A large stem population reduces the
likelyhood of multiple tricks being required.  If the dictionary is heavily and
redundantly populated, tricks are rarely necessary (and therefore not an
overall processing burden) and largely successful (if the input word is a
valid, but unusual, variant/construction).

Further, a conventional dictionary, especially one that wishes to set a
standard for proper language, excludes words that may not meet criteria of
propriety, slang, misspellings, etc.  This may place the onus on the
reader to convert words.  A computer dictionary ought to relieve the
reader as much as possible.  The present program may be a far way from
complete, but it's goal is to strive for that.

## Word Meanings

The meanings listed are generally those in the literature/dictionaries.
In the case of common words, there is general agreement among authors.
Some uncommon words display convoluted interpretations.

Generally, the meaning is given for the base word, as is usual for
dictionaries.  For the verb, it will be a present meaning, even when the
tense input is perfect.  For an adjective, the positive meaning is given,
even if a comparative or superlative form is shown.  This is also so when
a word is constructed with a suffix, thus an adverb constructed from its
adjective will show the base adjective meaning and an indication of how to
make the adverb in English.

For the level of usage for this program, and for convenience in coding,
the meaning field has been fixed at 80 characters.  It is possible to have
multiple 80 character lines for an entry, but this only necessary for the
most common words.  In order to conserve space, extraneous helpers like
'a', 'the', 'to', which sometimes appear in dictionary definitions, are
generally omitted.  The solidus ('/') is used both to separate equivalent
English meanings and to conserve space.

I have taken it upon myself to add some interpretations and synonyms, and
propose common usage for otherwise complex descriptive definitions.  The
idea is to prompt the reader, expecting that the text may not be that from
which some dictionary copied the meaning (from some 18th century
translator!).


In the meanings I only use words of which I know the meaning.
I find that in some cases the Oxford Latin Dictionary uses English
that is not in the Oxford English Dictionary.


Where available, the Linnean or 'scientific Latin' name is given in
parentheses, mostly for plants.  This is not a classical Latin name, but a
modern designation.  Similarity of this designation to some Latin word may
not be historically significant.

The spelling of the English meanings is US (plow not plough, color not
colour, and English corn is rendered as grain or wheat), in spite of the
fact that most of the Latin dictionaries that I have are British and use
British spelling.  The reason for this is (besides uniformity in the
program) that there is much computer processing and checking of the
dictionary data, including spell-checking of the English.  (This is not to
say that everything is correct, but it is much better than it would be
without the computer checking.) All my programs speak US English, so I can
count on it.  Only some are available in UK English, and I do not have all
of those versions.

Latin dictionaries seem to be locked into the 19th century.  The
English terms seem stilted, even by current British usage.  This is
probably because much work in translation was started then and later work
tended to copy from the previous dictionaries.  While this dictionary has
done some modernization, some of the previous obscurities have been
preserved.  This was done in order that certain machine processes could
compare the results of automatic translation with existing published work.


In addition, I have given US meanings to some terms that seem to be
literally translated from the Latin (or German!) (a person who
steals/drives off cattle is a rustler in the US).

Most dictionaries have an etymological approach, they are driven by the
derivation of words to distinguish with separate entries words that may be
identical in spelling but different derivations.  But they can lump
entirely different, even contradictory, meanings in a single entry if
there is some common derivation.  Philosophically, this dictionary is
usually not sensitive to derivations, but sometimes supports multiple
entries for vastly different meanings, application areas, or eras.


In a very small number of cases a source, such as OLD, will have an entry for which no English meaning is ptovifrd.
Instead, a few words of Latin text containg the word is given.
If they cannot figure it out, I certainly cannot.
Such a source entry is usually omitted ftom WORDS.


## Proper Names

Only a very few proper names are included, many just for test purposes,
others that users have requested.  The number of proper names is almost
limitless but very few are applicable to a particular document, and if it
is an obscure document it is unlikely that the names would be found in any
dictionary.

Meaning for proper names may cite a likely example of a person with that
name.  This is just an example; there are lots of others with that name.

There is a switch (defaulted to Yes) that allows the program to assume
that any capitalized unknown word is a proper name, and to ignore it.
Also, one can make up a local dictionary of names for one's particular
application.

## Letter Conventions (u/v, i/j, w)

### U and/or V

Strictly speaking, Latin did not have a V, just a consonant U, or a U
character that was easier in capitals (the way Latin was written by the
Romans) to write or chisel in stone as V. However, many modern texts and
dictionaries (with the important exception of the OLD) make the
distinction with two characters (u and v).  It appeared most appropriate
in a computer context (never destroy information) to make the distinction
and follow the common practice.  So all dictionary entries maintain the
V/v.  However, an input word following the U convention will be found.  At
an earlier version, an algorithm was kludged to convert where necessary.
While this worked in most cases, there were difficulties.  The present
system processes the dictionary and the input word as though U and V were
the same letter, although the basic dictionary maintains the distinction
and the output reflects this.  There is no need for the user to
set modes for this process.

### I and/or J

A similar situation arises with I, and its consonant form, J. In this
instance, the common practice is use only I, but there are many
counter-examples, both text and dictionaries.  (Lewis+Short uses J, but
OLD does not.) Because of common practice, the program started out as
pure-I dictionary with conversion of J-to-I on input.  It remained that
way through many versions, in spite of the logical inconsistency with U-V.
The technique worked perfectly, but eventually the aesthetic of
consistency won out and the U/V technique described above was extended to
I/J.

### W

While the letter W does not exist in classical Latin,
there are examples of W in medieval Latin.  I have not directly
faced this, and have few words in the dictionary yet with W.   The W
problem is not analogous to U/V.  While W sometimes could correspond to V
or UU, in most cases it is a valid letter, reflecting a Germanic origin of
the word.  It will be treated as a real letter, and tricks employed as useful.

