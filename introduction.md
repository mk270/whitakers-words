---
layout: default
title: Introduction
---

[Summary](index.html) |
[Introduction](introduction.html) |
[Guiding Philosophy](philosophy.html)

Introduction
============


I am no expert in Latin, indeed my training is limited to a couple of
years in high school more than 50 years ago.  But I always felt that Latin, as
presented after two millennia, was a scientific language.  It had the
interesting property of inflection, words were constructed in a logical
manner.  I admired this feature, but could never remember the vocabulary
well enough when it came time to exercise it on tests.

I decided to automate an elementary-level Latin vocabulary list.  As a
first stage, I produced a computer program that will analyze a Latin word
and give the various possible interpretations (case, person, gender,
tense, mood, etc.), within the limitations of its dictionary.  This might
be the first step to a full parsing system, but, although just a
development tool, it is useful by itself.

<B>Please remember that this is only a computer exercise in automating a
Latin dictionary.  I am not a Latin scholar and anything in the program or
documentation is filtered by me from reading the cited Latin dictionaries.  Please
let no one go to his teacher and cite my interpretation as an authority.  </B>

While developing this initial implementation, based on different sources,
I learned (or re-learned) something that I had overlooked at the
beginning.  Latin courses, and even very large Latin dictionaries, are put
together under very strict ground rules.  Some dictionary might be based
exclusively on 'Classical' (200 BC - 200 AD) texts; it might have every
word that appears in every surviving writing of Cicero, but nothing much
before or since.  Such a dictionary will be inadequate for translating
medieval theological or scientific texts.  In another example, one
textbook might use Caesar as their main source of readings (my high school
texts did), while another might avoid Caesar and all military writings
(either for pacifist reasons, or just because the author had taught Caesar
for 30 years and had grown bored with going over the same material, year
after year).  One can imagine that the selection of words in such
different texts would differ considerably; moreover, even with the same
words, the meanings attached would be different.  This presents a problem
in the development of a dictionary for general use.

One could produce a separate dictionary for each era and application or a
universal dictionary with tags to indicate the appropriate application and
meaning for each word.  With such a tag arrangement one would not be
offered inappropriate or improbable interpretations.  The present system
has such a mechanism, but it is not fully exploited.

The Version 1.97E dictionary may be found to be of fairly general use for
the student; it has all the easy words that every text uses.  It also has the
adverbs, prepositions, and conjunctions, which are not as
sensitive to application as are the nouns and verbs.  The system also
tests a few hundred prefixes and suffixes, if the raw word cannot be
found.  Beyond that, there are a large number of TRICKS which may be applied.
These may be thought of as correcting for variations in spelling.
This allows an interpretation of many words which would otherwise
be marked unknown.  The result of this analysis is fairly straightforward
in most cases, accurate but esoteric in some others.  Some constructions
are recognized Latin words, and some are perfectly reasonable words which
may never have been used by Cicero or Caesar but might have been used by
Augustine or a monk of Jarrow.  For about 1 in 10 constructed words the
result has no relation to the normal dictionary meaning.

BE WARNED!  The program will go to great lengths if all tricks are
invoked.  If you get a word formed with an enclitic, prefix, suffix, and
syncope, be very suspicious!  It my well be right, but look carefully.
(Try siquempiamque!)

The final try is to look at the input as two words run together.  In
most cases this works out, and is especially useful for late Latin number
usage.  However, this algorithm may go very wrong.  If it is not obviously
right, it is probably incorrect.

With this facility, and a 39000 word dictionary, trials on some tested
classical texts and the Vulgate Bible give hit rates of far better than
99%, excluding proper names (there are very few proper names in this
dictionary).  (I am an old soldier so the dictionary may have
every possible word for attack or destroy.  The system is near perfect for
Caesar.) The question arises, what hit rate can be expected for a general
dictionary.  Classical Latin dictionaries have no references to the
terminology of Christian theology.  The legal documents and deeds of the
Middle Ages are a challenge of jargon and abbreviations.  These areas
require special knowledge and vocabulary, but even there the ability to
handle the non-specialized words is a large part of the effort.

The development system allows the inclusion of specialized vocabulary (for
instance a SPEcial dictionary for specialized words not wanted in most
dictionaries), and the opportunity for the user to add additional words to
a DICT.LOC.

It was initially expected that there would be special dictionaries for
special applications.  That is why there is the possibility of a SPECIAL
dictionary.  Now the general dictionary is coded by AGE and application
AREA.  Thus special words used initially/only by St Thomas Aquinas would
be Medieval (AGE code F) and Ecclesiastical (AREA code E).  Eventually
there needs to be a filter that allows one, upon setting parameters for
Medieval and Ecclesiastical, to push those words over others.  Right now
there are not have enough non-classical vocabulary to rely on such a
scheme.  The problem is that one needs a very complete classical
dictionary before one can assure that new entries are uniquely Medieval,
that they are not just classical words that appear in a Medieval text.
And the updated is only into the D's.  So the situation is that the
mechanism is there, but not sufficient data.  Nevertheless that is exactly
the application I had in mind when I set out to do the program.

One can set a parameter to exclude medieval words if there is a classical
word answering the same parse.  Likewise, the program can ignore rare
meanings if there is a common meaning for the parse.

The program may be larger than is necessary for the present
application.  It is still in development but some effort has now been put
into optimization.  Nevertheless there is lots of room for speeding it up.
Specifically, the program is disk-oriented is order to run on small machines,
such as DOS with the 640KB limitation.  Rejecting this limitation and assuming
that the user has tens of megabytes of memory (clearly realistic today)
would allow faster processing.  The next version may go that way.


This is a free program, which means it is proper to copy it and pass it on
to your friends.  Consider it a developmental item for which there is no
charge.  However, just for form, it is Copyrighted (c).
Permission is hereby freely given for any and all use of program and data.
You can sell it as your own, but at least tell me.


This version is distributed without obligation, but the developer would
appreciate comments and suggestions.


William A Whitaker <BR>
(1936-2010)

