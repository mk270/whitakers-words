---
layout: default
title: Tests and status
---

[Development Plan](plan.html) |
[Programming](developers.html) |
[Testing](tests.html) |
[Slips](slips.html) |
[Roadmap](roadmap.html)


Tests and status
================

Test system
-----------

The code now (as of March 2016) comes with a trivial regression test, which
is to take the text of a book of the Aeneid and run it through WORDS,
comparing it with a saved copy of the output from an earlier date. This
catches quite a few bugs, and is automatically incorporated in the
[Travis continuous integration
testing](https://travis-ci.org/mk270/whitakers-words/builds) of WORDS.

In the future, there will be a suite of unit tests for WORDS, operating
via AUnit. If you know Latin and a little programming (you don't really
need to know Ada), and are willing to help out with this, please get
in contact.

The commentary below is from Whitaker's original collateral distributed with
the WORDS source code; it is largely still valid.

Coverage
--------

The program has been run against common classical texts.  Initially
this was mostly a check of the process and reliability of the program.  It
is now possible to run real texts and get valid statistics.  Relatively
few texts have been run multiple times in order to understand exactly
where failure occurred and to regression test the solutions.  Such testing
has taken place on texts totaling well over a million words.  The best
results come from those which have been run the most times.  Caesar and
the Vulgate are essentially without unknowns (excluding proper names),
Suetonius and Virgil are at the 0.1% level, Varro and Pliny have somewhat
more than 1% unknowns due to their specialized vocabulary.  While this is
a mechanical test and does not assure that the form and meaning reported
by the program is always correct, the actual number of misses found by
limited detailed examination is vanishingly small.


A far larger test (with feedback) has been made by John White in the development
of his Blitz Latin.  While not using WORDS, he has a program from much the
same basis, incorporating approximately the WORDS dictionary.  He has run
a much larger set of texts, including both classical and medieval, to the
extent of 20 million Latin words, and provided significant unknowns
to be included in WORDS.


The hardest test is against another dictionary.  While getting a 97+% hit
rate on long classical texts, a run against a large dictionary might fall
to 85-90%, the missing words being in those letters which the update has
not reached.  This is to be expected, since we both have the 10000 most
common words and have made somewhat different additions beyond that.  So
large electronic wordlists are a check on the program, and have been reserved
for that purpose, not simply incorporated as such.

We have gone so far that this is no longer significant and wordlists can be
integrated.  The only real impact has been the inclusion of modern Latin words
which come from such lists, and not from scans of texts.

English-to-Latin Tests
----------------------

So far there have been no formal validation of the English-to-Latin capability.
There have been numerous individual checks and anecdotal testing, as well
as some mechanical performance tests, but nothing fundamental.

The first test proposed is to take a small English-to-Latin dictionary,
say from the back of an introductory textbook, and check that the Latin
suggested for each entry is found in the top six returned by WORDS.
It is expected that there will be a high correspondence (to be shown).
Taking a much larger example may give a different result.
It may be that the Latin words chosen by WORDS are not the same as
the paper dictionary.

Current Status and Future Plans
-------------------------------


The present phase of refinement has incorporated the Oxford Latin
Dictionary and Lewis and Short entries into *D* (about a fourth).
Periodically, when I need a change of task, I run a major author
to check the
effectiveness of the code.  I may then include some words which turn up
frequently as unknowns, but this is done as the spirit moves me.  Smaller
sections of later authors may also be processed, giving some growth in
medieval Latin entries.  Recently I have worked the Vulgate of St. Jerome.

John White in support of his Blitz Latin program has run a very large
body of Latin text, including much medieval legal documents.  He provides
input to the dictionary as he finds significant unknowns.


