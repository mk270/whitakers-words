---
layout: default
title: Plan
---

[Development Plan](plan.html) |
[Programming](developers.html) |
[Testing](tests.html)

Organisation
============

There is an effort to establish an organisation to carry on the maintenance
of WORDS, both the dictionary and the software. The original author
suggested the following:


> An eventual outcome would be to have some institution, with real Latin
> capability, provide an exhaustive and authoritative program of this
> nature.  Until then, I and other individuals will make available our
> programs.


Packaging
=========

Packaged versions based on the current source code will be made for
Windows, MacOS and Linux; the tools for this will need to ensure the
right data files are included or generated, and that the package does
not accidentally ship copyright-infringing code, a genuine possibility.

Macrons
=======

We shall re-annotate the dictionary with macrons to mark vowel length; the
sources of this information will be:

* word lists and corpora that already express this information
* annotation by experts via PyBossa
* metrical inferences from poetry corpora

Dictionary sourcing
===================

A corrections and updates mechanism will be provided. William Whitaker, the
original author, intended to include a lot more words than are presently
available:

> I will continue to refine the dictionary and the program.  The major goal
> is to complete the inclusion of OLD and L+S, and this may take years.
> Along the way, and later, I will expand to medieval Latin.  I am not so
> unrealistic as to believe that I will 'finish', indeed, this is a hobby
> and there is no advantage to finishing.

Interfaces
==========

In the future, we shall provide structured output, and a library interface,
such that users can access the grammatical information embedded in the
programme without scraping it.

Code cleanup
============

The programme was originally maintained by a single author, its creator, who
was one of the creators of the Ada language in which it is written. From
the perspective of developers coming to it afresh, it has accumulated a
few employee-weeks' worth of technical debt over the decade and a half it
was under development.

To keep the code useful - so it can be compiled, understood, adapted and so
on, it needs to be made more readily intelligible. In general, this entails:

* shorter subprogrammes
* much less reliance on global variables
* less deeply nested subprogrammes
* subprogrammes that perform fewer tasks with fewer local variables
* a less flat structure at the source file level (>30 packages in one directory)
* less reliance on mutable stat
* less reliance on dummy values in enumerations
* documentation of the various packages and how they fit together


Martin Keegan, originally written June 2015
