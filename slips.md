---
layout: default
title: Slips
---

[Development Plan](plan.html) |
[Programming](developers.html) |
[Testing](tests.html) |
[Slips](slips.html)

How we could "crowd"-source lexical data
========================================

WORDS can be maintained in conjunction with a web-based system for
correcting and collecting lexical data; given an appropriate vetting
system, a subset of the users of this system can be accredited as
reliable Latin scholars, and their contributions adopted as part of the
official dataset.

The web-based version of WORDS currently (August 2015) supplies a
verbatim transcript of the output of the programme. It is proposed that
this be augmented with the following facilities:

* allow users to suggest corrections
* allow users to add citations
* allow users to submit missing words
* provide web links to targeted search results for a particular word
    * on the Web
    * within our citations dataset
* suggest to a user
    * a word in respect of which a citation is required
    * a citation purportedly including a word currently lacking one
* allow a user to authenticate with a particular identity
* allow administrators to accredit known users as reliable

Data model
----------

By "lexical item" we mean the collection of forms of a word, e.g., 
the lexical item *mensa* comprises the various forms *mensam*, *mensae* etc.

By "citation" we mean the tuple of a passage of text (thirty or forty words
long) containing the lexical item, and a well-formed unique identifier for
the passage.

There shall be a many-to-many relation between lexical items and citations.

Testing
-------

It may be necessary to avoid circularity by excluding cited material from the
test cases used to develop the programme itself.