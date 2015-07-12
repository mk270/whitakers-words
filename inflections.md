---
layout: default
title: Inflections
---

Inflections
===========

Inflections for WORDS are in a human-readable file called INFLECTS.LAT.
Presently there are almost 1800 separate entries.
This data is processed to produce a file INFLECTS.SEC used by the code.
The format of INFLECTS.LAT is simple, as for example:


    N     1 1 NOM S C                 1 1 a             X A

    V     1 1 PRES  ACTIVE  IND  1 P  2 4 amus          X A

    PREP  ACC                         1 0               X A



The part of speech is given,along with the appropriate characteristics
for a particular inflection.  The inflection/ending is specified by
the stem to which it is attached, a number of characters, and the ending string.
There is an AGE and FREQ for each entry.
