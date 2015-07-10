---
layout: default
title: User modifications
---

User modifications
==================

Writing DICT.LOC and UNIQUES.LAT
--------------------------------


To make the dictionary files used by the program is not difficult, but it
takes several auxiliary programs for checking and ordering which are best
handled by one center.  These are available to anyone who needs them, but
it is better that any general additions to the dictionary be handled
centrally that they can be included in the public release for everyone.

However, it is possible for a user to enhance the dictionary for special
situations.  This may be accomplished either by providing new dictionary
entries in a DICT.LOC file, those to be processed in the regular manner,
or to add a unique (single case/number/gender/...) in a text file called
UNIQUES.

DICT.LOC
--------

A dictionary entry for WORDS (in the simplest, editable form as read in a
DICT.LOC) is

<PRE><TT>
aqu   aqu
N    1 1 F T     X X X X X
water;

</TT></PRE>

For a noun there are two stems.  The definition of STEM is inherent in
the coding of inflections in the program.  Different grammars have
different definitions.  There is no formal connection with any other
usage.

To these stems are applied, as appropriate, the endings

<PRE><TT>
         S       P
NOM      a       ae
GEN      ae      arum
DAT      ae      is
ACC      am      as
ABL      a       is
</TT></PRE>

Or rather, the input word is analyzed for possible endings, and when these
are subtracted a match is sought with the dictionary stems.  A file
(INFLECTS.LAT) gives all the endings.

In this example, the first line
<PRE><TT>
aqu   aqu
</TT></PRE>
contains the two noun stems for the word found in printed dictionaries as

<PRE><TT>
aqua, -ae
</TT></PRE>

The second line

<PRE><TT>
N    1 1 F T     X X X X X
</TT></PRE>
says it is a noun (N), of the first declension, first variant, is feminine
(F), and is a thing (T), as opposed to a person, location, etc.  The X X X
X X represents coding about the age in which it is applicable, the
geographic and application area of the word, its frequency of use, and the
dictionary source of the entry.  None of this is necessary in a DICT.LOC
although something must be filled in and X X X X X is always satisfactory.

The last line is the English definition.  It can be as long as 80
characters.

<PRE><TT>
water;
</TT></PRE>


The case and exact spacing of the stems and codes is unimportant, as long
as they are separated by at least one blank.

The PART_OF_SPEECH_TYPE that you are most interested in are (X, N, ADJ,
ADV, V).  X is always a valid entry.  It stands for none, or all, or
unknown.  0 has the same function for numeric types.

The others in the type (PRON, PACK, VPAR, SUPINE, PREP, CONJ, INTERJ, NUM,
TACKON, PREFIX, SUFFIX) are either less interesting or artificial, used
only internally to the code.

A noun or a verb has a DECN_RECORD consisting of two small integers.  The
first is the declension/conjugation, and the second is a variant within
that.

N 1 1 is the conventional first declension.  But there are variants (6, 7,
8) which model Greek-line declensions.  (Greek-like variant start at 6);

N 2 1 is the regular -us, -i second declension.

N 2 2 is the regular -um, -i neuter form.

There is a N 2 3 for 'r' forms like puer, pueri.  In this case there is
the possibility of a difference in stems (ager, agri has stems coded as
ager, agr).

Again there are Greek-like variants (6, 7, 8, 9).

N 3 1 is regular third declension (lex, legis - lex, leg) for masculine
and feminine.

N 3 2 is for neuter (iter, itineris - iter, itiner).

Variants 3 and 4 are for I-stems.  And so it goes.

Each noun has a GENDER_TYPE (X, M, F, N, C).  X for unknown (something I
avoid for gender - guess if you have to) or all genders (useful in the
code but not in a dictionary), and C for common (M + F).

There is also a

<PRE><TT>
NOUN_KIND_TYPE (X,            --  unknown, nondescript
                N,            --  proper Name
                L,            --  Locale, country, city
                W,            --  a place Where
                P,            --  a Person type
                T)            --  a Thing
</TT></PRE>
which you probably do not care about either.  Most entries will all be
Thing.

Other codes are enumerated in the body of this document.

Verbs are done likewise, but there are four stems, as described below.  An
example is

<PRE><TT>
am  am  amav  amat
V 1 1 X    X X X A O
love;
</TT></PRE>


Now comes the hard part.  When starting from a dictionary one has all the
information to decide the values.  Just having a single instance of the
word lacks a lot.  Consider some examples from a user.

Elytris is surely from the Greek for sheath.  The question is how
Latinized did it get.  I suspect that by the 17th century it was
completely Latinized.  Even in classical times there was very little left
in the way of Greek forms ( elythris (or -es), elythris (N 3 3) but it
could be a Greek-like form (N 3 9).  I do not even know what case I
started with, if NOM, then it must be -is, -is, if GEN then -es, -is is
reasonable.  Then again, if it is DAT P we might have a N 1 1.

All this seems very uncertain, and, in the absence of a real dictionary
entry, it is.  However you can make the choices such that the result (the
output of the code) matches exactly what you have.  If you have more
information, lots of examples, the uncertainty shrinks.  If you have just
a single isolated example, there are limits.  (But if you do 100 and have
more information about some, you can make better guesses about the rest.)

Next we need a gender.  It may not make much difference (if M or F, or C)
in this case, but sometimes it matters.  You might be able to figure that
out from the text.

It is a thing (T), but X will work for your purposes.  For the rest, X X X
X X works fine.

So we have

<PRE><TT>
elythris   elythr
N   3 3  F T       X X X X X
elytra, wing cover of beetles
</TT></PRE>


sat, I happen to know is an abbreviated form of satis, so it is easy.  If
you want the adverb form, as you indicate:

<PRE><TT>
Sat
ADV POS     X X X X X
sufficiently, adequately; quite, well enough; fairly, (moderately)
</TT></PRE>


Adverbs have a comparison parameter (X, POS, COMP, SUPER).  Most will be
POS.

It also is an indeclinable (N 9 9) substantive:

<PRE><TT>
sat
N 9 9 N T     X X X X X
enough, sufficient; enough and some to spare; one of sufficient power

</TT></PRE>


Deplanata seems to be a 1-2 declension adjective, the -us, -a, -um form.
It also seems to derived from the verb deplanto (V 1 1) - break off/sever
(branch/shoot).

<PRE><TT>
deplanat   deplanat
ADJ 1 1 POS     X X X X X
broken off/severed (branch/shoot); (flattened)
</TT></PRE>


Adjectives have a DECN and a comparison.

The following were not at the time in the dictionary, but were in the OLD.

<PRE><TT>
alat  alat
ADJ 1 1 POS     X X X X X
winged, having wings; having a broad/expanded margin


(punct - ul - at  -> hole/prick/puncture - small - having)

punctulat   punctulat
ADJ 1 1 POS    X X X X X
punctured; having small holes/pricks/stabs/punctures

appendiculat   appendiculat
ADJ 1 1 POS    X X X X X
appendiculate; having/fringed by small appendages/bodies


acetabul   acetabul
N 2 2 N T     X X X X X
small cup (vinegar), 1/8 pint; cupped part (plant); sucker; socket, (cavity)


ruf  ruf
ADJ   1 1 POS     X X X X X
red (various); tawny; red-haired (persons); strong yellow/moderate orange


testace   testace
ADJ  1 1 POS    X X X X X
bricks; resembling bricks (esp. color); having hard covering/shell (animals)
</TT></PRE>


This one had no classical correspondence.

<PRE><TT>
brunne   brunne
ADJ 1 1  POS     X X X X X
brown
</TT></PRE>


There is one other remark.  It is probably wise to include in the
definition a more complete English meaning.  Just saying the meaning of appendiculatus is
appendiculate is not as interesting as it might be.

All the inflections are in a file called INFLECTS.LAT now a part of the
general distribution of [source code and data files](https://github.com/mk270/whitakers-words/).

Here is a quick reference for the most common types.

<PRE><TT>

--  All first declension nouns  - N 1 1
--  Ex: aqua aquae  =>  aqu aqu

--  Second declension nouns in "us"  - N 2 1
--  Ex: amicus amici  =>  amic amic

--  Second declension neuter nouns - N 2 2
--  Ex: verbum verbi  =>  verb verb

--  Second declension nouns in "er" whether of not the "er" in base - N 2 3
--  Ex; puer pueri  =>  puer puer
--  Ex: ager agri   =>  ager agr

--  Early (BC) 2nd declension nouns in ius/ium (not filius-like)  - N 2 4
--  for the most part formed GEN S in 'i', not 'ii'   --  G+L 33 R 1
--  Dictionaries often show as ...(i)i
--  N 2 4 uses GENDER discrimination to reduce to single VAR
--  Ex: radius rad(i)i  => radi radi        M
--  Ex: atrium atr(i)i  =>  atri atri       N

--  Third declension M or F nouns whose stems end in a consonant - N 3 1
--  Ex: miles militis  =>  miles milit
--  Ex: lex legis  =>  lex leg
--  Ex: frater fratris  =>  frater fratr
--  Ex: soror sororis  =>  soror soror
--  All third declension that have the endings -udo, -io, -tas, -x
--  Ex: pulcritudo pulcritudinis  =>  plucritudo pulcritudin
--  Ex: legio legionis  =>  legio legion
--  Ex: varietas varietatis  =>  varietas varietat
--  Ex: radix radicis  =>  radix  radic

--  Third declension  N nouns with stems ending in a consonant - N 3 2
--  Ex: nomen nomenis  =>  nomen nomen
--  Ex: iter itineris =>  iter itiner
--  Ex: tempus temporis  =>  tempus  tempor

--  Third declension nouns  I-stems (M + F)     - N 3 3
--  Ex: hostis hostis  =>  hostis host
--  Ex: finis finis  =>  finis fin
--  Consonant i-stems
--  Ex: urbs urbis  =>  urbs urb
--  Ex: mons montis  =>  mons mont
--  Also use this for present participles (-ns) used as substantives in M + F

--  Third declension nouns  I-stems (N)    - N 3 4
--  Ex: mare amris  =>  mare mar                       --  ending in "e"
--  Ex: animal animalis  =>  animal animal             --  ending in "al"
--  Ex: exemplar exemplaris  =>  exemplar exemplar     --  ending in "ar"
--  Also use this for present participles (-ns) used as substantives in N

--  Fourth declension nouns M + F in "us"  - N 4 1
--  Ex: passus passus  =>  pass pass
--  Ex: manus manus  =>  man man

--  Fourth declension nouns N in "u"  - N 4 2
--  Ex: genu genus  =>  gen gen
--  Ex: cornu cornus  =>  corn corn

--  All fifth declension nouns  - N 5 1
--  Ex: dies diei  =>  di di
--  Ex: res rei  =>  r r



--  Adjectives will mostly only be POS and have only the first two stems
--  ADJ X have four stems, zzz stands for any unknown/non-existent stem

--  Adjectives of first and second declension (-us in NOM S M)  - ADJ 1 1
--  Two stems for POS, third is for COMP, fourth for SUPER
--  Ex: malus mala malum  => mal mal pei pessi
--  Ex: altus alta altum  => alt alt alti altissi

--  Adjectives of first and second declension (-er) - ADJ 1 2
--  Ex: miser misera miserum  =>  miser miser miseri miserri
--  Ex: sacer sacra sacrum  =>  sacer sacr zzz  sacerri     --  no COMP
--  Ex: pulcher pulchri  =>  pulcher pulchr pulchri pulcherri

--  Adjectives of third declension - one ending  - ADJ 3 1
--  Ex: audax (gen) audacis  =>  audax audac audaci audacissi
--  Ex: prudens prudentis  =>  prudens prudent prudenti prudentissi

--  Adjectives of third declension - two endings   - ADJ 3 2
--  Ex: brevis breve  =>  brev brev brevi brevissi
--  Ex: facil facil   =>  facil facil facili facilli

--  Adjectives of third declension - three endings  - ADJ 3 3
--  Ex: celer celeris  celere  =>  celer celer celeri celerri
--  Ex: acer acris acre  =>  acer acr acri acerri







--  Verbs are mostly TRANS or INTRANS, but X works fine
--  Depondent verbs must have DEP
--  Verbs have four stems
--  The first stem is the first principal part (dictionary entry) - less 'o'
--  For 2nd decl, the 'e' is omitted, for 3rd decl i-stem, the 'i' is included
--  Third principal part always ends in 'i', this is omitted in stem
--  Fourth part in dictionary ends in -us (or -um), this is omitted
--  DEP verbs omit (have zzz) the third stem

--  Verbs of the first conjugation  --  V 1 1
--  Ex: voco vocare vocavi vocatus  =>  voc voc vocav vocat
--  Ex: porto portave portavi portatus  =>  port port portav portat

--  Verbs of the second conjugation   -  V 2 1
--  The characteristic 'e' is in the inflection, not carried in the stem
--  Ex:  moneo monere monui monitum  =>  mon mon monu monit
--  Ex:  habeo habere habui habitus  =>  hab hab habu habit
--  Ex:  deleo delere delevi deletus  =>  del del delev delet
--  Ex:  iubeo iubere iussi iussus  =>   iub iub iuss iuss
--  Ex:  video videre vidi visus  =>  vid vid vid vis

--  Verbs of the third conjugation, variant 1  - V 3 1
--  Ex: rego regere rexi rectum  =>  reg reg rex rect
--  Ex: pono ponere posui positus  =>  pon pon posu posit
--  Ex: capio capere cepi captus  => capi cap cep capt   --  I-stem too w/KEY

--  Verbs of the fourth conjugation are coded as a variant of third - V 3 4
--  Ex: audio audire audivi auditus  =>  audi aud audiv audit

--  Verbs like to be - coded as V 5 1
--  Ex: sum esse fui futurus  =>  s . fu fut
--  Ex: adsum adesse adfui adfuturus  =>  ads ad adfu adfut

</TT></PRE>


UNIQUES.LAT
-----------

There are a few Latin words that cannot be represented with the scheme of
stems and endings used by the program.  For these very few cases, the
program invokes a unique procedure.  The file UNIQUES.  contains a list of
such words and is read in at the loading of the program.  This is a simple
ASCII/DOS text file which the user can augment.  It is expected that there
will be very few occasions to do so, indeed, the tendency has been that
better processing has allowed uniques to be removed.  If a user finds an
important word that should be included, please communicate that to the
author.

The UNIQUES record is essentially the form as one might have it in output
if the word was processed normally.  In addition there are some additional
fields that the program presently expects.  While these could be
eliminated, it is convenient for the program not to make the UNIQUES a
special case.  So a noun form

<PRE><TT>
N 3 1 ACC S F T
</TT></PRE>
is followed by two zeros and an X

<PRE><TT>
N 3 1 ACC S F T  0 0                            X        X  X  X  B  O
</TT></PRE>
and then the five X's or, more properly, the dictionary codes.

<PRE><TT>
N 3 1 ACC S F T  0 0                            X        X  X  X  B  O
</TT></PRE>


These pro forma codes are absolutely necessary, but have no further
impact.

The program is written in Ada and uses Ada techniques.  Ada is designed
for high reliability systems (there is no claim the WORDS was developed
with all the other safeguards that that implies!) as a consequence is
unforgiving.  The exact form is required.  If you want to be sloppy you
have to deliberately program that in.

The following examples, and an examination of the UNIQUES.LAT file, should
allow the user to insert any unique necessary.

<PRE><TT>
requiem
N 3 1 ACC S F T  0 0                            X        X  X  X  B  O
rest (from labor), respite; intermission, pause, break; amusement, hobby;
bobus
N 3 1 DAT P C T  0 0                            X        X  X  X  C  X
ox, bull; cow; cattle (pl.)
quicquid
PRON 1 6 NOM S N INDEF   0 0                    X        X  X  X  B  X
whatever, whatsoever; everything which; each one; each; everything; anything
mavis
V     6 2 PRES  ACTIVE  IND  2 S X 0 0          X        X  X  X  B  X
prefer
cette
V    3 1 PRES ACTIVE IMP  2 P TRANS    0 0      X        X  X  X  B  O
give/bring here!/hand over, come (now/here); tell/show us, out with it! behold!
</TT></PRE>

