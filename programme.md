---
layout: default
title: Programme description
---

[Operational Description](operational.html) |
[Programme Description](programme.html) |
[Dictionary](dictionary.html) |
[Inflections](inflections.html) |
[English to Latin](english_to_latin.html) |
[User modifications](user_modifications.html)

# Programme description


A effect of the program is to derive the structure and meaning of
individual Latin words.  A procedure was devised to: examine the ending of
a word, compare it with the standard endings, derive the possible stems
that could be consistent, compare those stems with a dictionary of stems,
eliminate those for which the ending is inconsistent with the dictionary
stem (e.g., a verb ending with a noun dictionary item), if unsuccessful,
it tries with a large set of prefixes and suffixes, and various tackons
(e.g., -que), finally it tries various 'tricks' (e.g., 'ae' may be
replaced by 'e', 'inp...' by 'imp...', syncope, etc.), and it reports any
resulting matches as possible interpretations.

With the input of a word, or several words in a line, the program returns
information about the possible accedience, if it can find an agreeable
stem in its dictionary.

    =>amo
    am.o               V       1  1 PRES ACTIVE  IND  1 S
    love, like; fall in love with; be fond of; have a tendency to


To support this method, an INFLECT.SEC data file was constructed
containing possible Latin endings encoded by a structure that identifies
the part of speech, declension, conjugation, gender, person, number, etc.
This is a pure computer encoding for a 'brute force' search.  No
sophisticated knowledge of Latin is used at this point.  Rules of thumb
(e.g., the fact, always noted early in any Latin course, that a neuter
noun has the same ending in the nominative and accusative, with a final -a
in the plural) are not used in the search.  However, it is convenient to
combine several identical endings with a general encoding (e.g., the
endings of the perfect tenses are the same for all verbs, and are so
encoded, not replicated for every conjugation and variant).

Many of the distinguishing differences identifying conjugations come from
the voiced length of stem vowels (e.g., between the present, imperfect and
future tenses of a third conjugation I-stem verb and a fourth conjugation
verb).  These aural differences, the features that make Latin 'sound
right' to one who speaks it, are not relevant in the analysis of written
endings.

The endings for the verb conjugations are the result of trying to minimize
the number of individual endings records, while yet keeping the structure
of the inflections data file fairly readable.  There is no claim that the
resulting arrangement is consonant with any grammarian's view of Latin,
nor should it be examined from that viewpoint.  While it started from the
conjugations in text books, it can only be viewed as some fuzzy
intermediate step along a path to a mathematically minimal number of
encoded verb endings.  Later versions of the program might improve the
system.

There are some egregious liberties taken in the encoding.  With the
inclusion of two present stems, the third conjugation I-stem verbs may
share the endings of the regular third conjugation.  The fourth
conjugation has disappeared altogether, and is represented internally as a
variant of the third conjugation (3, 4), but this is
replaced for the user in output by 4 1. There is an artificial fifth
conjugation for esse and others, a sixth for eo, and a seventh for other
irregularities.

As an example, a verb ending record has the structure:

* PART -- the part code for a verb = V;
* CONjugation -- consisting of two parts:
* WHICH -- a conjugation identifier - range 0..9 and
* VAR -- a variant identifier on WHICH - range 0..9;
* TENSE -- an enumeration type - range PRES..FUTP + X;
* VOICE -- an enumeration type - range ACTIVE..PASSIVE + X;
* MOOD -- an enumeration type - range IND..PPL + X;
* PERSON -- person, first to third - range 1..3 + 0;
* NUMBER -- an enumeration type - range S..P + X;
* KEY -- which stem to be used - range 1..4;
* SIZE -- number of characters - range 0..9;
* ENDING -- the ending as a string of SIZE characters;
* AGE and FREQ flags which are not usually visible to the user.

Thus, the entry for the ending appropriate to 'amo' (with STEM = am) is:

    V 1 1 PRES IND ACTIVE 1 S X 1 o

The elements are straightforward and generally use the
abbreviations that are common in any Latin text.  An X or 0 represents the
'don't know' or 'don't care' for enumeration or numeric types.  Details
are documented below in the CODES section.

A verb dictionary record has the structure:

* STEMS -- for a verb there are 4 stems;
* PART --  part code for a verb = V
* WHICH -- a conjugation identifier - range 0..9
* VAR -- a variant identifier - range 0..9;
* KIND -- enumeration type of verb - range TO_BE..PERFDEF + X;
* AGE, AREA, GEO, FREQ, and SOURCE flags
* MEANING -- text for English translations (up to 80 characters).

Thus, an entry corresponding to 'amo amare amavi amatus' is:

    am am amav amat
    V 1 1 X            X X X X X
    love, like; fall in love with; be fond of; have a tendency to



Endings may not uniquely determine which stem, and therefore the right
meaning.  'portas' could be the accusative plural of 'gate', or the second
person, singular, present indicative active of 'carry'.  In both cases the
stem is 'port'.  All possibilities are reported.

    portas
    port.as V 1 1 PRES IND ACTIVE 2 S X
    carry, bring

    port.as N 1 1 ACC P F T
    gate, entrance; city gates; door; avenue;


And note that the same stem (port) has other uses (portus = harbor).

    portum
    port.um N 4 1 ACC S M T
    port, harbor; refuge, haven, place of refuge


PLEASE NOTE: It is certainly possible for the program to find a valid
Latin construction that fits the input word and to have that
interpretation be entirely wrong in the context.  It is even possible to
interpret a number, in Roman numerals, as a word!  (But the number would
be reported also.)


For the case of defective verbs, the process does not necessarily have to
be precise.  Since the purpose is only to translate from Latin, even if
there are unused forms included in the algorithm these will not come up
in any real Latin text.  The endings for the verb conjugations are the
result of trying to minimize the number of individual endings records,
while keeping the structure of the base INFLECTIONS data file fairly
readable.

In general the program will try to construct a match with the inflections
and the dictionaries.  There are some specific checks to reject
certain mathematically correct combinations that do not appear in the
language, but these checks are relatively few.  The philosophy has been to
allow a generous interpretation.  A remark in a text or dictionary that a
particular form does not exist must be tempered with the realization that
the author probably means that it has not been observed in the surviving
classical literature.  This body of reference is minuscule compared to the
total use of Latin, even limited to the classical period.  Who is to say
that further examples would not turn up such an example, even if it might
not have been approved of by Cicero.  It is also possible that such
reasonable, if 'improper', constructs might occur in later writings by
less educated, or just different, authors.  Certainly English shows this
sort of variation over time.

If the exact stem is not found in the dictionary, there are rules for the
construction of words which any student would try.  The simplest situation
is a known stem to which a prefix or suffix has been attached.  The method used
by the program (if DO_FIXES is on, default is Yes) is to try any fixes that fit,
to see if their removal results in an identifiable remainder.  Then the
meaning is mechanically implied from the meaning of the fix and the
stem.  The user may need to interpret with a more conventional English
usage.  This technique improves the hit performance significantly.  However,
in about 40% of the instances in which there is a hit, the derivation is
correct but the interpretation takes some imagination.  In something less
than 10% of the cases, the inferred fix is just wrong, so the user must
take some care to see if the interpretation makes any sense.

This method is complicated by the tendency for prefixes to be modified
upon attachment (ab+fero = aufero, sub+fero = suffero).  The program's
'tricks' take many such instances into account.  Ideally, one should look
inside the stem for identifiable fragments.  One would like to start with
the smallest possible stem, and that is most frequently the correct one.
While it is mathematically possible that the stem of 'actorum' is 'actor'
with the common inflection 'um', no intuitive first semester Latin student
would fail to opt for the genitive plural 'orum', and probably be right.
To first order, the procedure ignores such hints and may report this word in
both forms, as well as a verb participle.  However, it can use certain
generally applicable rules, like the superlative characteristic 'issim',
to further guess.

In addition, there is the capability to examine the word for such common
techniques as syncope, the omission of the 've' or 'vi' in certain verb
perfect forms (audivissem = audissem).

If the dictionary can not identify a matching stem, it may be possible to
derive a stem from 'nearby' stems (an adverb from an adjective is the most
common example) and infer a meaning.  If all else fails, a portion of the
possible dictionary stems can be listed, from which the user can draw in
making his own guess.


## Codes in Inflection Line

For completeness, the enumeration codes used in the output are listed here
from the Ada statements.  Simple numbers are used for person, declension,
conjugations, and their variants.  Not all the facilities implied by these
values are developed or used in the program or the dictionary.  This list
is only for Version 1.97E.  Other versions may be somewhat different.  This
may make their dictionaries incompatible with the present program.

NOTE: in print dictionaries certain information is conveyed by font
encoding, e.g., the use of bold face or italics.  There is no system
independent method of displaying such on computers (although individual
programs can handle these, each in it own unique way).  WORDS uses capital
letters to express some such differences, which method is system independent
in present usage.


     type PART_OF_SPEECH_TYPE
              X,         --  all, none, or unknown
              N,         --  Noun
              PRON,      --  PRONoun
              PACK,      --  PACKON -- artificial for code
              ADJ,       --  ADJective
              NUM,       --  NUMeral
              ADV,       --  ADVerb
              V,         --  Verb
              VPAR,      --  Verb PARticiple
              SUPINE,    --  SUPINE
              PREP,      --  PREPosition
              CONJ,      --  CONJunction
              INTERJ,    --  INTERJection
              TACKON,    --  TACKON --  artificial for code
              PREFIX,    --  PREFIX --  here artificial for code
              SUFFIX     --  SUFFIX --  here artificial for code

      type GENDER_TYPE
              X,         --  all, none, or unknown
              M,         --  Masculine
              F,         --  Feminine
              N,         --  Neuter
              C          --  Common (masculine and/or feminine)

      type CASE_TYPE
              X,         --  all, none, or unknown
              NOM,       --  NOMinative
              VOC,       --  VOCative
              GEN,       --  GENitive
              LOC,       --  LOCative
              DAT,       --  DATive
              ABL,       --  ABLative
              ACC        --  ACCusative

      type NUMBER_TYPE
              X,         --  all, none, or unknown
              S,         --  Singular
              P          --  Plural

      type PERSON_TYPE is range 0..3;

      type COMPARISON_TYPE
              X,         --  all, none, or unknown
              POS,       --  POSitive
              COMP,      --  COMParative
              SUPER      --  SUPERlative

      type NUMERAL_SORT_TYPE
             X,          --  all, none, or unknown
             CARD,       --  CARDinal
             ORD,        --  ORDinal
             DIST,       --  DISTributive
             ADVERB      --  numeral ADVERB

      type TENSE_TYPE
              X,         --  all, none, or unknown
              PRES,      --  PRESent
              IMPF,      --  IMPerFect
              FUT,       --  FUTure
              PERF,      --  PERFect
              PLUP,      --  PLUPerfect
              FUTP       --  FUTure Perfect

      type VOICE_TYPE
              X,         --  all, none, or unknown
              ACTIVE,    --  ACTIVE
              PASSIVE    --  PASSIVE

      type MOOD_TYPE
              X,         --  all, none, or unknown
              IND,       --  INDicative
              SUB,       --  SUBjunctive
              IMP,       --  IMPerative
              INF,       --  INFinitive
              PPL        --  ParticiPLe

      type NOUN_KIND_TYPE
              X,            --  unknown, nondescript
              S,            --  Singular "only"           --  not really used
              M,            --  plural or Multiple "only" --  not really used
              A,            --  Abstract idea
              G,            --  Group/collective Name -- Roman(s)
              N,            --  proper Name
              P,            --  a Person
              T,            --  a Thing
              L,            --  Locale, name of country/city
              W             --  a place Where

      type PRONOUN_KIND_TYPE
              X,            --  unknown, nondescript
              PERS,         --  PERSonal
              REL,          --  RELative
              REFLEX,       --  REFLEXive
              DEMONS,       --  DEMONStrative
              INTERR,       --  INTERRogative
              INDEF,        --  INDEFinite
              ADJECT        --  ADJECTival

       type VERB_KIND_TYPE
              X,         --  all, none, or unknown
              TO_BE,     --  only the verb TO BE (esse)
              TO_BEING,  --  compounds of the verb to be (esse)
              GEN,       --  verb taking the GENitive
              DAT,       --  verb taking the DATive
              ABL,       --  verb taking the ABLative
              TRANS,     --  TRANSitive verb
              INTRANS,   --  INTRANSitive verb
              IMPERS,    --  IMPERSonal verb (implied subject 'it', 'they', 'God')
                         --  agent implied in action, subject in predicate
              DEP,       --  DEPonent verb
                         --  only passive form but with active meaning
              SEMIDEP,   --  SEMIDEPonent verb (forms perfect as deponent)
                         --  (perfect passive has active force)
              PERFDEF    --  PERFect DEFinite verb
                         --  having only perfect stem, but with present force


The KIND_TYPEs represent various aspects of a word which may be useful to
some program, not necessarily the present one.  They were put in for
various reasons, and later versions may change the selection and use.
Some of the KIND flags are never used.  In some cases more than one KIND
flag might be appropriate, but only one is selected.  Some seemed to be a
good idea at one time, but have not since proved out.  The lists above are
just for completeness.

NOUN KIND is used in trimming (when set) the output and removing possibly
spurious cases (locative for a person, but preserving the vocative).

VERB KIND allows examples (when set) to give a more reasonable meaning.  A
DEP flag allows the example to reflect active meaning for passive form.
It also allows the dictionary form to be constructed properly from stems.
TRANS/INTRANS were included to allow a further program a hint as to what
kind of object it should expect.  This flag is only now being fixed during
the update.  There are some verbs which, although mostly used in one way,
might be either.  These are assigned X rather than breaking into two
entries.  This would be of no particular use at this point since it would
not allow the object to be determined.  GEN/DAT/ABL flags have related
function, but are almost absent.  TO_BE is used to indicate that a form of
esse may be part of a compound verb tense with a participle.  TO_BEING
indicates a verb related to esse (e.g., abesse) which has no object,
neither is in used to form compounds.  IMPERS is used to weed out person
and forms inappropriate to an impersonal verb, and to insert a special
meaning distinct from a general form associated with the same verb stem.

There is a problem in that all values for this parameter are not orthogonal.
DEP is a different sort of thing from INTRANS.  There ought to be a
KIND_1 and KIND_2 to separate the different classes.  However, this would
be overkill considering the use made of this parameter, so far.


There is a more difficult DEP problem.
'Good Latin' requires that the DEP be recognized and
processed to eliminate active forms.
In some cases there are dictionary examples, mostly medieval,
of the depondency being violated.
Some of those cases have been recognized with a separate entry.
This is not something that a suffix can handle appropriately,
even if mechanically it can function.
A better way might be to include the perfect form but still have the DEP flag,
thereby allow the trimming in most cases. This has not been done yet.
But an active form would be recognized if input, especially if the text is medieval.


NUMERAL KIND and VALUE are used by the program in constructing the meaning line.

## Help for Parameters


One can CHANGE_PARAMETERS by inputting a '#' [number sign] character (ASCII
35) as the input word, followed by a return.  (Note that this has changed
from early versions in which '?' was used.) Each parameter is listed and
the user is offered the opportunity to change it from the current value by
answering Y or N (any case).  For each parameter there is some explanation
or help.  This is displayed by in putting a '?' [question mark], followed
by a return.  HINT: While going down the list if one has made all the
changes desired, one need not continue to the end.  Just enter a space and
then give a return.  The program will interpret this as an illegal entry
(not Y or N) and will cancel the rest of the list, while retaining any
changes made to that point.

Some parameters may not function in the English mode, nor is the documentation
necessarily complete,


The various help displays are listed here:


    TRIM_OUTPUT_HELP
       This option instructs the program to remove from the output list of
       possible constructs those which are least likely.  There is now a fair
       amount of trimming, killing LOC and VOC plus removing Uncommon and
       non-classical (Archaic/Medieval) when more common results are found
       and this action is requested (turn it off in MDV (!) parameters).
       When a TRIM has been done, the output is followed by an asterix (*).
       There certainly is no absolute assurence that the items removed are
       not correct, just that they are statistically less likely.
       Note that poets are likely to employ unusual words and inflections for
       various reasons.  These may be trimmed out if this parameter in on.
       When in English mode, trim just reduces the output to the top six
       results, if there are that many.  Asterix means there are more
                                                       The default is Y(es)

    HAVE_OUTPUT_FILE_HELP
       This option instructs the program to create a file which can hold the
       output for later study, otherwise the results are just displayed on
       the screen.  The output file is named  WORD.OUT
       This means that one run will necessarily overwrite a previous run,
       unless the previous results are renamed or copied to a file of another
       name.  This is available if the METHOD is INTERACTIVE, no parameters.
       The default is N(o), since this prevents the program from overwriting
       previous work unintentionally.  Y(es) creates the output file.

    WRITE_OUTPUT_TO_FILE_HELP
       This option instructs the program, when HAVE_OUTPUT_FILE is on, to
       write results to the WORD.OUT file.
       This option may be turned on and off during running of the program,
       thereby capturing only certain desired results.  If the option
       HAVE_OUTPUT_FILE is off, the user will not be given a chance to turn
       this one on.  Only for INTERACTIVE running.         Default is N(o).
       This works in English mode, but output in somewhat different so far.

    DO_UNKNOWNS_ONLY_HELP
       This option instructs the program to only output those words that it
       cannot resolve.  Of course, it has to do processing on all words, but
       those that are found (with prefix/suffix, if that option in on) will
       be ignored.  The purpose of this option is t allow a quick look to
       determine if the dictionary and process is going to do an acceptable
       job on the current text.  It also allows the user to assemble a list
       of unknown words to look up manually, and perhaps augment the system
       dictionary.  For those purposes, the system is usually run with the
       MINIMIZE_OUTPUT option, just producing a list.  Another use is to run
       without MINIMIZE to an output file.  This gives a list of the input
       text with the unknown words, by line.  This functions as a spelling
       checker for Latin texts.  The default is N(o).
       This does not work in English mode, but may in the future.

    WRITE_UNKNOWNS_TO_FILE_HELP
       This option instructs the program to write all unresolved words to a
       UNKNOWNS file named  WORD.UNK
       With this option on , the file of unknowns is written, even though
       the main output contains both known and unknown (unresolved) words.
       One may wish to save the unknowns for later analysis, testing, or to
       form the basis for dictionary additions.  When this option is turned
       on, the UNKNOWNS file is written, destroying any file from a previous
       run.  However, the write may be turned on and off during a single run
       without destroying the information written in that run.
       This option is for specialized use, so its default is N(o).
       This does not work in English mode, but may in the future.

    IGNORE_UNKNOWN_NAMES_HELP
       This option instructs the program to assume that any capitalized word
       longer than three letters is a proper name.  As no dictionary can be
       expected to account for many proper names, many such occur that would
       be called UNKNOWN.  This contaminates the output in most cases, and
       it is often convenient to ignore these spurious UNKNOWN hits.  This
       option implements that mode, and calls such words proper names.
       Any proper names that are in the dictionary are handled in the normal
       manner.                                The default is Y(es).

    IGNORE_UNKNOWN_CAPS_HELP
       This option instructs the program to assume that any all caps word
       is a proper name or similar designation.  This convention is often
       used to designate speakers in a discussion or play.  No dictionary can
       claim to be exhaustive on proper names, so many such occur that would
       be called UNKNOWN.  This contaminates the output in most cases, and
       it is often convenient to ignore these spurious UNKNOWN hits.  This
       option implements that mode, and calls such words names.  Any similar
       designations that are in the dictionary are handled in the normal
       manner, as are normal words in all caps.    The default is Y(es).

    DO_COMPOUNDS_HELP
       This option instructs the program to look ahead for the verb TO_BE (or
       iri) when it finds a verb participle, with the expectation of finding
       a compound perfect tense or periphrastic.  This option can also be a
       trimming of the output, in that VPAR that do not fit (not NOM) will be
       excluded, possible interpretations are lost.  Default choice is Y(es).
       This processing is turned off with the choice of N(o).

    DO_FIXES_HELP
       This option instructs the program, when it is unable to find a proper
       match in the dictionary, to attach various prefixes and suffixes and
       try again.  This effort is successful in about a quarter of the cases
       which would otherwise give UNKNOWN results, or so it seems in limited
       tests.  For those cases in which a result is produced, about half give
       easily interpreted output; many of the rest are etymologically true,
       but not necessarily obvious; about a tenth give entirely spurious
       derivations.  The user must proceed with caution.
       The default choice is Y(es), since the results are generally useful.
       This processing can be turned off with the choice of N(o).

    DO_TRICKS_HELP
       This option instructs the program, when it is unable to find a proper
       match in the dictionary, and after various prefixes and suffixes, to
       try every dirty Latin trick it can think of, mainly common letter
       replacements like cl -> cul, vul -> vol, ads -> ass, inp -> imp, etc.
       Together these tricks are useful, but may give false positives (>10%).
       They provide for recognized variants in classical spelling.  Most of
       the texts with which this program will be used have been well edited
       and standardized in spelling.  Now, moreover,  the dictionary is being
       populated to such a state that the hit rate on tricks has fallen to a
       low level.  It is very seldom productive, and it is always expensive.
       The only excuse for keeping it as default is that now the dictionary
       is quite extensive and misses are rare.         Default is now Y(es).

    DO_DICTIONARY_FORMS_HELP
       This option instructs the program to output a line with the forms
       normally associated with a dictionary entry (NOM and GEN of a noun,
       the four principal parts of a verb, M-F-N NOM of an adjective, ...).
       This occurs when there is other output (i.e., not with UNKNOWNS_ONLY).
       The default choice is N(o), but it can be turned on with a Y(es).

    SHOW_AGE_HELP
       This option causes a flag, like '<Late>' to appear for inflection or
       form in the output.  The AGE indicates when this word/inflection was
       in use, at least from indications is dictionary citations.  It is
       just an indication, not controlling, useful when there are choices.
       No indication means that it is common throughout all periods.
       The default choice is Y(es), but it can be turned off with a N(o).

    SHOW_FREQUENCY_HELP
       This option causes a flag, like '<rare>' to appear for inflection or
       form in the output.  The FREQ is indicates the relative usage of the
       word or inflection, from indications is dictionary citations.  It is
       just an indication, not controlling, useful when there are choices.
       No indication means that it is common throughout all periods.
       The default choice is Y(es), but it can be turned off with a N(o).

    DO_EXAMPLES_HELP
       This option instructs the program to provide examples of usage of the
       cases/tenses/etc. that were constructed.  The default choice is N(o).
       This produces lengthy output and is turned on with the choice Y(es).

    DO_ONLY_MEANINGS_HELP
       This option instructs the program to only output the MEANING for a
       word, and omit the inflection details.  This is primarily used in
       analyzing new dictionary material, comparing with the existing.
       However it may be of use for the translator who knows most all of
       the words and just needs a little reminder for a few.
       The default choice is N(o), but it can be turned on with a Y(es).

    DO_STEMS_FOR_UNKNOWN_HELP
       This option instructs the program, when it is unable to find a proper
       match in the dictionary, and after various prefixes and suffixes, to
       list the dictionary entries around the unknown.  This will likely
       catch a substantive for which only the ADJ stem appears in dictionary,
       an ADJ for which there is only a N stem, etc.  This option should
       probably only be used with individual UNKNOWN words, and off-line
       from full translations, therefore the default choice is N(o).
       This processing can be turned on with the choice of Y(es).

    SAVE_PARAMETERS_HELP
       This option instructs the program, to save the current parameters, as
       just established by the user, in a file WORD.MOD.  If such a file
       exists, the program will load those parameters at the start.  If no
       such file can be found in the current subdirectory, the program will
       start with a default set of parameters.  Since this parameter file is
       human-readable ASCII, it may also be created with a text editor.  If
       the file found has been improperly created, is in the wrong format, or
       otherwise uninterpretable by the program, it will be ignored and the
       default parameters used, until a proper parameter file in written by
       the program.  Since one may want to make temporary changes during a
       run, but revert to the usual set, the default is N(o).



There is also a set of DEVELOPER_PARAMETERS that are unlikely to be of
interest to the normal user.  Some of these facilities may be disconnected
or not work for other reasons.  Additional parameters may be included
without notice or documentation.  The HELP may be the most reliable
source of information.  These parameters are mostly for the use in the
development process.  These may be changed or examined by in similar
change procedure by inputting a '!' [exclamation sign] character, followed
by a return.


    HAVE_STATISTICS_FILE_HELP
       This option instructs the program to create a file which can hold
       certain statistical information about the process.  The file is
       overwritten for new invocation of the program, so old data must be
       explicitly saved if it is to be retained.  The statistics are in TEXT
       format.     The statistics file is named  WORD.STA
       This information is only of development use, so the default is N(o).

    WRITE_STATISTICS_FILE_HELP
       This option instructs the program, with HAVE_STATISTICS_FILE, to put
       derived statistics in a file named  WORD.STA
       This option may be turned on and off while running of the program,
       thereby capturing only certain desired results.  The file is reset at
       each invocation of the program, if the HAVE_STATISTICS_FILE is set.
       If the option HAVE_STATISTICS_FILE is off, the user will not be given
       a chance to turn this one on.                Default is N(o).

    SHOW_DICTIONARY_HELP
       This option causes a flag, like 'GEN>' to be put before the meaning
       in the output.  While this is useful for certain development purposes,
       it forces off a few characters from the meaning, and is really of no
       interest to most users.
       The default choice is N(o), but it can be turned on with a Y(es).

    SHOW_DICTIONARY_LINE_HELP
       This option causes the number of the dictionary line for the current
       meaning to be output.  This is of use to no one but the dictionary
       maintainer.  The default choice is N(o).  It is activated by Y(es).

    SHOW_DICTIONARY_CODES_HELP
       This option causes the codes for the dictionary entry for the current
       meaning to be output.  This may not be useful to any but the most
       involved user.  The default choice is N(o).  It is activated by Y(es).

    DO_PEARSE_CODES_HELP
       This option causes special codes to be output flagging the different
       kinds of output lines.  01 for forms, 02 for dictionary forms, and
       03 for meaning. The default choice is N(o).  It is activated by Y(es).
       There are no Pearse codes in English mode.

    DO_ONLY_INITIAL_WORD_HELP
       This option instructs the program to only analyze the initial word on
       each line submitted.  This is a tool for checking and integrating new
       dictionary input, and will be of no interest to the general user.
       The default choice is N(o), but it can be turned on with a Y(es).

    FOR_WORD_LIST_CHECK_HELP
       This option works in conjunction with DO_ONLY_INITIAL_WORD to allow
       the processing of scanned dictionaries or text word lists.  It accepts
       only the forms common in dictionary entries, like NOM S for N or ADJ,
       or PRES ACTIVE IND 1 S for V.  It is be used only with DO_INITIAL_WORD
       The default choice is N(o), but it can be turned on with a Y(es).

    DO_ONLY_FIXES_HELP
       This option instructs the program to ignore the normal dictionary
       search and to go direct to attach various prefixes and suffixes before
       processing. This is a pure research tool.  It allows one to examine
       the coverage of pure stems and dictionary primary compositions.
       This option is only available if DO_FIXES is turned on.
       This is entirely a development and research tool, not to be used in
       conventional translation situations, so the default choice is N(o).
       This processing can be turned on with the choice of Y(es).

    DO_FIXES_ANYWAY_HELP
       This option instructs the program to do both the normal dictionary
       search and then process for the various prefixes and suffixes too.
       This is a pure research tool allowing one to consider the possibility
       of strange constructions, even in the presence of conventional
       results, e.g., alte => deeply (ADV), but al+t+e => wing+ed (ADJ VOC)
       (If multiple suffixes were supported this could also be wing+ed+ly.)
       This option is only available if DO_FIXES is turned on.
       This is entirely a development and research tool, not to be used in
       conventional translation situations, so the default choice is N(o).
       This processing can be turned on with the choice of Y(es).
             ------    PRESENTLY NOT IMPLEMENTED    ------

    USE_PREFIXES_HELP
       This option instructs the program to implement prefixes from ADDONS
       whenever and wherever FIXES are called for.  The purpose of this
       option is to allow some flexibility while the program in running to
       select various combinations of fixes, to turn them on and off,
       individually as well as collectively.  This is an option usually
       employed by the developer while experimenting with the ADDONS file.
       This option is only effective in connection with DO_FIXES.
       This is primarily a development tool, so the conventional user should
       probably maintain the default  choice of Y(es).

    USE_SUFFIXES_HELP
       This option instructs the program to implement suffixes from ADDONS
       whenever and wherever FIXES are called for.  The purpose of this
       option is to allow some flexibility while the program in running to
       select various combinations of fixes, to turn them on and off,
       individually as well as collectively.  This is an option usually
       employed by the developer while experimenting with the ADDONS file.
       This option is only effective in connection with DO_FIXES.
       This is primarily a development tool, so the conventional user should
       probably maintain the default  choice of Y(es).

    USE_TACKONS_HELP
       This option instructs the program to implement TACKONS from ADDONS
       whenever and wherever FIXES are called for.  The purpose of this
       option is to allow some flexibility while the program in running to
       select various combinations of fixes, to turn them on and off,
       individually as well as collectively.  This is an option usually
       employed by the developer while experimenting with the ADDONS file.
       This option is only effective in connection with DO_FIXES.
       This is primarily a development tool, so the conventional user should
       probably maintain the default  choice of Y(es).

    DO_MEDIEVAL_TRICKS_HELP
       This option instructs the program, when it is unable to find a proper
       match in the dictionary, and after various prefixes and suffixes, and
       trying every Classical Latin trick it can think of, to go to a few 
       that are usually only found in medieval Latin, replacements of z -> di
       caul -> col, st -> est, ix -> is, nct -> nt. It also tries some things
       like replacing doubled consonants in classical with a single one.
       Together these tricks are useful, but may give false positives (>20%).
       This option is only available if the general DO_TRICKS is chosen.
       If the text is late or medieval, this option is much more useful than
       tricks for classical.  The dictionary can never contain all spelling
       variations found in medieval Latin, but some constructs are common.
       The default choice is N(o), since the results are iffy, medieval only,
       and expensive.  This processing is turned on with the choice of Y(es).

    DO_SYNCOPE_HELP
       This option instructs the program to postulate that syncope of
       perfect stem verbs may have occurred (e.g, aver -> ar in the perfect),
       and to try various possibilities for the insertion of a removed 'v'.
       To do this it has to fully process the modified candidates, which can
       have a considerable impact on the speed of processing a large file.
       However, this trick seldom produces a false positive, and syncope is
       very common in Latin (first year texts excepted).  Default is Y(es).
       This processing is turned off with the choice of N(o).

    DO_TWO_WORDS_HELP
       There are some few common Latin expressions that combine two inflected
       words (e.g. respublica, paterfamilias).  There are numerous examples
       of numbers composed of two words combined together.
       Sometimes a text or inscription will have words run together.
       When WORDS is unable to reach a satisfactory solution with all other
       tricks, as a last stab it will try to break the input into two words.
       This most often fails.  Even if mechanically successful, the result is
       usually false and must be examined by the user.  If the result is
       correct, it is probably clear to the user.  Otherwise,  beware.
       This problem will not occur for a well edited text, such as one will
       find on your Latin exam, but sometimes with raw text.
       Since this is a last chance and infrequent, the default is Y(es);
       This processing is turned off with the choice of N(o).

    INCLUDE_UNKNOWN_CONTEXT_HELP
       This option instructs the program, when writing to an UNKNOWNS file,
       to put out the whole context of the UNKNOWN (the whole input line on
       which the UNKNOWN was found).  This is appropriate for processing
       large text files in which it is expected that there will be relatively
       few UNKNOWNS.    The main use at the moment is to provide display
       of the input line on the output file in the case of UNKNOWNS_ONLY.

    NO_MEANINGS_HELP
       This option instructs the program to omit putting out meanings.
       This is only useful for certain dictionary maintenance procedures.
       The combination not DO_DICTIONARY_FORMS, MEANINGS_ONLY, NO_MEANINGS
       results in no visible output, except spacing lines.    Default is N)o.

    OMIT_ARCHAIC_HELP
       THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!
       This option instructs the program to omit inflections and dictionary
       entries with an AGE code of A (Archaic).  Archaic results are rarely
       of interest in general use.  If there is no other possible form, then
       the Archaic (roughly defined) will be reported.  The default is Y(es).

    OMIT_MEDIEVAL_HELP
       THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!
       This option instructs the program to omit inflections and dictionary
       entries with AGE codes of E or later, those not in use in Roman times.
       While later forms and words are a significant application, most users
       will not want them.  If there is no other possible form, then the
       Medieval (roughly defined) will be reported.   The default is Y(es).

    OMIT_UNCOMMON_HELP
       THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!
       This option instructs the program to omit inflections and dictionary
       entries with FREQ codes indicating that the selection is uncommon.
       While these forms area significant feature of the program, many users
       will not want them.  If there is no other possible form, then the
       uncommon (roughly defined) will be reported.   The default is Y(es).

    DO_I_FOR_J_HELP
       This option instructs the program to modify the output so that the j/J
       is represented as i/I.  The consonant i was written as j in cursive in
       Imperial times and called i longa, and often rendered as j in medieval
       times.  The capital is usually rendered as I, as in inscriptions.
       If this is NO/FALSE, the output will have the same character as input.
       The program default, and the dictionary convention is to retain the j.
       Reset if this is unsuitable for your application. The default is N(o).

    DO_U_FOR_V_HELP
       This option instructs the program to modify the output so that the u
       is represented as v.  The consonant u was written sometimes as uu.
       The pronunciation was as current w, and important for poetic meter.
       With the printing press came the practice of distinguishing consonant
       u with the character v, and was common for centuries.  The practice of
       using only u has been adopted in some 20th century publications (OLD),
        but it is confusing to many modern readers.  The capital is commonly
       V in any case, as it was and is in inscriptions (easier to chisel).
       If this is NO/FALSE, the output will have the same character as input.
       The program default, and the dictionary convention is to retain the v.
       Reset If this is unsuitable for your application. The default is N(o).

    PAUSE_IN_SCREEN_OUTPUT_HELP
       This option instructs the program to pause in output on the screen
       after about 16 lines so that the user can read the output, otherwise
       it would just scroll off the top.  A RETURN/ENTER gives another page.
       If the program is waiting for a return, it cannot take other input.
       This option is active only for keyboard entry or command line input,
       and only when there is no output file.  It is moot if only single word
       input or brief output.                 The default is Y(es).

    NO_SCREEN_ACTIVITY_HELP
       This option instructs the program not to keep a running screen of the
       input.  This is probably only to be used by the developer to calibrate
       run times for large text file input, removing the time necessary to
       write to screen.                       The default is N(o).

    UPDATE_LOCAL_DICTIONARY_HELP
       This option instructs the program to invite the user to input a new
       word to the local dictionary on the fly.  This is only active if the
       program is not using an (@) input file!  If an UNKNOWN is discovered,
       the program asks for STEM, PART, and MEAN, the basic elements of a
       dictionary entry.  These are put into the local dictionary right then,
       and are available for the rest of the session, and all later sessions.
       The use of this option requires a detailed knowledge of the structure
       of dictionary entries, and is not for the average user.  If the entry
       is not valid, reloading the dictionary will raise and exception, and
       the invalid entry will be rejected, but the program will continue
       without that word.  Any invalid entries can be corrected or deleted
       off-line with a text editor on the local dictionary file.  If one does
       not want to enter a word when this option is on, a simple RETURN at
       the STEM=> prompt will ignore and continue the program.  This option
       is only for very experienced users and should normally be off.
                                                 The default is N(o).
             ------    NOT AVAILABLE IN THIS VERSION   -------

    UPDATE_MEANINGS_HELP
       This option instructs the program to invite the user to modify the
       meaning displayed on a word translation.  This is only active if the
       program is not using an (@) input file!  These changes are put into
       the dictionary right then and permanently, and are available from
       then on, in this session, and all later sessions.   Unfortunately,
       these changes will not survive the replacement of the dictionary by a
       new version from the developer.  Changes can only be recovered by
       considerable processing by the developer, and should be left there.
       This option is only for experienced users and should remain off.
                                                 The default is N(o).
             ------    NOT AVAILABLE IN THIS VERSION   -------

    MINIMIZE_OUTPUT_HELP
       This option instructs the program to minimize the output.  This is a
       somewhat flexible term, but the use of this option will probably lead
       to less output.                        The default is Y(es).

    SAVE_PARAMETERS_HELP
       This option instructs the program, to save the current parameters, as
       just established by the user, in a file WORD.MDV.  If such a file
       exists, the program will load those parameters at the start.  If no
       such file can be found in the current subdirectory, the program will
       start with a default set of parameters.  Since this parameter file is
       human-readable ASCII, it may also be created with a text editor.  If
       the file found has been improperly created, is in the wrong format, or
       otherwise uninterpretable by the program, it will be ignored and the
       default parameters used, until a proper parameter file in written by
       the program.  Since one may want to make temporary changes during a
       run, but revert to the usual set, the default is N(o).


## Special Cases

Some adjectives have no conventional positive forms (either missing or
undeclined), or the POS forms have more than one COMP/SUPER.  In these few
cases, the individual COMP or SUPER form is entered separately.  Since it
is not directly connected with a POS form, and only the POS forms have
different numbered declensions, the special form is given a declension of
(0, 0).  An additional consequence is that the dictionary form in output
is only for the COMP/SUPER, and does not reflect all comparisons.

## Uniques

There are some irregular situations which are not convenient to handle
through the general algorithms.  For these a UNIQUES file and procedure
was established.  The number of these special cases is less than one
hundred, but may increase as new situations arise, and decrease as
algorithms provide better coverage.  The user will not see much
difference, except in that no dictionary forms are available for these
unique words.

## Tricks

There are a number of situations in Latin writing where certain
modifications or conventions regularly are found.  While often found,
these are not the normal classical forms.  If a conventional match is not
found, the program may be instructed to TRY_TRICKS.  Below is a partial
list of current tricks.  The syncopated form of the perfect often drops
the 'v' and loses the vowel.  An initial 'a' followed by a double letter
often is used for an 'ad' prefix, likewise an initial 'ad' prefix is often
replaced by an 'a' followed by a double letter.  An initial 'i' followed
by a double letter often is used for an 'in' prefix, likewise an initial
'in' prefix is often replaced by an 'i' followed by a double letter.  A
leading 'inp' could be an 'imp'.  A leading 'obt' could be an 'opt'.  An
initial 'har...' or 'hal...' may be rendered by an 'ar' or 'al', likewise
the dictionary entry may have 'ar'/'al' and the trial word begin with
'ha...'.  An initial 'c' could be a 'k', or the dictionary entry uses 'c'
for 'k'.  A nonterminal 'ae' is often rendered by an 'e'.  An initial 'E'
can replace an 'Ae'.  An 'iis...' beginning some forms of 'eo' may be
contracted to 'is...'.  A nonterminal 'ii' is often replaced by just 'i';
including 'ji', since in this program and dictionary all 'j' are made 'i'.
A 'cl' could be a 'cul'.  A 'vul' could be a 'vol'.  and many others,
including a procedure to try to break the input word into two.

Various manipulations of 'u' and 'v' are possible: 'v' could be replaced
by 'u', like the new Oxford Latin Dictionary, leading 'U' could be
replaced by 'V', checking capitalization, all 'U's could have been
replaced by 'V', like stone cutting.  Previous versions had various
kludges attempting to calculate the correct interpretation.  They were
surprisingly good, but philosophically baseless and certainly failed in a
number of cases.  The present version simply considers 'u' and 'v' as the
same letter in parsing the word.  However, the dictionary entries make the
distinction and this is reflected in the output.

Various combinations of these tricks are attempted, and each try that
results in a possible hit is run against the full dictionary, which can
make these efforts time consuming.  That is a good reason to make the
dictionary as large as possible, rather than counting on a smaller number
of roots and doing the maximum word formation.

Finally, while the program could succeed on a word that requires two or
three of these tricks to work in combination, there are limits.  Some
words for which all the modifications are supported will fail, if there
are just too many.  In fact, it is probably better that that be the case,
otherwise one will generate too many false positives.  Testing so far does
not seem to show excessive zeal on the part of the program, but the user
should examine the results, especially when several tricks are involved.

There is a basic conflict here.  At the state of the 1.97E dictionary there
are so few words that both fail the main program and are caught by tricks
that this option could be defaulted to No.  However, one could argue that
there will be very few occasions for trying TRICKS, so that the cost is
minimal.  Unfortunately the degree of completeness of the dictionary for
classical latin does not carry over to medieval Latin.  With the hope that
the program will become more useful in that area, the default has been
set to Yes, reflecting the philosophy early in the development
for classical Latin.

## Trimming of uncommon results

Trimming has an impact on output.  If TRIM_OUTPUT parameter is set, and
specific parameters set in the MDEV, the program will deprecate those
possible forms which come from archaic or medieval (non-classical) stems
or inflections, also stems or inflections which are relatively uncommon.
It will report such if no classical/common solutions are found.  The
default is set for this, expecting that most users are students and
unlikely to encounter rare forms.  Other users can set the parameters
appropriately for their situation.

This capability is preliminary.  It is just becoming useful in that the
factors are set for about half the dictionary entries.  There are still a
large number of entries and inflections that are not set and will continue
to be reported until determination of rarity is made.
