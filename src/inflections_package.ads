-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936–2010)
--
-- This is a free program, which means it is proper to copy it and pass
-- it on to your friends. Consider it a developmental item for which
-- there is no charge. However, just for form, it is Copyrighted
-- (c). Permission is hereby freely given for any and all use of program
-- and data. You can sell it as your own, but at least tell me.
-- 
-- This version is distributed without obligation, but the developer
-- would appreciate comments and suggestions.
-- 
-- All parts of the WORDS system, source code and data files, are made freely
-- available to anyone who wishes to use them, for whatever purpose.

with text_io; use text_io;
with direct_io;
package inflections_package is
   pragma elaborate_body;
   package integer_io is new text_io.integer_io(integer);

   --  Generally simple/enumeration types have names ending in _TYPE
   --            complex/record     types have names ending in _RECORD
   --            array              types have names ending in _ARRAY

   max_stem_size    : constant := 18;
   max_meaning_size : constant := 80;

   subtype stem_type is string(1..max_stem_size);
   null_stem_type : constant stem_type := (others => ' ');

   package stem_type_io is
      default_width : natural := max_stem_size;
      procedure get(f : in file_type; d : out stem_type);
      procedure get(d : out stem_type);
      procedure put(f : in file_type; d : in stem_type);
      procedure put(d : in stem_type);
      procedure get(s : in string; d : out stem_type;
                                   last : out integer);
      procedure put(s : out string; d : in stem_type);
   end stem_type_io;

   subtype meaning_type is string(1..max_meaning_size);
   null_meaning_type : constant meaning_type := (others => ' ');

   type part_of_speech_type is (
     x,         --  all, none, or unknown
     n,         --  Noun
     pron,      --  PRONoun
     pack,      --  PACKON -- artificial for code
     adj,       --  ADJective
     num,       --  NUMeral
     adv,       --  ADVerb
     v,         --  Verb
     vpar,      --  Verb PARticiple
     supine,    --  SUPINE
     prep,      --  PREPosition
     conj,      --  CONJunction
     interj,    --  INTERJection
     
     -- keep tackon/prefix/suffix together, as they are used in range queries
     tackon,    --  TACKON --  artificial for code
     prefix,    --  PREFIX --  here artificial for code
     suffix     --  SUFFIX --  here artificial for code
                               );

   package part_of_speech_type_io is
      new text_io.enumeration_io(part_of_speech_type);

   subtype which_type is natural range 0..9;

   subtype variant_type is natural range 0..9;

   which_type_io_default_width : integer := 1;
   variant_type_io_default_width : integer := 1;

   type decn_record is
      record
         which        : which_type := 0;
         var          : variant_type := 0;
      end record;

   function "<" (left, right : decn_record) return boolean;

   package decn_record_io is
      default_width : natural;
      procedure get(f : in file_type; d : out decn_record);
      procedure get(d : out decn_record);
      procedure put(f : in file_type; d : in decn_record);
      procedure put(d : in decn_record);
      procedure get(s : in string; d : out decn_record;
                                   last : out integer);
      procedure put(s : out string; d : in decn_record);
   end decn_record_io;

   type gender_type is (
                        x,         --  all, none, or unknown
                        m,         --  Masculine
                        f,         --  Feminine
                        n,         --  Neuter
                        c          --  Common (masculine and/or feminine)
                       );

   package gender_type_io is new text_io.enumeration_io(gender_type);

   type case_type is (
                      x,         --  all, none, or unknown
                      nom,       --  NOMinative
                      voc,       --  VOCative
                      gen,       --  GENitive
                      loc,       --  LOCative
                      dat,       --  DATive
                      abl,       --  ABLative
                      acc        --  ACCusitive
                     );

   package case_type_io is new text_io.enumeration_io(case_type);

   type number_type is (
                        x,         --  all, none, or unknown
                        s,         --  Singular
                        p          --  Plural
                       );

   package number_type_io is new text_io.enumeration_io(number_type);

   type person_type is range 0..3;
   package person_type_io is new text_io.integer_io(person_type);

   type comparison_type is (
                            x,         --  all, none, or unknown
                            pos,       --  POSitive
                            comp,      --  COMParative
                            super      --  SUPERlative
                           );

   package comparison_type_io is new text_io.enumeration_io(comparison_type);

   type stem_key_type is new natural range 0..9;

   package stem_key_type_io is new text_io.integer_io(stem_key_type);
   stem_key_type_io_default_width : integer := 1;

   type numeral_sort_type is (
                              x,          --  all, none, or unknown
                              card,       --  CARDinal
                              ord,        --  ORDinal
                              dist,       --  DISTributive
                              adverb      --  numeral ADVERB
                             );

   package numeral_sort_type_io is
      new text_io.enumeration_io(numeral_sort_type);

   type tense_type is (
                       x,         --  all, none, or unknown
                       pres,      --  PRESent
                       impf,      --  IMPerFect
                       fut,       --  FUTure
                       perf,      --  PERFect
                       plup,      --  PLUPerfect
                       futp       --  FUTure Perfect
                      );

   package tense_type_io is new text_io.enumeration_io(tense_type);

   type voice_type is (
                       x,         --  all, none, or unknown
                       active,    --  ACTIVE
                       passive    --  PASSIVE
                      );

   package voice_type_io is new text_io.enumeration_io(voice_type);

   type mood_type is (
                      x,         --  all, none, or unknown
                      ind,       --  INDicative
                      sub,       --  SUBjunctive
                      imp,       --  IMPerative
                      inf,       --  INFinative
                      ppl        --  ParticiPLe
                     );

   package mood_type_io is new text_io.enumeration_io(mood_type);

   type tense_voice_mood_record is
      record
         tense : tense_type := x;
         voice : voice_type := x;
         mood  : mood_type  := x;
      end record;

   package tense_voice_mood_record_io is
      default_width : natural;
      procedure get(f : in file_type; t : out tense_voice_mood_record);
      procedure get(t : out tense_voice_mood_record);
      procedure put(f : in file_type; t : in tense_voice_mood_record);
      procedure put(t : in tense_voice_mood_record);
      procedure get(s : in string; t : out tense_voice_mood_record;
                                   last : out integer);
      procedure put(s : out string; t : in tense_voice_mood_record);
   end tense_voice_mood_record_io;

   type noun_kind_type is (
                           x,            --  unknown, nondescript
                           s,            --  Singular "only"           --  not really used
                           m,            --  plural or Multiple "only" --  not really used
                           a,            --  Abstract idea
                           g,            --  Group/collective Name -- Roman(s)
                           n,            --  proper Name
                           p,            --  a Person
                           t,            --  a Thing
                           l,            --  Locale, name of country/city
                           w             --  a place Where
                          );

   package noun_kind_type_io is new text_io.enumeration_io(noun_kind_type);

   type pronoun_kind_type is (
                              x,            --  unknown, nondescript
                              pers,         --  PERSonal
                              rel,          --  RELative
                              reflex,       --  REFLEXive
                              demons,       --  DEMONStrative
                              interr,       --  INTERRogative
                              indef,        --  INDEFinite
                              adject        --  ADJECTival
                             );

   package pronoun_kind_type_io is
      new text_io.enumeration_io(pronoun_kind_type);

   subtype numeral_value_type is natural range 0..1000;

   numeral_value_type_io_default_width : integer := 5;

   type verb_kind_type is (
                           x,         --  all, none, or unknown
                           to_be,     --  only the verb TO BE (esse)
                           to_being,  --  compounds of the verb to be (esse)
                           gen,       --  verb taking the GENitive
                           dat,       --  verb taking the DATive
                           abl,       --  verb taking the ABLative
                           trans,     --  TRANSitive verb
                           intrans,   --  INTRANSitive verb
                           impers,    --  IMPERSonal verb (implied subject 'it', 'they', 'God')
                           --  agent implied in action, subject in predicate
                           dep,       --  DEPonent verb
                           --  only passive form but with active meaning
                           semidep,   --  SEMIDEPonent verb (forms perfect as deponent)
                           --  (perfect passive has active force)
                           perfdef    --  PERFect DEFinite verb
                             --  having only perfect stem, but with present force
                          );

   package verb_kind_type_io is
      new text_io.enumeration_io(verb_kind_type);

   type noun_record is
      record
         decl        : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
      end record;

   package noun_record_io is
      default_width : natural;
      procedure get(f : in file_type; n : out noun_record);
      procedure get(n : out noun_record);
      procedure put(f : in file_type; n : in noun_record);
      procedure put(n : in noun_record);
      procedure get(s : in string; n : out noun_record; last : out integer);
      procedure put(s : out string; n : in noun_record);
   end noun_record_io;

   type pronoun_record is
      record
         decl        : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
      end record;

   package pronoun_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out pronoun_record);
      procedure get(p : out pronoun_record);
      procedure put(f : in file_type; p : in pronoun_record);
      procedure put(p : in pronoun_record);
      procedure get(s : in string; p : out pronoun_record; last : out integer);
      procedure put(s : out string; p : in pronoun_record);
   end pronoun_record_io;

   type propack_record is
      record
         decl        : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
      end record;

   package propack_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out propack_record);
      procedure get(p : out propack_record);
      procedure put(f : in file_type; p : in propack_record);
      procedure put(p : in propack_record);
      procedure get(s : in string; p : out propack_record; last : out integer);
      procedure put(s : out string; p : in propack_record);
   end propack_record_io;

   type adjective_record is
      record
         decl        : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
         co          : comparison_type := x;
      end record;

   package adjective_record_io is
      default_width : natural;
      procedure get(f : in file_type; a : out adjective_record);
      procedure get(a : out adjective_record);
      procedure put(f : in file_type; a : in adjective_record);
      procedure put(a : in adjective_record);
      procedure get(s : in string; a : out adjective_record; last : out integer);
      procedure put(s : out string; a : in adjective_record);
   end adjective_record_io;

   type numeral_record is
      record
         decl        : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
         sort        : numeral_sort_type := x;
      end record;

   package numeral_record_io is
      default_width : natural;
      procedure get(f : in file_type; num : out numeral_record);
      procedure get(num : out numeral_record);
      procedure put(f : in file_type; num : in numeral_record);
      procedure put(num : in numeral_record);
      procedure get(s : in string; num : out numeral_record; last : out integer);
      procedure put(s : out string; num : in numeral_record);
   end numeral_record_io;

   type adverb_record is
      record
         co   : comparison_type := x;
      end record;

   package adverb_record_io is
      default_width : natural;
      procedure get(f : in file_type; a : out adverb_record);
      procedure get(a : out adverb_record);
      procedure put(f : in file_type; a : in adverb_record);
      procedure put(a : in adverb_record);
      procedure get(s : in string; a : out adverb_record; last : out integer);
      procedure put(s : out string; a : in adverb_record);
   end adverb_record_io;

   type verb_record is
      record
         con         : decn_record;
         tense_voice_mood  : tense_voice_mood_record;
         person      : person_type := 0;
         number      : number_type := x;
      end record;

   package verb_record_io is
      default_width : natural;
      procedure get(f : in file_type; v : out verb_record);
      procedure get(v : out verb_record);
      procedure put(f : in file_type; v : in verb_record);
      procedure put(v : in verb_record);
      procedure get(s : in string; v : out verb_record; last : out integer);
      procedure put(s : out string; v : in verb_record);
   end verb_record_io;

   type vpar_record is
      record
         con         : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
         tense_voice_mood  : tense_voice_mood_record;
      end record;

   package vpar_record_io is
      default_width : natural;
      procedure get(f : in file_type; vp : out vpar_record);
      procedure get(vp : out vpar_record);
      procedure put(f : in file_type; vp : in vpar_record);
      procedure put(vp : in vpar_record);
      procedure get(s : in string; vp : out vpar_record; last : out integer);
      procedure put(s : out string; vp : in vpar_record);
   end vpar_record_io;

   type supine_record is
      record
         con         : decn_record;
         cs          : case_type := x;
         number      : number_type := x;
         gender      : gender_type := x;
      end record;

   package supine_record_io is
      default_width : natural;
      procedure get(f : in file_type; vp : out supine_record);
      procedure get(vp : out supine_record);
      procedure put(f : in file_type; vp : in supine_record);
      procedure put(vp : in supine_record);
      procedure get(s : in string; vp : out supine_record; last : out integer);
      procedure put(s : out string; vp : in supine_record);
   end supine_record_io;

   type preposition_record is
      record
         obj : case_type := x;
      end record;

   package preposition_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out preposition_record);
      procedure get(p : out preposition_record);
      procedure put(f : in file_type; p : in preposition_record);
      procedure put(p : in preposition_record);
      procedure get(s : in string; p : out preposition_record; last : out integer);
      procedure put(s : out string; p : in preposition_record);
   end preposition_record_io;

   type conjunction_record is
      record
         null;
      end record;

   package conjunction_record_io is
      default_width : natural;
      procedure get(f : in file_type; c : out conjunction_record);
      procedure get(c : out conjunction_record);
      procedure put(f : in file_type; c : in conjunction_record);
      procedure put(c : in conjunction_record);
      procedure get(s : in string; c : out conjunction_record; last : out integer);
      procedure put(s : out string; c : in conjunction_record);
   end conjunction_record_io;

   type interjection_record is
      record
         null;
      end record;

   package interjection_record_io is
      default_width : natural;
      procedure get(f : in file_type; i : out interjection_record);
      procedure get(i : out interjection_record);
      procedure put(f : in file_type; i : in interjection_record);
      procedure put(i : in interjection_record);
      procedure get(s : in string; i : out interjection_record; last : out integer);
      procedure put(s : out string; i : in interjection_record);
   end interjection_record_io;

   --  TACKON, PREFIX, SUFFIX are nulls put in to allow easy printing later

   type tackon_record is
      record
         null;
      end record;

   null_tackon_record : tackon_record;

   package tackon_record_io is
      default_width : natural;
      procedure get(f : in file_type; i : out tackon_record);
      procedure get(i : out tackon_record);
      procedure put(f : in file_type; i : in tackon_record);
      procedure put(i : in tackon_record);
      procedure get(s : in string; i : out tackon_record; last : out integer);
      procedure put(s : out string; i : in tackon_record);
   end tackon_record_io;

   type prefix_record is
      record
         null;
      end record;

   null_prefix_record : prefix_record;

   package prefix_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out prefix_record);
      procedure get(p : out prefix_record);
      procedure put(f : in file_type; p : in prefix_record);
      procedure put(p : in prefix_record);
      procedure get(s : in string; p : out prefix_record; last : out integer);
      procedure put(s : out string; p : in prefix_record);
   end prefix_record_io;

   type suffix_record is
      record
         null;
      end record;

   null_suffix_record : suffix_record;

   package suffix_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out suffix_record);
      procedure get(p : out suffix_record);
      procedure put(f : in file_type; p : in suffix_record);
      procedure put(p : in suffix_record);
      procedure get(s : in string; p : out suffix_record; last : out integer);
      procedure put(s : out string; p : in suffix_record);
   end suffix_record_io;

   type quality_record(pofs : part_of_speech_type := x) is
      record
         case pofs is
            when n =>
               n : noun_record;
            when pron =>
               pron : pronoun_record;
            when pack =>
               pack : propack_record;
            when adj =>
               adj : adjective_record;
            when num =>
               num : numeral_record;
            when adv =>
               adv : adverb_record;
            when v =>
               v : verb_record;
            when vpar =>
               vpar : vpar_record;
            when supine =>
               supine : supine_record;
            when prep =>
               prep : preposition_record;
            when conj =>
               conj : conjunction_record;
            when interj =>
               interj : interjection_record;
            when tackon =>
               tackon : tackon_record;
            when prefix =>
               prefix : prefix_record;
            when suffix =>
               suffix : suffix_record;
            when others =>
               null;
         end case;
      end record;

   null_quality_record : quality_record;

   function "<" (left, right : quality_record) return boolean;

   package quality_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out quality_record);
      procedure get(p : out quality_record);
      procedure put(f : in file_type; p : in quality_record);
      procedure put(p : in quality_record);
      procedure get(s : in string; p : out quality_record; last : out integer);
      procedure put(s : out string; p : in quality_record);
   end quality_record_io;

   type quality_array is array (integer range <>) of quality_record;

   max_ending_size : constant := 7;
   subtype ending_size_type is integer range 0..max_ending_size;

   ending_size_type_io_default_width : integer := 3;

   subtype ending is string(1..max_ending_size);

   type ending_record is
      record
         size : ending_size_type := 0;
         suf  : ending := (others => ' ');
      end record;

   package ending_record_io is
      default_width : natural;
      procedure get(f : in file_type; x : out ending_record);
      procedure get(x : out ending_record);
      procedure put(f : in file_type; x : in ending_record);
      procedure put(x : in ending_record);
      procedure get(s : in string; x : out ending_record; last : out integer);
      procedure put(s : out string; x : in ending_record);
   end ending_record_io;

   null_ending_record : ending_record;

   type age_type is (
                     x,   --              --  In use throughout the ages/unknown -- the default
                     a,   --  archaic     --  Very early forms, obsolete by classical times
                     b,   --  early       --  Early Latin, pre-classical, used for effect/poetry
                     c,   --  classical   --  Limited to classical (~150 BC - 200 AD)
                     d,   --  late        --  Late, post-classical (3rd-5th centuries)
                     e,   --  later       --  Latin not in use in Classical times (6-10), Christian
                     f,   --  medieval    --  Medieval (11th-15th centuries)
                     g,   --  scholar     --  Latin post 15th - Scholarly/Scientific   (16-18)
                     h    --  modern      --  Coined recently, words for new things (19-20)
                    );
   package age_type_io is new text_io.enumeration_io(age_type);

   type frequency_type is (     --  For dictionary entries
                                x,    --              --  Unknown or unspecified
                                a,    --  very freq   --  Very frequent, in all Elementry Latin books
                                b,    --  frequent    --  Frequent, in top 10 percent
                                c,    --  common      --  For Dictionary, in top 10,000 words
                                d,    --  lesser      --  For Dictionary, in top 20,000 words
                                e,    --  uncommon    --  2 or 3 citations
                                f,    --  very rare   --  Having only single citation in OLD or L+S
                                i,    --  inscription --  Only citation is inscription
                                m,    --  graffiti    --  Presently not much used
                                n     --  Pliny       --  Things that appear (almost) only in Pliny Natural History
                          );

   --  For inflections, the same type is used with different weights
   --  X,    --              --  Unknown or unspecified
   --  A,    --  most freq   --  Very frequent, the most common
   --  B,    --  sometimes   --  sometimes, a not unusual VARIANT
   --  C,    --  uncommon    --  occasionally seen
   --  D,    --  infrequent  --  recognizable variant, but unlikely
   --  E,    --  rare        --  for a few cases, very unlikely
   --  F,    --  very rare   --  singular examples,
   --  I,    --  inscription --  Only citation is inscription
   --  M,    --              --  Presently not used
   --  N     --              --  Presently not used

   package frequency_type_io is new text_io.enumeration_io(frequency_type);

   type inflection_record is
      record
         qual   : quality_record   := null_quality_record;
         key    : stem_key_type := 0;
         ending : ending_record := null_ending_record;
         age    : age_type      := x;
         freq   : frequency_type      := x;
      end record;

   null_inflection_record : inflection_record;

   package inflection_record_io is
      default_width : natural;
      procedure get(f : in file_type; p : out inflection_record);
      procedure get(p : out inflection_record);
      procedure put(f : in file_type; p : in inflection_record);
      procedure put(p : in inflection_record);
      procedure get(s : in string; p : out inflection_record; last : out integer);
      procedure put(s : out string; p : in inflection_record);
   end inflection_record_io;

   --  This implies a knowledge of the inflections last character
   subtype inflections_section_1 is character range 'a'..'i';
   subtype inflections_section_2 is character range 'm'..'r';
   subtype inflections_section_3 is character range 's'..'s';
   subtype inflections_section_4 is character range 't'..'u';

   size_of_blank_inflections   : constant integer :=  80;    --  ############
   size_of_inflections_section : constant integer := 570;    --  ############

   type inflection_array is array (positive range <>) of inflection_record;
   subtype lel_section is inflection_array(1..size_of_inflections_section);
   package lel_section_io is new direct_io(lel_section);

   bel : inflection_array(1..size_of_blank_inflections);

   lel : lel_section;

   type inflection_array_index is array (integer range <>,
                                         character range <>) of integer;

   belf, bell : inflection_array_index(0..0, ' '..' ') := (0 => (others => 0));
   lelf, lell : inflection_array_index(1..max_ending_size,
                                       'a'..'z') := (others => (others => 0));
   pelf, pell : inflection_array_index(1..max_ending_size,
                                       'a'..'z') := (others => (others => 0));

   number_of_inflections : integer := 0;

   procedure establish_inflections_section;

   --  <=   means for this purpose "contained in"

   function "<=" (left, right : part_of_speech_type) return boolean;
   function "<=" (left, right : decn_record) return boolean;
   function "<=" (left, right : gender_type) return boolean;
   function "<=" (left, right : case_type)   return boolean;
   function "<=" (left, right : number_type) return boolean;
   function "<=" (left, right : person_type) return boolean;
   function "<=" (left, right : comparison_type) return boolean;
   function "<=" (left, right : tense_voice_mood_record)  return boolean;
   function "<=" (left, right : noun_kind_type)   return boolean;
   function "<=" (left, right : pronoun_kind_type)   return boolean;
   function "<=" (left, right : stem_key_type)   return boolean;  -- not verbs
   function "<=" (left, right : age_type)   return boolean;
   function "<=" (left, right : frequency_type)   return boolean;

   give_up : exception;

end inflections_package;
