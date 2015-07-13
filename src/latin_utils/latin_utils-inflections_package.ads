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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Direct_IO;
package Latin_Utils.Inflections_Package is

   ---------------------------------------------------------------------------

   pragma Elaborate_Body;
   package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);

   ---------------------------------------------------------------------------
   --  Generally simple/enumeration types have names ending in _TYPE
   --            complex/record     types have names ending in _RECORD
   --            array              types have names ending in _ARRAY

   Max_Stem_Size    : constant := 18;
   Max_Meaning_Size : constant := 80;

   subtype Stem_Type is String (1 .. Max_Stem_Size);
   Null_Stem_Type : constant Stem_Type := (others => ' ');

   ---------------------------------------------------------------------------
   -- FIXME: These subprograms don't check if Is_Open (File)
   package Stem_Type_IO is
      Default_Width : constant Natural := Max_Stem_Size;
      procedure Get (File : in File_Type; Item : out Stem_Type);
      procedure Get (Item : out Stem_Type);
      procedure Put (File : in File_Type; Item : in Stem_Type);
      procedure Put (Item : in Stem_Type);
      -- TODO: Document meaning of Last
      procedure Get
        ( Source : in  String;
          Target : out Stem_Type;
          Last   : out Integer
        );
      procedure Put (Target : out String; Item : in Stem_Type);
   end Stem_Type_IO;

   ---------------------------------------------------------------------------

   subtype Meaning_Type is String (1 .. Max_Meaning_Size);
   Null_Meaning_Type : constant Meaning_Type := (others => ' ');

   type Part_Of_Speech_Type is
     ( X,         --  all, none, or unknown
     N,         --  Noun
     Pron,      --  PRONoun
     Pack,      --  PACKON -- artificial for code
     Adj,       --  ADJective
     Num,       --  NUMeral
     Adv,       --  ADVerb
     V,         --  Verb
     Vpar,      --  Verb PARticiple
     Supine,    --  SUPINE
     Prep,      --  PREPosition
     Conj,      --  CONJunction
     Interj,    --  INTERJection
                -- keep tackon/prefix/suffix together, as they are used in range queries
     Tackon,    --  TACKON --  artificial for code
     Prefix,    --  PREFIX --  here artificial for code
     Suffix     --  SUFFIX --  here artificial for code
     );

   package Part_Of_Speech_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Part_Of_Speech_Type);

   ---------------------------------------------------------------------------

   subtype Which_Type   is Natural range 0 .. 9;
   subtype Variant_Type is Natural range 0 .. 9;

   Which_Type_IO_Default_Width   : Integer := 1;
   Variant_Type_IO_Default_Width : Integer := 1;

   ---------------------------------------------------------------------------

   type Decn_Record is
      record
         Which        : Which_Type   := 0;
         Var          : Variant_Type := 0;
      end record;

   function "<" (Left, Right : Decn_Record) return Boolean;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Decn_Record_IO is
      Default_Width : Natural;
      procedure Get (File : in File_Type; Item : out Decn_Record);
      procedure Get (Item : out Decn_Record);
      procedure Put (File : in File_Type; Item : in Decn_Record);
      procedure Put (Item : in Decn_Record);
      -- TODO: Document meaning of Last
      procedure Get
        ( Source : in String;
          Target : out Decn_Record;
          Last   : out Integer
        );
      procedure Put (Target : out String; Item : in Decn_Record);
   end Decn_Record_IO;

   ---------------------------------------------------------------------------

   type Gender_Type is
     ( X, --  all, none, or unknown
     M, --  Masculine
     F, --  Feminine
     N, --  Neuter
     C  --  Common (masculine and/or feminine)
     );

   package Gender_Type_IO is new Ada.Text_IO.Enumeration_IO (Gender_Type);

   ---------------------------------------------------------------------------

   type Case_Type is
     ( X,   --  all, none, or unknown
     Nom, --  NOMinative
     Voc, --  VOCative
     Gen, --  GENitive
     Loc, --  LOCative
     Dat, --  DATive
     Abl, --  ABLative
     Acc  --  ACCusitive
     );

   package Case_Type_IO is new Ada.Text_IO.Enumeration_IO (Case_Type);

   ---------------------------------------------------------------------------

   type Number_Type is
     ( X, --  all, none, or unknown
     S, --  Singular
     P  --  Plural
     );

   package Number_Type_IO is new Ada.Text_IO.Enumeration_IO (Number_Type);

   ---------------------------------------------------------------------------

   type Person_Type is range 0 .. 3;
   package Person_Type_IO is new Ada.Text_IO.Integer_IO (Person_Type);

   ---------------------------------------------------------------------------

   type Comparison_Type is
     ( X,    --  all, none, or unknown
     Pos,  --  POSitive
     Comp, --  COMParative
     Super --  SUPERlative
     );

   package Comparison_Type_IO is new
     Ada.Text_IO.Enumeration_IO (Comparison_Type);

   ---------------------------------------------------------------------------

   type Stem_Key_Type is new Natural range 0 .. 9;

   package Stem_Key_Type_IO is new Ada.Text_IO.Integer_IO (Stem_Key_Type);
   Stem_Key_Type_IO_Default_Width : constant Integer := 1;

   ---------------------------------------------------------------------------

   type Numeral_Sort_Type is
     ( X,     --  all, none, or unknown
     Card,  --  CARDinal
     Ord,   --  ORDinal
     Dist,  --  DISTributive
     Adverb --  numeral ADVERB
     );

   package Numeral_Sort_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Numeral_Sort_Type);

   ---------------------------------------------------------------------------

   type Tense_Type is
     ( X,    --  all, none, or unknown
     Pres, --  PRESent
     Impf, --  IMPerFect
     Fut,  --  FUTure
     Perf, --  PERFect
     Plup, --  PLUPerfect
     Futp  --  FUTure Perfect
     );

   package Tense_Type_IO is new Ada.Text_IO.Enumeration_IO (Tense_Type);

   ---------------------------------------------------------------------------

   type Voice_Type is
     ( X,      --  all, none, or unknown
     Active, --  ACTIVE
     Passive --  PASSIVE
     );

   package Voice_Type_IO is new Ada.Text_IO.Enumeration_IO (Voice_Type);

   ---------------------------------------------------------------------------

   type Mood_Type is
     ( X,         --  all, none, or unknown
     Ind,       --  INDicative
     Sub,       --  SUBjunctive
     Imp,       --  IMPerative
     Inf,       --  INFinative
     Ppl        --  ParticiPLe
     );

   package Mood_Type_IO is new Ada.Text_IO.Enumeration_IO (Mood_Type);

   ---------------------------------------------------------------------------

   type tense_voice_mood_record is
      record
         tense : Tense_Type := X;
         voice : Voice_Type := X;
         mood  : Mood_Type  := X;
      end record;

   package tense_voice_mood_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; t : out tense_voice_mood_record);
      procedure Get (t : out tense_voice_mood_record);
      procedure Put (f : in File_Type; t : in tense_voice_mood_record);
      procedure Put (t : in tense_voice_mood_record);
      procedure Get (s : in String; t : out tense_voice_mood_record;
                                    last : out Integer);
      procedure Put (s : out String; t : in tense_voice_mood_record);
   end tense_voice_mood_record_io;

   type Noun_Kind_Type is (
     x,            --  unknown, nondescript
     s,            --  Singular "only"           --  not really used
     m,            --  plural or Multiple "only" --  not really used
     a,            --  Abstract idea
     g,            --  Group/collective Name -- Roman (s)
     n,            --  proper Name
     p,            --  a Person
     t,            --  a Thing
     l,            --  Locale, name of country/city
     w             --  a place Where
                          );

   package Noun_Kind_Type_IO is new Ada.Text_IO.Enumeration_IO (Noun_Kind_Type);

   type Pronoun_Kind_Type is (
     x,            --  unknown, nondescript
     pers,         --  PERSonal
     rel,          --  RELative
     reflex,       --  REFLEXive
     demons,       --  DEMONStrative
     interr,       --  INTERRogative
     indef,        --  INDEFinite
     adject        --  ADJECTival
                             );

   package Pronoun_Kind_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Pronoun_Kind_Type);

   subtype Numeral_Value_Type is Natural range 0 .. 1000;

   Numeral_Value_Type_IO_Default_Width : Integer := 5;

   type Verb_Kind_Type is (
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

   package Verb_Kind_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Verb_Kind_Type);

   type noun_record is
      record
         decl        : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
      end record;

   package noun_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; n : out noun_record);
      procedure Get (n : out noun_record);
      procedure Put (f : in File_Type; n : in noun_record);
      procedure Put (n : in noun_record);
      procedure Get (s : in String; n : out noun_record; last : out Integer);
      procedure Put (s : out String; n : in noun_record);
   end noun_record_io;

   type pronoun_record is
      record
         decl        : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
      end record;

   package pronoun_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out pronoun_record);
      procedure Get (p : out pronoun_record);
      procedure Put (f : in File_Type; p : in pronoun_record);
      procedure Put (p : in pronoun_record);
      procedure Get (s : in String; p : out pronoun_record; last : out Integer);
      procedure Put (s : out String; p : in pronoun_record);
   end pronoun_record_io;

   type propack_record is
      record
         decl        : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
      end record;

   package propack_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out propack_record);
      procedure Get (p : out propack_record);
      procedure Put (f : in File_Type; p : in propack_record);
      procedure Put (p : in propack_record);
      procedure Get (s : in String; p : out propack_record; last : out Integer);
      procedure Put (s : out String; p : in propack_record);
   end propack_record_io;

   type adjective_record is
      record
         decl        : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
         co          : Comparison_Type := X;
      end record;

   package adjective_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; a : out adjective_record);
      procedure Get (a : out adjective_record);
      procedure Put (f : in File_Type; a : in adjective_record);
      procedure Put (a : in adjective_record);
      procedure Get (s : in String; a : out adjective_record; last : out Integer);
      procedure Put (s : out String; a : in adjective_record);
   end adjective_record_io;

   type numeral_record is
      record
         decl        : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
         sort        : Numeral_Sort_Type := X;
      end record;

   package numeral_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; num : out numeral_record);
      procedure Get (num : out numeral_record);
      procedure Put (f : in File_Type; num : in numeral_record);
      procedure Put (num : in numeral_record);
      procedure Get (s : in String; num : out numeral_record; last : out Integer);
      procedure Put (s : out String; num : in numeral_record);
   end numeral_record_io;

   type adverb_record is
      record
         co   : Comparison_Type := X;
      end record;

   package adverb_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; a : out adverb_record);
      procedure Get (a : out adverb_record);
      procedure Put (f : in File_Type; a : in adverb_record);
      procedure Put (a : in adverb_record);
      procedure Get (s : in String; a : out adverb_record; last : out Integer);
      procedure Put (s : out String; a : in adverb_record);
   end adverb_record_io;

   type verb_record is
      record
         con         : Decn_Record;
         tense_voice_mood  : tense_voice_mood_record;
         person      : Person_Type := 0;
         number      : Number_Type := X;
      end record;

   package verb_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; v : out verb_record);
      procedure Get (v : out verb_record);
      procedure Put (f : in File_Type; v : in verb_record);
      procedure Put (v : in verb_record);
      procedure Get (s : in String; v : out verb_record; last : out Integer);
      procedure Put (s : out String; v : in verb_record);
   end verb_record_io;

   type vpar_record is
      record
         con         : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
         tense_voice_mood  : tense_voice_mood_record;
      end record;

   package vpar_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; vp : out vpar_record);
      procedure Get (vp : out vpar_record);
      procedure Put (f : in File_Type; vp : in vpar_record);
      procedure Put (vp : in vpar_record);
      procedure Get (s : in String; vp : out vpar_record; last : out Integer);
      procedure Put (s : out String; vp : in vpar_record);
   end vpar_record_io;

   type supine_record is
      record
         con         : Decn_Record;
         cs          : Case_Type := X;
         number      : Number_Type := X;
         gender      : Gender_Type := X;
      end record;

   package supine_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; vp : out supine_record);
      procedure Get (vp : out supine_record);
      procedure Put (f : in File_Type; vp : in supine_record);
      procedure Put (vp : in supine_record);
      procedure Get (s : in String; vp : out supine_record; last : out Integer);
      procedure Put (s : out String; vp : in supine_record);
   end supine_record_io;

   type preposition_record is
      record
         obj : Case_Type := X;
      end record;

   package preposition_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out preposition_record);
      procedure Get (p : out preposition_record);
      procedure Put (f : in File_Type; p : in preposition_record);
      procedure Put (p : in preposition_record);
      procedure Get (s : in String; p : out preposition_record; last : out Integer);
      procedure Put (s : out String; p : in preposition_record);
   end preposition_record_io;

   type conjunction_record is
      record
         null;
      end record;

   package conjunction_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; c : out conjunction_record);
      procedure Get (c : out conjunction_record);
      procedure Put (f : in File_Type; c : in conjunction_record);
      procedure Put (c : in conjunction_record);
      procedure Get (s : in String; c : out conjunction_record; last : out Integer);
      procedure Put (s : out String; c : in conjunction_record);
   end conjunction_record_io;

   type interjection_record is
      record
         null;
      end record;

   package interjection_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; i : out interjection_record);
      procedure Get (i : out interjection_record);
      procedure Put (f : in File_Type; i : in interjection_record);
      procedure Put (i : in interjection_record);
      procedure Get (s : in String; i : out interjection_record; last : out Integer);
      procedure Put (s : out String; i : in interjection_record);
   end interjection_record_io;

   --  TACKON, PREFIX, SUFFIX are nulls Put in to allow easy printing later

   type tackon_record is
      record
         null;
      end record;

   null_tackon_record : tackon_record;

   package tackon_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; i : out tackon_record);
      procedure Get (i : out tackon_record);
      procedure Put (f : in File_Type; i : in tackon_record);
      procedure Put (i : in tackon_record);
      procedure Get (s : in String; i : out tackon_record; last : out Integer);
      procedure Put (s : out String; i : in tackon_record);
   end tackon_record_io;

   type prefix_record is
      record
         null;
      end record;

   null_prefix_record : prefix_record;

   package prefix_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out prefix_record);
      procedure Get (p : out prefix_record);
      procedure Put (f : in File_Type; p : in prefix_record);
      procedure Put (p : in prefix_record);
      procedure Get (s : in String; p : out prefix_record; last : out Integer);
      procedure Put (s : out String; p : in prefix_record);
   end prefix_record_io;

   type suffix_record is
      record
         null;
      end record;

   null_suffix_record : suffix_record;

   package suffix_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out suffix_record);
      procedure Get (p : out suffix_record);
      procedure Put (f : in File_Type; p : in suffix_record);
      procedure Put (p : in suffix_record);
      procedure Get (s : in String; p : out suffix_record; last : out Integer);
      procedure Put (s : out String; p : in suffix_record);
   end suffix_record_io;

   type quality_record (pofs : Part_Of_Speech_Type := X) is
      record
         case pofs is
            when N =>
               N : noun_record;
            when Pron =>
               Pron : pronoun_record;
            when Pack =>
               Pack : propack_record;
            when Adj =>
               Adj : adjective_record;
            when Num =>
               Num : numeral_record;
            when Adv =>
               Adv : adverb_record;
            when V =>
               V : verb_record;
            when Vpar =>
               Vpar : vpar_record;
            when Supine =>
               Supine : supine_record;
            when Prep =>
               Prep : preposition_record;
            when Conj =>
               Conj : conjunction_record;
            when Interj =>
               Interj : interjection_record;
            when Tackon =>
               Tackon : tackon_record;
            when Prefix =>
               Prefix : prefix_record;
            when Suffix =>
               Suffix : suffix_record;
            when X =>
               null;
         end case;
      end record;

   null_quality_record : quality_record;

   -- FIXME results in erroneous execution in case of Tackon .. Suffix
   function "<" (left, right : quality_record) return Boolean;

   package quality_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out quality_record);
      procedure Get (p : out quality_record);
      procedure Put (f : in File_Type; p : in quality_record);
      procedure Put (p : in quality_record);
      procedure Get (s : in String; p : out quality_record; last : out Integer);
      procedure Put (s : out String; p : in quality_record);
   end quality_record_io;

   type quality_array is array (Integer range <>) of quality_record;

   max_ending_size : constant := 7;
   subtype ending_size_type is Integer range 0 .. max_ending_size;

   ending_size_type_io_Default_Width : Integer := 3;

   subtype ending is String (1 .. max_ending_size);

   type ending_record is
      record
         size : ending_size_type := 0;
         suf  : ending := (others => ' ');
      end record;

   package ending_record_io is
      Default_Width : Natural;
      procedure Get (f : in File_Type; x : out ending_record);
      procedure Get (x : out ending_record);
      procedure Put (f : in File_Type; x : in ending_record);
      procedure Put (x : in ending_record);
      procedure Get (s : in String; x : out ending_record; last : out Integer);
      procedure Put (s : out String; x : in ending_record);
   end ending_record_io;

   null_ending_record : ending_record;

   type Age_Type is (
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
   package Age_Type_IO is new Ada.Text_IO.Enumeration_IO (Age_Type);

   type Frequency_Type is (     --  For dictionary entries
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

   package Frequency_Type_IO is new Ada.Text_IO.Enumeration_IO (Frequency_Type);

   type Inflection_Record is
      record
         qual   : quality_record   := null_quality_record;
         key    : Stem_Key_Type := 0;
         ending : ending_record := null_ending_record;
         age    : Age_Type      := x;
         freq   : Frequency_Type      := x;
      end record;

   Null_Inflection_Record : Inflection_Record;

   package Inflection_Record_IO is
      Default_Width : Natural;
      procedure Get (f : in File_Type; p : out Inflection_Record);
      procedure Get (p : out Inflection_Record);
      procedure Put (f : in File_Type; p : in Inflection_Record);
      procedure Put (p : in Inflection_Record);
      procedure Get (s : in String; p : out Inflection_Record; last : out Integer);
      procedure Put (s : out String; p : in Inflection_Record);
   end Inflection_Record_IO;

   --  This implies a knowledge of the inflections last Character
   subtype inflections_section_1 is Character range 'a' .. 'i';
   subtype inflections_section_2 is Character range 'm' .. 'r';
   subtype inflections_section_3 is Character range 's' .. 's';
   subtype inflections_section_4 is Character range 't' .. 'u';

   size_of_blank_inflections   : constant Integer :=  80;    --  ############
   size_of_inflections_section : constant Integer := 570;    --  ############

   type inflection_array is array (Positive range <>) of Inflection_Record;
   subtype lel_section is inflection_array (1 .. size_of_inflections_section);
   package lel_section_io is new Ada.Direct_IO (lel_section);

   bel : inflection_array (1 .. size_of_blank_inflections);

   lel : lel_section;

   type inflection_array_index is array (Integer range <>,
     Character range <>) of Integer;

   belf, bell : inflection_array_index (0 .. 0, ' ' .. ' ') := (0 => (others => 0));
   lelf, lell : inflection_array_index (1 .. max_ending_size,
     'a' .. 'z') := (others => (others => 0));
   pelf, pell : inflection_array_index (1 .. max_ending_size,
     'a' .. 'z') := (others => (others => 0));

   number_of_inflections : Integer := 0;

   procedure establish_inflections_section;

   --  <=   means for this purpose "contained in"
   overriding function "<=" (left, right : Part_Of_Speech_Type) return Boolean;
   function "<=" (left, right : Decn_Record) return Boolean;
   overriding function "<=" (left, right : Gender_Type) return Boolean;
   overriding function "<=" (left, right : Case_Type)   return Boolean;
   overriding function "<=" (left, right : Number_Type) return Boolean;
   overriding function "<=" (left, right : Person_Type) return Boolean;
   overriding function "<=" (left, right : Comparison_Type) return Boolean;
   function "<=" (left, right : tense_voice_mood_record)  return Boolean;
   overriding function "<=" (left, right : Noun_Kind_Type)   return Boolean;
   overriding function "<=" (left, right : Pronoun_Kind_Type)   return Boolean;
   overriding function "<=" (left, right : Stem_Key_Type)   return Boolean;  -- not verbs
   overriding function "<=" (left, right : Age_Type)   return Boolean;
   overriding function "<=" (left, right : Frequency_Type)   return Boolean;

   give_up : exception;

end Latin_Utils.Inflections_Package;
