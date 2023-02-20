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

--
-- NOTE: Default_Widths in various nested IO packages are declared as
-- constants due to the fact that modifying Default_Width would break
-- IO routines in given package (and packages using it).
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Direct_IO;
package Latin_Utils.Inflections_Package is

   ---------------------------------------------------------------------------

   pragma Elaborate_Body;

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
        (Source : in  String;
         Target : out Stem_Type;
         Last   : out Integer
        );
      procedure Put (Target : out String; Item : in Stem_Type);
   end Stem_Type_IO;

   ---------------------------------------------------------------------------

   subtype Meaning_Type is String (1 .. Max_Meaning_Size);
   Null_Meaning_Type : constant Meaning_Type := (others => ' ');

   type Part_Of_Speech_Type is
     (X,         --  all, none, or unknown
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

   Which_Type_IO_Default_Width   : constant Integer := 1;
   Variant_Type_IO_Default_Width : constant Integer := 1;

   ---------------------------------------------------------------------------

   type Decn_Record is
      record
         Which        : Which_Type   := 0;
         Var          : Variant_Type := 0;
      end record;

   function "<" (Left, Right : Decn_Record) return Boolean;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Decn_Record_IO is
      Default_Width : constant Natural :=
         Which_Type_IO_Default_Width + 1 +
         Variant_Type_IO_Default_Width;
      procedure Get (File : in File_Type; Item : out Decn_Record);
      procedure Get (Item : out Decn_Record);
      procedure Put (File : in File_Type; Item : in Decn_Record);
      procedure Put (Item : in Decn_Record);
      -- TODO: Document meaning of Last
      procedure Get
        (Source : in String;
          Target : out Decn_Record;
          Last   : out Integer
        );
      procedure Put (Target : out String; Item : in Decn_Record);
   end Decn_Record_IO;

   ---------------------------------------------------------------------------

   type Gender_Type is
     (X, --  all, none, or unknown
     M, --  Masculine
     F, --  Feminine
     N, --  Neuter
     C  --  Common (masculine and/or feminine)
     );

   package Gender_Type_IO is new Ada.Text_IO.Enumeration_IO (Gender_Type);

   ---------------------------------------------------------------------------

   type Case_Type is
     (X,   --  all, none, or unknown
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
     (X, --  all, none, or unknown
     S, --  Singular
     P  --  Plural
     );

   package Number_Type_IO is new Ada.Text_IO.Enumeration_IO (Number_Type);

   ---------------------------------------------------------------------------

   type Person_Type is range 0 .. 3;
   package Person_Type_IO is new Ada.Text_IO.Integer_IO (Person_Type);

   ---------------------------------------------------------------------------

   type Comparison_Type is
     (X,    --  all, none, or unknown
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
     (X,     --  all, none, or unknown
     Card,  --  CARDinal
     Ord,   --  ORDinal
     Dist,  --  DISTributive
     Adverb --  numeral ADVERB
     );

   package Numeral_Sort_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Numeral_Sort_Type);

   ---------------------------------------------------------------------------

   type Tense_Type is
     (X,    --  all, none, or unknown
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
     (X,     --  all, none, or unknown
     Active, --  ACTIVE
     Passive --  PASSIVE
     );

   package Voice_Type_IO is new Ada.Text_IO.Enumeration_IO (Voice_Type);

   ---------------------------------------------------------------------------

   type Mood_Type is
     (X,  --  all, none, or unknown
     Ind, --  INDicative
     Sub, --  SUBjunctive
     Imp, --  IMPerative
     Inf, --  INFinative
     Ppl  --  ParticiPLe
     );

   package Mood_Type_IO is new Ada.Text_IO.Enumeration_IO (Mood_Type);

   ---------------------------------------------------------------------------

   type Tense_Voice_Mood_Record is
      record
         Tense : Tense_Type := X;
         Voice : Voice_Type := X;
         Mood  : Mood_Type  := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Tense_Voice_Mood_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Tense_Type'Width + 1 +
        Voice_Type'Width + 1 +
        Mood_Type'Width;
      procedure Get (File : in  File_Type; Item : out Tense_Voice_Mood_Record);
      procedure Get (Item : out Tense_Voice_Mood_Record);
      procedure Put (File : in  File_Type; Item : in  Tense_Voice_Mood_Record);
      procedure Put (Item : in  Tense_Voice_Mood_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Tense_Voice_Mood_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Tense_Voice_Mood_Record);
   end Tense_Voice_Mood_Record_IO;

   ---------------------------------------------------------------------------

   type Noun_Kind_Type is
      (X, --  unknown, nondescript
      S,  --  Singular "only"           --  not really used
      M,  --  plural or Multiple "only" --  not really used
      A,  --  Abstract idea
      G,  --  Group/collective Name -- Roman(s)
      N,  --  proper Name
      P,  --  a Person
      T,  --  a Thing
      L,  --  Locale, name of country/city
      W   --  a place Where
      );

   package Noun_Kind_Type_IO is new Ada.Text_IO.Enumeration_IO (Noun_Kind_Type);

   ---------------------------------------------------------------------------

   type Pronoun_Kind_Type is
      (X,      --  unknown, nondescript
      Pers,   --  PERSonal
      Rel,    --  RELative
      Reflex, --  REFLEXive
      Demons, --  DEMONStrative
      Interr, --  INTERRogative
      Indef,  --  INDEFinite
      Adject  --  ADJECTival
      );

   package Pronoun_Kind_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Pronoun_Kind_Type);

   ---------------------------------------------------------------------------

   subtype Numeral_Value_Type is Natural range 0 .. 1000;

   Numeral_Value_Type_IO_Default_Width : constant Integer := 5;

   ---------------------------------------------------------------------------

   type Verb_Kind_Type is
      (X,        --  all, none, or unknown
      To_Be,     --  only the verb TO BE (esse)
      To_Being,  --  compounds of the verb to be (esse)
      Gen,       --  verb taking the GENitive
      Dat,       --  verb taking the DATive
      Abl,       --  verb taking the ABLative
      Trans,     --  TRANSitive verb
      Intrans,   --  INTRANSitive verb
      Impers,    --  IMPERSonal verb (implied subject 'it', 'they', 'God')
      --  agent implied in action, subject in predicate
      Dep,       --  DEPonent verb
      --  only passive form but with active meaning
      Semidep,   --  SEMIDEPonent verb (forms perfect as deponent)
      --  (perfect passive has active force)
      Perfdef    --  PERFect DEFinite verb
      --  having only perfect stem, but with present force
      );

   package Verb_Kind_Type_IO is
      new Ada.Text_IO.Enumeration_IO (Verb_Kind_Type);

   ---------------------------------------------------------------------------

   type Noun_Record is
      record
         Decl        : Decn_Record;
         Of_Case     : Case_Type    := X;
         Number      : Number_Type  := X;
         Gender      : Gender_Type  := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Noun_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width;
      procedure Get (File : in  File_Type; Item : out Noun_Record);
      procedure Get (Item : out Noun_Record);
      procedure Put (File : in  File_Type; Item : in  Noun_Record);
      procedure Put (Item : in  Noun_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Noun_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Noun_Record);
   end Noun_Record_IO;

   ---------------------------------------------------------------------------

   type Pronoun_Record is
      record
         Decl        : Decn_Record;
         Of_Case     : Case_Type := X;
         Number      : Number_Type := X;
         Gender      : Gender_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Pronoun_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width;
      procedure Get (File : in  File_Type; Item : out Pronoun_Record);
      procedure Get (Item : out Pronoun_Record);
      procedure Put (File : in  File_Type; Item : in  Pronoun_Record);
      procedure Put (Item : in  Pronoun_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Pronoun_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Pronoun_Record);
   end Pronoun_Record_IO;

   ---------------------------------------------------------------------------

   type Propack_Record is
      record
         Decl        : Decn_Record;
         Of_Case     : Case_Type   := X;
         Number      : Number_Type := X;
         Gender      : Gender_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Propack_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width;
      procedure Get (File : in  File_Type; Item : out Propack_Record);
      procedure Get (Item : out Propack_Record);
      procedure Put (File : in  File_Type; Item : in  Propack_Record);
      procedure Put (Item : in  Propack_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Propack_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Propack_Record);
   end Propack_Record_IO;

   ---------------------------------------------------------------------------

   type Adjective_Record is
      record
         Decl        : Decn_Record;
         Of_Case     : Case_Type := X;
         Number      : Number_Type := X;
         Gender      : Gender_Type := X;
         Comparison  : Comparison_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Adjective_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width + 1 +
        Comparison_Type'Width;
      procedure Get (File : in File_Type; Item : out Adjective_Record);
      procedure Get (Item : out Adjective_Record);
      procedure Put (File : in File_Type; Item : in Adjective_Record);
      procedure Put (Item : in Adjective_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Adjective_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Adjective_Record);
   end Adjective_Record_IO;

   ---------------------------------------------------------------------------

   type Numeral_Record is
      record
         Decl        : Decn_Record;
         Of_Case     : Case_Type := X;
         Number      : Number_Type := X;
         Gender      : Gender_Type := X;
         Sort        : Numeral_Sort_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Numeral_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width + 1 +
        Numeral_Sort_Type'Width;
      procedure Get (File : in  File_Type; Item : out Numeral_Record);
      -- FIXME: This subprogram seems to be incorrect
      procedure Get (Item : out Numeral_Record);
      procedure Put (File : in  File_Type; Item : in  Numeral_Record);
      procedure Put (Item : in  Numeral_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Numeral_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Numeral_Record);
   end Numeral_Record_IO;

   ---------------------------------------------------------------------------

   type Adverb_Record is
      record
         Comparison   : Comparison_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Adverb_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Comparison_Type'Width;
      procedure Get (File : in  File_Type; Item : out Adverb_Record);
      procedure Get (Item : out Adverb_Record);
      procedure Put (File : in  File_Type; Item : in  Adverb_Record);
      procedure Put (Item : in  Adverb_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Adverb_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Adverb_Record);
   end Adverb_Record_IO;

   ---------------------------------------------------------------------------

   type Verb_Record is
      record
         Con         : Decn_Record;
         Tense_Voice_Mood  : Tense_Voice_Mood_Record;
         Person      : Person_Type := 0;
         Number      : Number_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Verb_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Tense_Voice_Mood_Record_IO.Default_Width + 1 +
        Person_Type'Width + 1 +
        Number_Type'Width;
      procedure Get (File : in  File_Type; Item : out Verb_Record);
      procedure Get (Item : out Verb_Record);
      procedure Put (File : in  File_Type; Item : in  Verb_Record);
      procedure Put (Item : in  Verb_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Verb_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Verb_Record);
   end Verb_Record_IO;

   ---------------------------------------------------------------------------

   type Vpar_Record is
      record
         Con         : Decn_Record;
         Of_Case     : Case_Type := X;
         Number      : Number_Type := X;
         Gender      : Gender_Type := X;
         Tense_Voice_Mood  : Tense_Voice_Mood_Record;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Vpar_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width + 1 +
        Tense_Voice_Mood_Record_IO.Default_Width;
      procedure Get (File : in  File_Type; Item : out Vpar_Record);
      procedure Get (Item : out Vpar_Record);
      procedure Put (File : in  File_Type; Item : in  Vpar_Record);
      procedure Put (Item : in  Vpar_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Vpar_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Vpar_Record);
   end Vpar_Record_IO;

   ---------------------------------------------------------------------------

   type Supine_Record is
      record
         Con         : Decn_Record;
         Of_Case     : Case_Type := X;
         Number      : Number_Type := X;
         Gender      : Gender_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Supine_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Decn_Record_IO.Default_Width + 1 +
        Case_Type'Width + 1 +
        Number_Type'Width + 1 +
        Gender_Type'Width;
      procedure Get (File : in  File_Type; Item : out Supine_Record);
      procedure Get (Item : out Supine_Record);
      procedure Put (File : in  File_Type; Item : in  Supine_Record);
      procedure Put (Item : in  Supine_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Supine_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Supine_Record);
   end Supine_Record_IO;

   ---------------------------------------------------------------------------

   type Preposition_Record is
      record
         Of_Case : Case_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Preposition_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := Case_Type'Width;
      procedure Get (File : in  File_Type; Item : out Preposition_Record);
      procedure Get (Item : out Preposition_Record);
      procedure Put (File : in  File_Type; Item : in  Preposition_Record);
      procedure Put (Item : in  Preposition_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Preposition_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Preposition_Record);
   end Preposition_Record_IO;

   ---------------------------------------------------------------------------

   type Conjunction_Record is null record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Conjunction_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := 0;
      procedure Get (File : in  File_Type; Item : out Conjunction_Record);
      procedure Get (Item : out Conjunction_Record);
      procedure Put (File : in  File_Type; Item : in  Conjunction_Record);
      procedure Put (Item : in  Conjunction_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Conjunction_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Conjunction_Record);
   end Conjunction_Record_IO;

   ---------------------------------------------------------------------------

   type Interjection_Record is null record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Interjection_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := 0;
      procedure Get (File : in  File_Type; Item : out Interjection_Record);
      procedure Get (Item : out Interjection_Record);
      procedure Put (File : in  File_Type; Item : in  Interjection_Record);
      procedure Put (Item : in  Interjection_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Interjection_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Interjection_Record);
   end Interjection_Record_IO;

   ---------------------------------------------------------------------------
   --  TACKON, PREFIX, SUFFIX are nulls Put in to allow easy printing later

   type Tackon_Record is null record;

   Null_Tackon_Record : constant Tackon_Record := (null record);

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Tackon_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := 0;
      procedure Get (File : in  File_Type; Item : out Tackon_Record);
      procedure Get (Item : out Tackon_Record);
      procedure Put (File : in  File_Type; Item : in  Tackon_Record);
      procedure Put (Item : in  Tackon_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Tackon_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Tackon_Record);
   end Tackon_Record_IO;

   ---------------------------------------------------------------------------

   type Prefix_Record is null record;

   Null_Prefix_Record : constant Prefix_Record := (null record);

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Prefix_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := 0;
      procedure Get (File : in  File_Type; Item : out Prefix_Record);
      procedure Get (Item : out Prefix_Record);
      procedure Put (File : in  File_Type; Item : in  Prefix_Record);
      procedure Put (Item : in  Prefix_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Prefix_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Prefix_Record);
   end Prefix_Record_IO;

   ---------------------------------------------------------------------------

   type Suffix_Record is null record;

   Null_Suffix_Record : constant Suffix_Record := (null record);

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Suffix_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := 0;
      procedure Get (File : in  File_Type; Item : out Suffix_Record);
      procedure Get (Item : out Suffix_Record);
      procedure Put (File : in  File_Type; Item : in  Suffix_Record);
      procedure Put (Item : in  Suffix_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Suffix_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Suffix_Record);
   end Suffix_Record_IO;

   ---------------------------------------------------------------------------

   type Quality_Record (Pofs : Part_Of_Speech_Type := X) is
      record
         case Pofs is
            when N =>
               Noun : Noun_Record;
            when Pron =>
               Pron : Pronoun_Record;
            when Pack =>
               Pack : Propack_Record;
            when Adj =>
               Adj : Adjective_Record;
            when Num =>
               Num : Numeral_Record;
            when Adv =>
               Adv : Adverb_Record;
            when V =>
               Verb : Verb_Record;
            when Vpar =>
               Vpar : Vpar_Record;
            when Supine =>
               Supine : Supine_Record;
            when Prep =>
               Prep : Preposition_Record;
            when Conj =>
               Conj : Conjunction_Record;
            when Interj =>
               Interj : Interjection_Record;
            when Tackon =>
               Tackon : Tackon_Record;
            when Prefix =>
               Prefix : Prefix_Record;
            when Suffix =>
               Suffix : Suffix_Record;
            when X =>
               null;
         end case;
      end record;

   Null_Quality_Record : constant Quality_Record := Quality_Record'(Pofs => X);

   -- FIXME results in erroneous execution in case of Tackon .. Suffix
   function "<" (Left, Right : Quality_Record) return Boolean;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Quality_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Part_Of_Speech_Type'Width + 1 +
        Vpar_Record_IO.Default_Width; --  Largest
      procedure Get (File : in  File_Type; Item : out Quality_Record);
      procedure Get (Item : out Quality_Record);
      procedure Put (File : in  File_Type; Item : in  Quality_Record);
      procedure Put (Item : in Quality_Record);
      -- TODO: Document meaning of Last
      procedure Get
        (Source : in  String;
         Target : out Quality_Record;
         Last   : out Integer
        );
      procedure Put (Target : out String; Item : in Quality_Record);
   end Quality_Record_IO;

   type Quality_Array is array (Integer range <>) of Quality_Record;

   ---------------------------------------------------------------------------

   Max_Ending_Size : constant := 7;
   subtype Ending_Size_Type is Integer range 0 .. Max_Ending_Size;

   -- FIXME: This probably should be constant.
   Ending_Size_Type_IO_Default_Width : Integer := 3;

   subtype Ending is String (1 .. Max_Ending_Size);

   type Ending_Record is
      record
         Size : Ending_Size_Type := 0;
         Suf  : Ending := (others => ' ');
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Ending_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural := 3 + 1 + Max_Ending_Size;
      procedure Get (File : in  File_Type; Item : out Ending_Record);
      procedure Get (Item : out Ending_Record);
      procedure Put (File : in  File_Type; Item : in  Ending_Record);
      procedure Put (Item : in  Ending_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Ending_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Ending_Record);
   end Ending_Record_IO;

   Null_Ending_Record : constant Ending_Record := (0, (others => ' '));

   ---------------------------------------------------------------------------

   type Age_Type is (
     X,   --              --  In use throughout the ages/unknown -- the default
     A,   --  archaic     --  Very early forms, obsolete by classical times
     B,   --  early       --  Early Latin, pre-classical, used for effect/poetry
     C,   --  classical   --  Limited to classical (~150 BC - 200 AD)
     D,   --  late        --  Late, post-classical (3rd-5th centuries)
     E,   --  later       --  Latin not in use in Classical times (6-10), X'ian
     F,   --  medieval    --  Medieval (11th-15th centuries)
     G,   --  scholar     --  Latin post 15th - Scholarly/Scientific   (16-18)
     H    --  modern      --  Coined recently, words for new things (19-20)
                    );
   package Age_Type_IO is new Ada.Text_IO.Enumeration_IO (Age_Type);

   type Frequency_Type is (    --  For dictionary entries
     X,    --              --  Unknown or unspecified
     A,    --  very freq   --  Very frequent, in all Elementry Latin books
     B,    --  frequent    --  Frequent, in top 10 percent
     C,    --  common      --  For Dictionary, in top 10,000 words
     D,    --  lesser      --  For Dictionary, in top 20,000 words
     E,    --  uncommon    --  2 or 3 citations
     F,    --  very rare   --  Having only single citation in OLD or L+S
     I,    --  inscription --  Only citation is inscription
     M,    --  graffiti    --  Presently not much used
     N     --  Pliny       --  Appearing (almost) only in Pliny Natural History
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

   ---------------------------------------------------------------------------

   type Inflection_Record is
      record
         Qual   : Quality_Record := Null_Quality_Record;
         Key    : Stem_Key_Type  := 0;
         Ending : Ending_Record  := Null_Ending_Record;
         Age    : Age_Type       := X;
         Freq   : Frequency_Type := X;
      end record;

   Null_Inflection_Record : constant Inflection_Record :=
      (Null_Quality_Record, 0, Null_Ending_Record, X, X);

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Inflection_Record_IO is
      -- FIXME: This probably should be constant.
      Default_Width : constant Natural :=
        Quality_Record_IO.Default_Width + 1 +
        1  + 1 +
        Ending_Record_IO.Default_Width + 1 +
        Age_Type'Width + 1 +
        Frequency_Type'Width;
      procedure Get (File : in  File_Type; Item : out Inflection_Record);
      procedure Get (Item : out Inflection_Record);
      procedure Put (File : in  File_Type; Item : in  Inflection_Record);
      procedure Put (Item : in  Inflection_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
          Target : out Inflection_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Inflection_Record);
   end Inflection_Record_IO;

   ---------------------------------------------------------------------------
   --  This implies a knowledge of the inflections last Character
   subtype Inflections_Section_1 is Character range 'a' .. 'i';
   subtype Inflections_Section_2 is Character range 'm' .. 'r';
   subtype Inflections_Section_3 is Character range 's' .. 's';
   subtype Inflections_Section_4 is Character range 't' .. 'u';

   Size_Of_Blank_Inflections   : constant Integer :=  80;    --  ############
   Size_Of_Inflections_Section : constant Integer := 570;    --  ############

   type Inflection_Array is array (Positive range <>) of Inflection_Record;
   subtype Lel_Section is Inflection_Array (1 .. Size_Of_Inflections_Section);
   package Lel_Section_Io is new Ada.Direct_IO (Lel_Section);

   Bel : Inflection_Array (1 .. Size_Of_Blank_Inflections);

   Lel : Lel_Section;

   type Inflection_Array_Index is array (Integer range <>,
     Character range <>) of Integer;

   -- FIXME: Following 6 variables are global state. This should be eliminated.
   Belf, Bell : Inflection_Array_Index (0 .. 0, ' ' .. ' ') :=
     (0 => (others => 0));
   Lelf, Lell : Inflection_Array_Index (1 .. Max_Ending_Size,
     'a' .. 'z') := (others => (others => 0));
   Pelf, Pell : Inflection_Array_Index (1 .. Max_Ending_Size,
     'a' .. 'z') := (others => (others => 0));

   Number_Of_Inflections : Integer := 0;

   procedure Establish_Inflections_Section;

   --  <=   means for this purpose "contained in"
   overriding function "<=" (Left, Right : Part_Of_Speech_Type) return Boolean;
   function "<=" (Left, Right : Decn_Record) return Boolean;
   overriding function "<=" (Left, Right : Gender_Type) return Boolean;
   overriding function "<=" (Left, Right : Case_Type)   return Boolean;
   overriding function "<=" (Left, Right : Number_Type) return Boolean;
   overriding function "<=" (Left, Right : Person_Type) return Boolean;
   overriding function "<=" (Left, Right : Comparison_Type) return Boolean;
   function "<=" (Left, Right : Tense_Voice_Mood_Record)  return Boolean;
   overriding function "<=" (Left, Right : Noun_Kind_Type)   return Boolean;
   overriding function "<=" (Left, Right : Pronoun_Kind_Type)   return Boolean;
   overriding function "<=" (Left, Right : Stem_Key_Type)   return Boolean;
   -- not verbs

   overriding function "<=" (Left, Right : Age_Type)   return Boolean;
   overriding function "<=" (Left, Right : Frequency_Type)   return Boolean;

   Give_Up : exception;

end Latin_Utils.Inflections_Package;
