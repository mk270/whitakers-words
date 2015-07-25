-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936-2010)
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

with Ada.Text_IO;
with Ada.Direct_IO;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
package Latin_Utils.Dictionary_Package is

   ---------------------------------------------------------------------------

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------

   ZZZ_Stem  : constant Stem_Type := "zzz" & (4 .. Max_Stem_Size => ' ');
   type Stems_Type is array (Stem_Key_Type range 1 .. 4) of Stem_Type;
   Null_Stems_Type : constant Stems_Type := (others => Null_Stem_Type);

   ---------------------------------------------------------------------------

   type Dictionary_Kind is
      (X,            --  null
        Addons,       --  For FIXES
        Xxx,          --  TRICKS
        Yyy,          --  Syncope
        Nnn,          --  Unknown Name
        Rrr,          --  Roman Numerals
        Ppp,          --  Compounds
        General,
        Special,
        Local,
        Unique
      );

   package Dictionary_Kind_IO is new
      Ada.Text_IO.Enumeration_IO (Dictionary_Kind);

   Ext : array (Dictionary_Kind) of String (1 .. 3) :=
      ("X  ", "ADD", "XXX", "YYY", "NNN", "RRR", "PPP",
        "GEN", "SPE", "LOC", "UNI"
      );

   Default_Dictionary_Kind : Dictionary_Kind := X;

   --  Starts out as False and set to True when the Dict is loaded
   Dictionary_Available : array (Dictionary_Kind) of Boolean :=
      (others => False);

   ---------------------------------------------------------------------------

   type Area_Type is
      (X,      --  All or none
        A,      --  Agriculture, Flora, Fauna, Land, Equipment, Rural
        B,      --  Biological, Medical, Body Parts
        D,      --  Drama, Music, Theater, Art, Painting, Sculpture
        E,      --  Ecclesiastic, Biblical, Religious
        G,      --  Grammar, Retoric, Logic, Literature, Schools
        L,      --  Legal, Government, Tax, Financial, Political, Titles
        P,      --  Poetic
        S,      --  Science, Philosophy, Mathematics, Units/Measures
        T,      --  Technical, Architecture, Topography, Surveying
        W,      --  War, Military, Naval, Ships, Armor
        Y       --  Mythology
      );

   overriding function "<=" (Left, Right : Area_Type) return Boolean;

   package Area_Type_IO is new
      Ada.Text_IO.Enumeration_IO (Area_Type);

   ---------------------------------------------------------------------------

   type Geo_Type is
      (X,      --  All or none
        A,      --  Africa
        B,      --  Britian
        C,      --  China
        D,      --  Scandinavia
        E,      --  Egypt
        F,      --  France, Gaul
        G,      --  Germany
        H,      --  Greece
        I,      --  Italy, Rome
        J,      --  India
        K,      --  Balkans
        N,      --  Netherlands
        P,      --  Persia
        Q,      --  Near East
        R,      --  Russia
        S,      --  Spain, Iberia
        U       --  Eastern Europe
      );

   package Geo_Type_IO is new
      Ada.Text_IO.Enumeration_IO (Geo_Type);

   ---------------------------------------------------------------------------

   type Source_Type is
      (X,  --  General or unknown or too common to say
        A,
        B,  --  C.H.Beeson, A Primer of Medieval Latin, 1925 (Bee)
        C,  --  Charles Beard, Cassell's Latin Dictionary 1892 (Cas)
        D,  --  J.N.Adams, Latin Sexual Vocabulary, 1982 (Sex)
        E,  --  L.F.Stelten, Dictionary of Eccles. Latin, 1995 (Ecc)
        F,  --  Roy J. Deferrari, Dictionary of St. Thomas Aquinas, 1960 (DeF)
        G,  --  Gildersleeve + Lodge, Latin Grammar 1895 (G+L)
        H,  --  Collatinus Dictionary by Yves Ouvrard
        I,  --  Leverett, F.P., Lexicon of the Latin Language, Boston 1845
        J,  --  Bracton: De Legibus Et Consuetudinibus Angliæ
        K,  --  Calepinus Novus, modern Latin, by Guy Licoppe (Cal)
        L,  --  Lewis, C.S., Elementary Latin Dictionary 1891
        M,  --  Latham, Revised Medieval Word List, 1980 (Latham)
        N,  --  Lynn Nelson, Wordlist (Nel)
        O,  --  Oxford Latin Dictionary, 1982 (OLD)
        P,  --  Souter, A Glossary of Later Latin to 600 A.D.,
            --    Oxford 1949 (Souter)
        Q,  --  Other, cited or unspecified dictionaries
        R,  --  Plater + White, A Grammar of the Vulgate, Oxford 1926 (Plater)
        S,  --  Lewis and Short, A Latin Dictionary, 1879 (L+S)
        T,  --  Found in a translation  --  no dictionary reference
        U,  --
        V,  --  Vademecum in opus Saxonis - Franz Blatt (Saxo)
        W,  --  My personal guess, mostly obvious extrapolation (Whitaker or W)
        Y,  --  Temp special code
        Z   --  Sent by user -- no dictionary reference,
            --    Mostly John White of Blitz Latin

      --  Consulted but used only indirectly
      --  Liddell + Scott Greek-English Lexicon (Lid)
      --  Oxford English Dictionary 2002 (OED)

      --  Consulted but used only occasionally, seperately referenced
      --  D.A. Kidd, Collins Latin Gem Dictionary, 1957 (Col)
      --  Allen + Greenough, New Latin Grammar, 1888 (A+G)
      --  Harrington/Pucci/Elliott, Medieval Latin 2nd Ed 1997 (Harr)
      --  C.C./C.L. Scanlon Latin Grammar/Second Latin, TAN 1976 (SCANLON)
      --  W. M. Lindsay, Short Historical Latin Grammar, 1895 (Lindsay)
      --  Du Cange
      --  Oxford English Dictionary (OED)

      --  NOTE that the WORDS dictionary is not just a copy of source info,
      --  but the indicated SOURCE is a main reference/check point used to
      --  derive the entry
      );

   package Source_Type_IO is new
      Ada.Text_IO.Enumeration_IO (Source_Type);

   ---------------------------------------------------------------------------
   -- NOTE: Should n and v be changed to noun and verb for clarity?
   type Kind_Entry (Pofs : Part_Of_Speech_Type := X) is
      record
         case Pofs is
            when N =>
               N_Kind : Noun_Kind_Type := X;
            when Pron =>
               Pron_Kind : Pronoun_Kind_Type := X;
            when Pack =>
               Pack_Kind : Pronoun_Kind_Type := X;
            when Adj | Adv | X =>
               null;
            when Num =>
               Num_Value : Numeral_Value_Type := 0;
            when V =>
               V_Kind : Verb_Kind_Type := X;
            when Vpar =>
               Vpar_Kind : Verb_Kind_Type := X;
            when Supine =>
               Supine_Kind : Verb_Kind_Type := X;
            when Prep .. Suffix =>
               null;
         end case;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   -- TODO: Document the meaning of POFS
   package Kind_Entry_IO is
      Default_Width : Natural;
      procedure Get
         (File : in  Ada.Text_IO.File_Type;
           POFS : in  Part_Of_Speech_Type;
           Item : out Kind_Entry
         );
      procedure Get (POFS : in Part_Of_Speech_Type; Item : out Kind_Entry);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           POFS : in Part_Of_Speech_Type;
           Item : in Kind_Entry
         );
      procedure Put (POFS : in Part_Of_Speech_Type; Item : in Kind_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           POFS   : in  Part_Of_Speech_Type;
           Target : out Kind_Entry;
           Last   : out Integer
         );
      procedure Put
         (Target : out String;
           POFS   : in  Part_Of_Speech_Type;
           Item   : in  Kind_Entry
         );
   end Kind_Entry_IO;

   Null_Kind_Entry : constant Kind_Entry := (Pofs => X);

   ---------------------------------------------------------------------------

   type Translation_Record is
      record
         Age  : Age_Type       := X;
         Area : Area_Type      := X;
         Geo  : Geo_Type       := X;
         Freq : Frequency_Type := X;
         Source : Source_Type  := X;
      end record;

   Null_Translation_Record : constant Translation_Record := (X, X, X, X, X);

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Translation_Record_IO is
      Default_Width : Ada.Text_IO.Field;
      procedure Get
         (File : in  Ada.Text_IO.File_Type;
           Item : out Translation_Record
         );
      procedure Get (Item : out Translation_Record);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           Item : in Translation_Record
         );
      procedure Put (Item : in Translation_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Translation_Record;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Translation_Record);
   end Translation_Record_IO;

   ---------------------------------------------------------------------------

   type Noun_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl   : Decn_Record := (0, 0);
         Gender : Gender_Type := X;
         Kind   : Noun_Kind_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Noun_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Noun_Entry);
      procedure Get (Item : out Noun_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Noun_Entry);
      procedure Put (Item : in Noun_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Noun_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Noun_Entry);
   end Noun_Entry_IO;

   ---------------------------------------------------------------------------

   type Pronoun_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl  : Decn_Record := (0, 0);
         Kind  : Pronoun_Kind_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Pronoun_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Pronoun_Entry);
      procedure Get (Item : out Pronoun_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Pronoun_Entry);
      procedure Put (Item : in Pronoun_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Pronoun_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Pronoun_Entry);
   end Pronoun_Entry_IO;

   ---------------------------------------------------------------------------

   type Propack_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl  : Decn_Record := (0, 0);
         Kind  : Pronoun_Kind_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Propack_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Propack_Entry);
      procedure Get (Item : out Propack_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Propack_Entry);
      procedure Put (Item : in  Propack_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Propack_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Propack_Entry);
   end Propack_Entry_IO;

   ---------------------------------------------------------------------------

   type Adjective_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl : Decn_Record := (0, 0);
         Co   : Comparison_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Adjective_Entry_IO is
      Default_Width : Natural;
      procedure Get
         (File : in Ada.Text_IO.File_Type;
           Item : out Adjective_Entry
         );
      procedure Get (Item : out Adjective_Entry);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           Item : in Adjective_Entry
         );
      procedure Put (Item : in Adjective_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Adjective_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Adjective_Entry);
   end Adjective_Entry_IO;

   ---------------------------------------------------------------------------

   type Numeral_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl  : Decn_Record := (0, 0);
         Sort  : Numeral_Sort_Type := X;
         Value : Numeral_Value_Type := 0;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Numeral_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Numeral_Entry);
      procedure Get (Item : out Numeral_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Numeral_Entry);
      procedure Put (Item : in Numeral_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Numeral_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Numeral_Entry);
   end Numeral_Entry_IO;

   ---------------------------------------------------------------------------

   type Adverb_Entry is
      record
         Co   : Comparison_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Adverb_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Adverb_Entry);
      procedure Get (Item : out Adverb_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Adverb_Entry);
      procedure Put (Item : in Adverb_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Adverb_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Adverb_Entry);
   end Adverb_Entry_IO;

   ---------------------------------------------------------------------------

   type Verb_Entry is
      record
         -- NOTE: Other records use Decl as field name for Decn_Record,
         --    should this one be changed into Decl for consistency?
         Con  : Decn_Record := (0, 0);
         Kind : Verb_Kind_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Verb_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Verb_Entry);
      procedure Get (Item : out Verb_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Verb_Entry);
      procedure Put (Item : in Verb_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Verb_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Verb_Entry);
   end Verb_Entry_IO;

   ---------------------------------------------------------------------------

   type Preposition_Entry is
      record
         Obj : Case_Type := X;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Preposition_Entry_IO is
      Default_Width : Natural;
      procedure Get
         (File : in Ada.Text_IO.File_Type;
           Item : out Preposition_Entry
         );
      procedure Get (Item : out Preposition_Entry);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           Item : in Preposition_Entry
         );
      procedure Put (Item : in Preposition_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Preposition_Entry;
           Last : out Integer
         );
      procedure Put (Target : out String; Item : in Preposition_Entry);
   end Preposition_Entry_IO;

   ---------------------------------------------------------------------------

   type Conjunction_Entry is null record;

   package Conjunction_Entry_IO is
      Default_Width : Natural;
      procedure Get
         (File : in Ada.Text_IO.File_Type;
           Item : out Conjunction_Entry
         );
      procedure Get (Item : out Conjunction_Entry);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           Item : in Conjunction_Entry
         );
      procedure Put (Item : in Conjunction_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in String;
           Target : out Conjunction_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Conjunction_Entry);
   end Conjunction_Entry_IO;

   ---------------------------------------------------------------------------

   type Interjection_Entry is null record;

   package Interjection_Entry_IO is
      Default_Width : Natural;
      procedure Get
         (File : in Ada.Text_IO.File_Type;
           Item : out Interjection_Entry
         );
      procedure Get (Item : out Interjection_Entry);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           Item : in Interjection_Entry
         );
      procedure Put (Item : in Interjection_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in String;
           Target : out Interjection_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Interjection_Entry);
   end Interjection_Entry_IO;

   ---------------------------------------------------------------------------
   -- NOTE: Should n and v be changed to noun and verb for clarity?
   type Part_Entry (Pofs : Part_Of_Speech_Type := X) is
      record
         case Pofs is
            when N =>
               N : Noun_Entry;
            when Pron =>
               Pron : Pronoun_Entry;
            when Pack =>
               Pack : Propack_Entry;
            when Adj =>
               Adj : Adjective_Entry;
            when Num =>
               Num : Numeral_Entry;
            when Adv =>
               Adv : Adverb_Entry;
            when V =>
               V : Verb_Entry;
            when Vpar =>
               null;        --  There will be no VPAR dictionary entries
            when Supine =>
               null;        --  There will be no SUPINE dictionary entries
            when Prep =>
               Prep : Preposition_Entry;
            when Conj =>
               Conj : Conjunction_Entry;
            when Interj =>
               Interj : Interjection_Entry;
            when Tackon .. Suffix =>
               null;
            when X =>
               null;
         end case;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Part_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Part_Entry);
      procedure Get (Item : out Part_Entry);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Part_Entry);
      procedure Put (Item : in Part_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Part_Entry;
           Last   : out Integer
         );
      -- FIXME: There is possibility that remainder of Target is not filled with
      --    ' ' in certain cases.
      procedure Put (Target : out String; Item : in Part_Entry);
   end Part_Entry_IO;

   Null_Part_Entry : constant Part_Entry := (Pofs => X);

   -- FIXME: In some cases may result in erroneous execution due to no return
   --    (ie. Left.pofs = Right.pofs and then Left.pofs = x)
   function "<" (Left, Right : Part_Entry) return Boolean;

   ---------------------------------------------------------------------------

   type Dictionary_Entry is
      record
         Stems : Stems_Type         := Null_Stems_Type;
         Part  : Part_Entry         := Null_Part_Entry;
         Tran  : Translation_Record := Null_Translation_Record;
         Mean  : Meaning_Type       := Null_Meaning_Type;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Dictionary_Entry_IO is
      Default_Width : Ada.Text_IO.Field;
      procedure Get
         (File : in Ada.Text_IO.File_Type;
           Item : out Dictionary_Entry
         );
      procedure Get (Item : out Dictionary_Entry);
      procedure Put
         (File : in Ada.Text_IO.File_Type;
           Item : in Dictionary_Entry
         );
      procedure Put (Item : in Dictionary_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in  String;
           Target : out Dictionary_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Dictionary_Entry);
   end Dictionary_Entry_IO;

   Null_Dictionary_Entry : constant Dictionary_Entry :=
      (Null_Stems_Type, Null_Part_Entry,
        Null_Translation_Record, Null_Meaning_Type
      );

   package Dict_IO is new Ada.Direct_IO (Dictionary_Entry);
   Dict_File : array (Dictionary_Kind) of Dict_IO.File_Type;

   ---------------------------------------------------------------------------

   package MNPC_IO is new Ada.Text_IO.Integer_IO (Dict_IO.Count);
   subtype MNPC_Type is Dict_IO.Count;
   Null_MNPC : constant Dict_IO.Count := Dict_IO.Count'First;

   ---------------------------------------------------------------------------

   type Parse_Record is
      record
         Stem  : Stem_Type := Null_Stem_Type;
         IR    : Inflection_Record := Null_Inflection_Record;
         D_K   : Dictionary_Kind := Default_Dictionary_Kind;
         MNPC  : Dict_IO.Count := Null_MNPC;
      end record;

   Null_Parse_Record : constant Parse_Record :=
      (Null_Stem_Type, Null_Inflection_Record,
        Default_Dictionary_Kind, Null_MNPC
      );

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Parse_Record_IO is
      Default_Width : Ada.Text_IO.Field;
      procedure Get (File : in Ada.Text_IO.File_Type; Item : out Parse_Record);
      procedure Get (Item : out Parse_Record);
      procedure Put (File : in Ada.Text_IO.File_Type; Item : in Parse_Record);
      procedure Put (Item : in Parse_Record);
      -- TODO: Document meaning of Last
      procedure Get
         (Source : in String;
          Target : out Parse_Record;
          Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Parse_Record);
   end Parse_Record_IO;

   type Parse_Array is array (Integer range <>) of Parse_Record;

   ---------------------------------------------------------------------------

   function Number_Of_Stems (Part : Part_Of_Speech_Type) return Stem_Key_Type;

   ---------------------------------------------------------------------------

end Latin_Utils.Dictionary_Package;
