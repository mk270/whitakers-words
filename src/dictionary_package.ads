-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936â€“2010)
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

--  Need KIND_ENTRY and IO
--  Need to modify TRANS
with Text_IO; use Text_IO;
with Direct_IO;
with Inflections_Package; use Inflections_Package;
package Dictionary_Package is
   pragma Elaborate_Body;

   zzz_stem  : constant Stem_Type := "zzz" & (4..Max_Stem_Size => ' ');
   type stems_type is array (Stem_Key_Type range 1..4) of Stem_Type;
   null_stems_type : constant stems_type := (others => Null_Stem_Type);

   type Dictionary_Kind is (x,            --  null
     addons,       --  For FIXES
     xxx,          --  TRICKS
     yyy,          --  Syncope
     nnn,          --  Unknown Name
     rrr,          --  Roman Numerals
     ppp,          --  Compounds
     general, special, local, unique);

   package Dictionary_Kind_IO is new Text_IO.Enumeration_IO (Dictionary_Kind);

   ext : array (Dictionary_Kind) of String(1..3) := ("X  ", "ADD", "XXX", "YYY",
     "NNN", "RRR", "PPP",
     "GEN", "SPE", "LOC",
     "UNI");

   Default_Dictionary_Kind : Dictionary_Kind := x;

   dictionary_available : array (Dictionary_Kind) of Boolean := (False,
     False, False, False, False, False, False,  --  don't SEARCH
     False, False, False, False);
   --  Start out as FALSE and set to TRUE when the DICT is loaded

   type area_type is (
     x,      --  All or none
     a,      --  Agriculture, Flora, Fauna, Land, Equipment, Rural
     b,      --  Biological, Medical, Body Parts
     d,      --  Drama, Music, Theater, Art, Painting, Sculpture
     e,      --  Ecclesiastic, Biblical, Religious
     g,      --  Grammar, Retoric, Logic, Literature, Schools
     l,      --  Legal, Government, Tax, Financial, Political, Titles
     p,      --  Poetic
     s,      --  Science, Philosophy, Mathematics, Units/Measures
     t,      --  Technical, Architecture, Topography, Surveying
     w,      --  War, Military, Naval, Ships, Armor
     y       --  Mythology
                     );

   package area_type_io is new Text_IO.enumeration_io(area_type);

   type geo_type is (
     x,      --  All or none
     a,      --  Africa
     b,      --  Britian
     c,      --  China
     d,      --  Scandinavia
     e,      --  Egypt
     f,      --  France, Gaul
     g,      --  Germany
     h,      --  Greece
     i,      --  Italy, Rome
     j,      --  India
     k,      --  Balkans
     n,      --  Netherlands
     p,      --  Persia
     q,      --  Near East
     r,      --  Russia
     s,      --  Spain, Iberia
     u       --  Eastern Europe
                    );

   package geo_type_io is new Text_IO.enumeration_io(geo_type);

   type source_type is (
     x,      --  General or unknown or too common to say
     a,
     b,      --  C.H.Beeson, A Primer of Medieval Latin, 1925 (Bee)
     c,      --  Charles Beard, Cassell's Latin Dictionary 1892 (Cas)
     d,      --  J.N.Adams, Latin Sexual Vocabulary, 1982 (Sex)
     e,      --  L.F.Stelten, Dictionary of Eccles. Latin, 1995 (Ecc)
     f,      --  Roy J. Deferrari, Dictionary of St. Thomas Aquinas, 1960 (DeF)
     g,      --  Gildersleeve + Lodge, Latin Grammar 1895 (G+L)
     h,      --  Collatinus Dictionary by Yves Ouvrard
     i,      --  Leverett, F.P., Lexicon of the Latin Language, Boston 1845
     j,      --  Bracton: De Legibus Et Consuetudinibus Angliæ
     k,      --  Calepinus Novus, modern Latin, by Guy Licoppe (Cal)
     l,      --  Lewis, C.S., Elementary Latin Dictionary 1891
     m,      --  Latham, Revised Medieval Word List, 1980 (Latham)
     n,      --  Lynn Nelson, Wordlist (Nel)
     o,      --  Oxford Latin Dictionary, 1982 (OLD)
     p,      --  Souter, A Glossary of Later Latin to 600 A.D., Oxford 1949 (Souter)
     q,      --  Other, cited or unspecified dictionaries
     r,      --  Plater + White, A Grammar of the Vulgate, Oxford 1926 (Plater)
     s,      --  Lewis and Short, A Latin Dictionary, 1879 (L+S)
     t,      --  Found in a translation  --  no dictionary reference
     u,      --
     v,      --  Vademecum in opus Saxonis - Franz Blatt (Saxo)
     w,      --  My personal guess, mostly obvious extrapolation (Whitaker or W)
     y,      --  Temp special code
     z       --  Sent by user --  no dictionary reference, Mostly John White of Blitz Latin

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

     --  Note that the WORDS dictionary is not just a copy of source info, but the
     --  indicated SOURCE is a main reference/check point used to derive the entry

                       );

   package source_type_io is new Text_IO.enumeration_io(source_type);

   type kind_entry(pofs : Part_Of_Speech_Type := x) is
      record
         case pofs is
            when n =>
               n_kind : Noun_Kind_Type := x;
            when pron =>
               pron_kind : Pronoun_Kind_Type := x;
            when pack =>
               pack_kind : Pronoun_Kind_Type := x;
            when adj =>
               null;
            when num =>
               num_value : Numeral_Value_Type := 0;
            when v =>
               v_kind : verb_kind_type := x;
            when vpar =>
               vpar_kind : verb_kind_type := x;
            when supine =>
               supine_kind : verb_kind_type := x;
            when others =>
               null;
         end case;
      end record;

   package kind_entry_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type;
                    ps : in Part_Of_Speech_Type; p : out kind_entry);
      procedure Get(ps : in Part_Of_Speech_Type; p : out kind_entry);
      procedure Put(f : in File_Type;
                    ps : in Part_Of_Speech_Type; p : in kind_entry);
      procedure Put(ps : in Part_Of_Speech_Type; p : in kind_entry);
      procedure Get(s : in String; ps : in Part_Of_Speech_Type;
                                   p : out kind_entry; last : out Integer);
      procedure Put(s : out String;
                    ps : in Part_Of_Speech_Type; p : in kind_entry);
   end kind_entry_io;

   null_kind_entry : kind_entry;

   type translation_record is
      record
         age  : age_type := x;
         area : area_type := x;
         geo  : geo_type := x;
         freq : frequency_type := x;
         source : source_type := x;
      end record;

   null_translation_record : translation_record;

   package translation_record_io is
      Default_Width : Text_IO.Field;
      procedure Get(f : in Text_IO.File_Type; tr : out translation_record);
      procedure Get(tr : out translation_record);
      procedure Put(f : in Text_IO.File_Type; tr : in translation_record);
      procedure Put(tr : in translation_record);
      procedure Get(s : in String; tr : out translation_record; last : out Integer);
      procedure Put(s : out String; tr : in translation_record);
   end translation_record_io;

   ---------------------------------------------------------------------------

   type Noun_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl   : Decn_Record := (0, 0);
         Gender : Gender_Type := x;
         Kind   : Noun_Kind_Type := x;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Noun_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in File_Type; Item: out Noun_Entry);
      procedure Get (Item : out Noun_Entry);
      procedure Put (File : in File_Type; Item : in Noun_Entry);
      procedure Put (Item : in Noun_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in  String;
           Target : out Noun_Entry;
           Last   : out Integer
         );
      procedure Put (Target: out String; Item : in Noun_Entry);
   end Noun_Entry_IO;

   ---------------------------------------------------------------------------

   type Pronoun_Entry is
      record
         -- NOTE: Seems like Decl is breaking of abbrev used
         Decl  : Decn_Record := (0, 0);
         Kind  : Pronoun_Kind_Type := x;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Pronoun_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in File_Type; Item : out Pronoun_Entry);
      procedure Get (Item : out Pronoun_Entry);
      procedure Put (File : in File_Type; Item : in Pronoun_Entry);
      procedure Put (Item : in Pronoun_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in  String;
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
         Kind  : Pronoun_Kind_Type := x;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Propack_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in  File_Type; Item : out Propack_Entry);
      procedure Get (Item : out Propack_Entry);
      procedure Put (File : in  File_Type; Item : in Propack_Entry);
      procedure Put (Item : in  Propack_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in  String;
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
         Co   : Comparison_Type := x;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Adjective_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in File_Type; Item : out Adjective_Entry);
      procedure Get (Item : out Adjective_Entry);
      procedure Put (File : in File_Type; Item : in Adjective_Entry);
      procedure Put (Item : in Adjective_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in  String;
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
         Sort  : Numeral_Sort_Type := x;
         Value : Numeral_Value_Type := 0;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Numeral_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in File_Type; Item : out Numeral_Entry);
      procedure Get (Item : out Numeral_Entry);
      procedure Put (File : in File_Type; Item : in Numeral_Entry);
      procedure Put (Item : in Numeral_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in  String;
           Target : out Numeral_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Numeral_Entry);
   end Numeral_Entry_IO;

   ---------------------------------------------------------------------------

   type Adverb_Entry is
      record
         Co   : Comparison_Type := x;
      end record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Adverb_Entry_IO is
      Default_Width : Natural;
      procedure Get (File : in File_Type; Item : out Adverb_Entry);
      procedure Get (Item : out Adverb_Entry);
      procedure Put (File : in File_Type; Item : in Adverb_Entry);
      procedure Put (Item : in Adverb_Entry);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in String;
           Target : out Adverb_Entry;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Adverb_Entry);
   end Adverb_Entry_IO;

   ---------------------------------------------------------------------------

   type verb_entry is
      record
         con  : Decn_Record := (0,0);
         kind : verb_kind_type := x;
      end record;

   package verb_entry_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; v : out verb_entry);
      procedure Get(v : out verb_entry);
      procedure Put(f : in File_Type; v : in verb_entry);
      procedure Put(v : in verb_entry);
      procedure Get(s : in String; v : out verb_entry; last : out Integer);
      procedure Put(s : out String; v : in verb_entry);
   end verb_entry_io;

   type preposition_entry is
      record
         obj : case_type := x;
      end record;

   package preposition_entry_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; p : out preposition_entry);
      procedure Get(p : out preposition_entry);
      procedure Put(f : in File_Type; p : in preposition_entry);
      procedure Put(p : in preposition_entry);
      procedure Get(s : in String; p : out preposition_entry; last : out Integer);
      procedure Put(s : out String; p : in preposition_entry);
   end preposition_entry_io;

   type conjunction_entry is
      record
         null;
      end record;

   package conjunction_entry_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; c : out conjunction_entry);
      procedure Get(c : out conjunction_entry);
      procedure Put(f : in File_Type; c : in conjunction_entry);
      procedure Put(c : in conjunction_entry);
      procedure Get(s : in String; c : out conjunction_entry; last : out Integer);
      procedure Put(s : out String; c : in conjunction_entry);
   end conjunction_entry_io;

   type interjection_entry is
      record
         null;
      end record;

   package interjection_entry_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; i : out interjection_entry);
      procedure Get(i : out interjection_entry);
      procedure Put(f : in File_Type; i : in interjection_entry);
      procedure Put(i : in interjection_entry);
      procedure Get(s : in String; i : out interjection_entry; last : out Integer);
      procedure Put(s : out String; i : in interjection_entry);
   end interjection_entry_io;

   type part_entry(pofs : Part_Of_Speech_Type := x) is
      record
         case pofs is
            when n =>
               n : Noun_Entry;
            when pron =>
               pron : Pronoun_Entry;
            when pack =>
               pack : Propack_Entry;
            when adj =>
               adj : Adjective_Entry;
            when num =>
               num : Numeral_Entry;
            when adv =>
               adv : Adverb_Entry;
            when v =>
               v : verb_entry;
            when vpar =>
               null;        --  There will be no VPAR dictionary entries
            when supine =>
               null;        --  There will be no SUPINE dictionary entries
            when prep =>
               prep : preposition_entry;
            when conj =>
               conj : conjunction_entry;
            when interj =>
               interj : interjection_entry;
            when others =>
               null;
         end case;
      end record;

   package part_entry_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; p : out part_entry);
      procedure Get(p : out part_entry);
      procedure Put(f : in File_Type; p : in part_entry);
      procedure Put(p : in part_entry);
      procedure Get(s : in String; p : out part_entry; last : out Integer);
      procedure Put(s : out String; p : in part_entry);
   end part_entry_io;

   null_part_entry : part_entry;

   function "<" (left, right : part_entry) return Boolean;

   type dictionary_entry is
      record
         stems : stems_type         := null_stems_type;
         part  : part_entry         := null_part_entry;
         --            KIND  : KIND_ENTRY         := NULL_KIND_ENTRY;
         tran  : translation_record := null_translation_record;
         mean  : Meaning_Type       := Null_Meaning_Type;
      end record;

   package dictionary_entry_io is
      Default_Width : Field;
      procedure Get(f : in File_Type; d : out dictionary_entry);
      procedure Get(d : out dictionary_entry);
      procedure Put(f : in File_Type; d : in dictionary_entry);
      procedure Put(d : in dictionary_entry);
      procedure Get(s : in String; d : out dictionary_entry; last : out Integer);
      procedure Put(s : out String; d : in dictionary_entry);
   end dictionary_entry_io;

   null_dictionary_entry : dictionary_entry;

   package Dict_IO is new direct_io(dictionary_entry);
   dict_file : array (Dictionary_Kind) of Dict_IO.File_Type;

   package MNPC_IO is new Text_IO.Integer_IO (Dict_IO.Count);
   subtype MNPC_type is Dict_IO.Count;
   Null_MNPC : Dict_IO.Count := Dict_IO.Count'First;

   ---------------------------------------------------------------------------

   type Parse_Record is
      record
         Stem  : Stem_Type := Null_Stem_Type;
         IR    : Inflection_Record := Null_Inflection_Record;
         D_K   : Dictionary_Kind := Default_Dictionary_Kind;
         MNPC  : Dict_IO.Count := Null_MNPC;
      end record;

   -- FIXME: Why this one is not constant?
   Null_Parse_Record : Parse_Record;

   -- FIXME: These subprograms don't check if Is_Open (File)
   package Parse_Record_IO is
      Default_Width : Text_IO.Field;
      procedure Get (File : in Text_IO.File_Type; Item : out Parse_Record);
      procedure Get (Item : out Parse_Record);
      procedure Put (File : in Text_IO.File_Type; Item : in Parse_Record);
      procedure Put (Item : in Parse_Record);
      -- TODO: Document meaning of Last
      procedure Get
         ( Source : in String;
           Target : out Parse_Record;
           Last   : out Integer
         );
      procedure Put (Target : out String; Item : in Parse_Record);
   end Parse_Record_IO;

   type Parse_Array is array (Integer range <>) of Parse_Record;

   ---------------------------------------------------------------------------

   function Number_Of_Stems (Part : Part_Of_Speech_Type) return Stem_Key_Type;

   overriding function "<=" (left, right : area_type) return Boolean;

end Dictionary_Package;
