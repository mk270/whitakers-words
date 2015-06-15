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
with text_io; use text_io;
with direct_io;
with inflections_package; use inflections_package;
package dictionary_package is
   pragma elaborate_body;

   zzz_stem  : constant stem_type := "zzz" & (4..max_stem_size => ' ');
   type stems_type is array (stem_key_type range 1..4) of stem_type;
   null_stems_type : constant stems_type := (others => null_stem_type);

   type dictionary_kind is (x,            --  null
     addons,       --  For FIXES
     xxx,          --  TRICKS
     yyy,          --  Syncope
     nnn,          --  Unknown Name
     rrr,          --  Roman Numerals
     ppp,          --  Compounds
     general, special, local, unique);

   package dictionary_kind_io is new text_io.enumeration_io(dictionary_kind);

   ext : array (dictionary_kind) of string(1..3) := ("X  ", "ADD", "XXX", "YYY",
     "NNN", "RRR", "PPP",
     "GEN", "SPE", "LOC",
     "UNI");

   default_dictionary_kind : dictionary_kind := x;

   dictionary_available : array (dictionary_kind) of boolean := (false,
     false, false, false, false, false, false,  --  don't SEARCH
     false, false, false, false);
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

   package area_type_io is new text_io.enumeration_io(area_type);

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

   package geo_type_io is new text_io.enumeration_io(geo_type);

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
     z       --  Sent by user --  no dictionary reference
             --  Mostly John White of Blitz Latin

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

   package source_type_io is new text_io.enumeration_io(source_type);

   type kind_entry(pofs : part_of_speech_type := x) is
      record
         case pofs is
            when n =>
               n_kind : noun_kind_type := x;
            when pron =>
               pron_kind : pronoun_kind_type := x;
            when pack =>
               pack_kind : pronoun_kind_type := x;
            when adj =>
               null;
            when num =>
               num_value : numeral_value_type := 0;
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
      default_width : natural;
      procedure get(f : in file_type;
                    ps : in part_of_speech_type; p : out kind_entry);
      procedure get(ps : in part_of_speech_type; p : out kind_entry);
      procedure put(f : in file_type;
                    ps : in part_of_speech_type; p : in kind_entry);
      procedure put(ps : in part_of_speech_type; p : in kind_entry);
      procedure get(s : in string; ps : in part_of_speech_type;
                                   p : out kind_entry; last : out integer);
      procedure put(s : out string;
                    ps : in part_of_speech_type; p : in kind_entry);
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
      default_width : text_io.field;
      procedure get(f : in text_io.file_type; tr : out translation_record);
      procedure get(tr : out translation_record);
      procedure put(f : in text_io.file_type; tr : in translation_record);
      procedure put(tr : in translation_record);
      procedure get(s : in string; tr : out translation_record; last : out integer);
      procedure put(s : out string; tr : in translation_record);
   end translation_record_io;

   type noun_entry is
      record
         decl   : decn_record := (0, 0);
         gender : gender_type := x;
         kind   : noun_kind_type := x;
      end record;

   package noun_entry_io is
      default_width : natural;
      procedure get(f : in file_type; n : out noun_entry);
      procedure get(n : out noun_entry);
      procedure put(f : in file_type; n : in noun_entry);
      procedure put(n : in noun_entry);
      procedure get(s : in string; n : out noun_entry; last : out integer);
      procedure put(s : out string; n : in noun_entry);
   end noun_entry_io;

   type pronoun_entry is
      record
         decl  : decn_record := (0,0);
         kind : pronoun_kind_type := x;
      end record;

   package pronoun_entry_io is
      default_width : natural;
      procedure get(f : in file_type; p : out pronoun_entry);
      procedure get(p : out pronoun_entry);
      procedure put(f : in file_type; p : in pronoun_entry);
      procedure put(p : in pronoun_entry);
      procedure get(s : in string; p : out pronoun_entry; last : out integer);
      procedure put(s : out string; p : in pronoun_entry);
   end pronoun_entry_io;

   type propack_entry is
      record
         decl  : decn_record := (0,0);
         kind : pronoun_kind_type := x;
      end record;

   package propack_entry_io is
      default_width : natural;
      procedure get(f : in file_type; p : out propack_entry);
      procedure get(p : out propack_entry);
      procedure put(f : in file_type; p : in propack_entry);
      procedure put(p : in propack_entry);
      procedure get(s : in string; p : out propack_entry; last : out integer);
      procedure put(s : out string; p : in propack_entry);
   end propack_entry_io;

   type adjective_entry is
      record
         decl : decn_record := (0, 0);
         co   : comparison_type := x;
      end record;

   package adjective_entry_io is
      default_width : natural;
      procedure get(f : in file_type; a : out adjective_entry);
      procedure get(a : out adjective_entry);
      procedure put(f : in file_type; a : in adjective_entry);
      procedure put(a : in adjective_entry);
      procedure get(s : in string; a : out adjective_entry; last : out integer);
      procedure put(s : out string; a : in adjective_entry);
   end adjective_entry_io;

   type numeral_entry is
      record
         decl  : decn_record := (0,0);
         sort  : numeral_sort_type := x;
         value : numeral_value_type := 0;
      end record;

   package numeral_entry_io is
      default_width : natural;
      procedure get(f : in file_type; num : out numeral_entry);
      procedure get(num : out numeral_entry);
      procedure put(f : in file_type; num : in numeral_entry);
      procedure put(num : in numeral_entry);
      procedure get(s : in string; num : out numeral_entry; last : out integer);
      procedure put(s : out string; num : in numeral_entry);
   end numeral_entry_io;

   type adverb_entry is
      record
         co   : comparison_type := x;
      end record;

   package adverb_entry_io is
      default_width : natural;
      procedure get(f : in file_type; a : out adverb_entry);
      procedure get(a : out adverb_entry);
      procedure put(f : in file_type; a : in adverb_entry);
      procedure put(a : in adverb_entry);
      procedure get(s : in string; a : out adverb_entry; last : out integer);
      procedure put(s : out string; a : in adverb_entry);
   end adverb_entry_io;

   type verb_entry is
      record
         con  : decn_record := (0,0);
         kind : verb_kind_type := x;
      end record;

   package verb_entry_io is
      default_width : natural;
      procedure get(f : in file_type; v : out verb_entry);
      procedure get(v : out verb_entry);
      procedure put(f : in file_type; v : in verb_entry);
      procedure put(v : in verb_entry);
      procedure get(s : in string; v : out verb_entry; last : out integer);
      procedure put(s : out string; v : in verb_entry);
   end verb_entry_io;

   type preposition_entry is
      record
         obj : case_type := x;
      end record;

   package preposition_entry_io is
      default_width : natural;
      procedure get(f : in file_type; p : out preposition_entry);
      procedure get(p : out preposition_entry);
      procedure put(f : in file_type; p : in preposition_entry);
      procedure put(p : in preposition_entry);
      procedure get(s : in string; p : out preposition_entry; last : out integer);
      procedure put(s : out string; p : in preposition_entry);
   end preposition_entry_io;

   type conjunction_entry is
      record
         null;
      end record;

   package conjunction_entry_io is
      default_width : natural;
      procedure get(f : in file_type; c : out conjunction_entry);
      procedure get(c : out conjunction_entry);
      procedure put(f : in file_type; c : in conjunction_entry);
      procedure put(c : in conjunction_entry);
      procedure get(s : in string; c : out conjunction_entry; last : out integer);
      procedure put(s : out string; c : in conjunction_entry);
   end conjunction_entry_io;

   type interjection_entry is
      record
         null;
      end record;

   package interjection_entry_io is
      default_width : natural;
      procedure get(f : in file_type; i : out interjection_entry);
      procedure get(i : out interjection_entry);
      procedure put(f : in file_type; i : in interjection_entry);
      procedure put(i : in interjection_entry);
      procedure get(s : in string; i : out interjection_entry; last : out integer);
      procedure put(s : out string; i : in interjection_entry);
   end interjection_entry_io;

   type part_entry(pofs : part_of_speech_type := x) is
      record
         case pofs is
            when n =>
               n : noun_entry;
            when pron =>
               pron : pronoun_entry;
            when pack =>
               pack : propack_entry;
            when adj =>
               adj : adjective_entry;
            when num =>
               num : numeral_entry;
            when adv =>
               adv : adverb_entry;
            when v =>
               v : verb_entry;
            when vpar =>
               null;                 --  There will be no VPAR dictionary entries
            when supine =>
               null;                 --  There will be no SUPINE dictionary entries
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
      default_width : natural;
      procedure get(f : in file_type; p : out part_entry);
      procedure get(p : out part_entry);
      procedure put(f : in file_type; p : in part_entry);
      procedure put(p : in part_entry);
      procedure get(s : in string; p : out part_entry; last : out integer);
      procedure put(s : out string; p : in part_entry);
   end part_entry_io;

   null_part_entry : part_entry;

   function "<" (left, right : part_entry) return boolean;

   type dictionary_entry is
      record
         stems : stems_type         := null_stems_type;
         part  : part_entry         := null_part_entry;
         --            KIND  : KIND_ENTRY         := NULL_KIND_ENTRY;
         tran  : translation_record := null_translation_record;
         mean  : meaning_type       := null_meaning_type;
      end record;

   package dictionary_entry_io is
      default_width : field;
      procedure get(f : in file_type; d : out dictionary_entry);
      procedure get(d : out dictionary_entry);
      procedure put(f : in file_type; d : in dictionary_entry);
      procedure put(d : in dictionary_entry);
      procedure get(s : in string; d : out dictionary_entry; last : out integer);
      procedure put(s : out string; d : in dictionary_entry);
   end dictionary_entry_io;

   null_dictionary_entry : dictionary_entry;

   package dict_io is new direct_io(dictionary_entry);
   dict_file : array (dictionary_kind) of dict_io.file_type;

   package mnpc_io is new text_io.integer_io(dict_io.count);
   subtype mnpc_type is dict_io.count;
   null_mnpc : dict_io.count := dict_io.count'first;

   type parse_record is
      record
         stem  : stem_type := null_stem_type;
         ir    : inflection_record := null_inflection_record;
         d_k   : dictionary_kind := default_dictionary_kind;
         mnpc  : dict_io.count := null_mnpc;
      end record;

   null_parse_record : parse_record;

   package parse_record_io is
      default_width : text_io.field;
      procedure get(f : in text_io.file_type; pr : out parse_record);
      procedure get(pr : out parse_record);
      procedure put(f : in text_io.file_type; pr : in parse_record);
      procedure put(pr : in parse_record);
      procedure get(s : in string; pr : out parse_record; last : out integer);
      procedure put(s : out string; pr : in parse_record);
   end parse_record_io;

   type parse_array is array (integer range <>) of parse_record;

   function number_of_stems(p : part_of_speech_type) return stem_key_type;

   function "<=" (left, right : area_type) return boolean;

end dictionary_package;
