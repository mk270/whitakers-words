--  Need KIND_ENTRY and IO
--  Need to modify TRANS
   with TEXT_IO; 
   with DIRECT_IO;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   package DICTIONARY_PACKAGE is
      use TEXT_IO;
   
      ZZZ_STEM  : constant STEM_TYPE := "zzz" & (4..MAX_STEM_SIZE => ' ');
      type STEMS_TYPE is array (STEM_KEY_TYPE range 1..4) of STEM_TYPE;
      NULL_STEMS_TYPE : constant STEMS_TYPE := (others => NULL_STEM_TYPE);
   
   
      type DICTIONARY_KIND is (X,            --  null
                               ADDONS,       --  For FIXES
                               XXX,          --  TRICKS
                               YYY,          --  Syncope
                               NNN,          --  Unknown Name
                               RRR,          --  Roman Numerals
                               PPP,          --  Compounds
                               GENERAL, SPECIAL, LOCAL, UNIQUE);
   
      package DICTIONARY_KIND_IO is new TEXT_IO.ENUMERATION_IO(DICTIONARY_KIND);
   
      EXT : array (DICTIONARY_KIND) of STRING(1..3) := ("X  ", "ADD", "XXX", "YYY", 
                                                        "NNN", "RRR", "PPP",
                                                        "GEN", "SPE", "LOC", 
                                                        "UNI");
   
      DEFAULT_DICTIONARY_KIND : DICTIONARY_KIND := X;
   
      DICTIONARY_AVAILABLE : array (DICTIONARY_KIND) of BOOLEAN := (FALSE,
                                                            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  --  don't SEARCH
                                                            FALSE, FALSE, FALSE, FALSE);  
    --  Start out as FALSE and set to TRUE when the DICT is loaded
   
      type AREA_TYPE is (
                        X,      --  All or none
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
   
      package AREA_TYPE_IO is new TEXT_IO.ENUMERATION_IO(AREA_TYPE);
   
      type GEO_TYPE is (
                       X,      --  All or none
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
   
      package GEO_TYPE_IO is new TEXT_IO.ENUMERATION_IO(GEO_TYPE);
   
        type SOURCE_TYPE is (
    X,      --  General or unknown or too common to say
    A,      
    B,      --  C.H.Beeson, A Primer of Medieval Latin, 1925 (Bee)
    C,      --  Charles Beard, Cassell's Latin Dictionary 1892 (Cas)       
    D,      --  J.N.Adams, Latin Sexual Vocabulary, 1982 (Sex)
    E,      --  L.F.Stelten, Dictionary of Eccles. Latin, 1995 (Ecc)
    F,      --  Roy J. Deferrari, Dictionary of St. Thomas Aquinas, 1960 (DeF)
    G,      --  Gildersleeve + Lodge, Latin Grammar 1895 (G+L)
    H,      --  Collatinus Dictionary by Yves Ouvrard 
    I,      --  Leverett, F.P., Lexicon of the Latin Language, Boston 1845
    J,      --  Bracton: De Legibus Et Consuetudinibus Angliæ
    K,      --  Calepinus Novus, modern Latin, by Guy Licoppe (Cal)
    L,      --  Lewis, C.S., Elementary Latin Dictionary 1891
    M,      --  Latham, Revised Medieval Word List, 1980 (Latham)
    N,      --  Lynn Nelson, Wordlist (Nel)
    O,      --  Oxford Latin Dictionary, 1982 (OLD)
    P,      --  Souter, A Glossary of Later Latin to 600 A.D., Oxford 1949 (Souter)
    Q,      --  Other, cited or unspecified dictionaries
    R,      --  Plater + White, A Grammar of the Vulgate, Oxford 1926 (Plater)
    S,      --  Lewis and Short, A Latin Dictionary, 1879 (L+S)
    T,      --  Found in a translation  --  no dictionary reference
    U,      --              
    V,      --  Vademecum in opus Saxonis - Franz Blatt (Saxo)
    W,      --  My personal guess, mostly obvious extrapolation (Whitaker or W)   
    Y,      --  Temp special code
    Z       --  Sent by user --  no dictionary reference
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

   
      package SOURCE_TYPE_IO is new TEXT_IO.ENUMERATION_IO(SOURCE_TYPE);
   
   
      type KIND_ENTRY(POFS : PART_OF_SPEECH_TYPE := X) is
         record
            case POFS is
               when N =>
                  N_KIND : NOUN_KIND_TYPE := X;
               when PRON =>
                  PRON_KIND : PRONOUN_KIND_TYPE := X;
               when PACK =>
                  PACK_KIND : PRONOUN_KIND_TYPE := X;
               when ADJ =>
               null;
               when NUM =>
                  NUM_VALUE : NUMERAL_VALUE_TYPE := 0;
               when V =>
                  V_KIND : VERB_KIND_TYPE := X;
               when VPAR =>
                  VPAR_KIND : VERB_KIND_TYPE := X;
               when SUPINE =>
                  SUPINE_KIND : VERB_KIND_TYPE := X;
               when others =>
               null;                              
            end case;
         end record;
   
      package KIND_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; 
                       PS : in PART_OF_SPEECH_TYPE; P : out KIND_ENTRY);
         procedure GET(PS : in PART_OF_SPEECH_TYPE; P : out KIND_ENTRY);
         procedure PUT(F : in FILE_TYPE; 
                       PS : in PART_OF_SPEECH_TYPE; P : in KIND_ENTRY);
         procedure PUT(PS : in PART_OF_SPEECH_TYPE; P : in KIND_ENTRY);
         procedure GET(S : in STRING; PS : in PART_OF_SPEECH_TYPE; 
                       P : out KIND_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING;
                       PS : in PART_OF_SPEECH_TYPE; P : in KIND_ENTRY);  
      end KIND_ENTRY_IO;  
   
      NULL_KIND_ENTRY : KIND_ENTRY;
   
   
   
   
      type TRANSLATION_RECORD is
         record
            AGE  : AGE_TYPE := X;
            AREA : AREA_TYPE := X;
            GEO  : GEO_TYPE := X;
            FREQ : FREQUENCY_TYPE := X;
            SOURCE : SOURCE_TYPE := X;
         end record;
   
      NULL_TRANSLATION_RECORD : TRANSLATION_RECORD;
   
      package TRANSLATION_RECORD_IO is
         DEFAULT_WIDTH : TEXT_IO.FIELD;
         procedure GET(F : in TEXT_IO.FILE_TYPE; TR : out TRANSLATION_RECORD);
         procedure GET(TR : out TRANSLATION_RECORD);
         procedure PUT(F : in TEXT_IO.FILE_TYPE; TR : in TRANSLATION_RECORD);
         procedure PUT(TR : in TRANSLATION_RECORD);
         procedure GET(S : in STRING; TR : out TRANSLATION_RECORD; LAST : out INTEGER);
         procedure PUT(S : out STRING; TR : in TRANSLATION_RECORD);  
      end TRANSLATION_RECORD_IO;   
   
   
   
   
      type NOUN_ENTRY is
         record
            DECL   : DECN_RECORD := (0, 0);
            GENDER : GENDER_TYPE := X;
            KIND   : NOUN_KIND_TYPE := X;
         end record;
   
      package NOUN_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; N : out NOUN_ENTRY);
         procedure GET(N : out NOUN_ENTRY);
         procedure PUT(F : in FILE_TYPE; N : in NOUN_ENTRY);
         procedure PUT(N : in NOUN_ENTRY);
         procedure GET(S : in STRING; N : out NOUN_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; N : in NOUN_ENTRY);  
      end NOUN_ENTRY_IO;  
   
   
   
      type PRONOUN_ENTRY is
         record
            DECL  : DECN_RECORD := (0,0);
            KIND : PRONOUN_KIND_TYPE := X;
         end record;
   
      package PRONOUN_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; P : out PRONOUN_ENTRY);
         procedure GET(P : out PRONOUN_ENTRY);
         procedure PUT(F : in FILE_TYPE; P : in PRONOUN_ENTRY);
         procedure PUT(P : in PRONOUN_ENTRY);
         procedure GET(S : in STRING; P : out PRONOUN_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; P : in PRONOUN_ENTRY);
      end PRONOUN_ENTRY_IO;
   
   
      type PROPACK_ENTRY is
         record
            DECL  : DECN_RECORD := (0,0);
            KIND : PRONOUN_KIND_TYPE := X;
         end record;
   
      package PROPACK_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; P : out PROPACK_ENTRY);
         procedure GET(P : out PROPACK_ENTRY);
         procedure PUT(F : in FILE_TYPE; P : in PROPACK_ENTRY);
         procedure PUT(P : in PROPACK_ENTRY);
         procedure GET(S : in STRING; P : out PROPACK_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; P : in PROPACK_ENTRY);
      end PROPACK_ENTRY_IO;
   
   
   
      type ADJECTIVE_ENTRY is
         record
            DECL : DECN_RECORD := (0, 0);
            CO   : COMPARISON_TYPE := X;
         end record;
   
      package ADJECTIVE_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; A : out ADJECTIVE_ENTRY);
         procedure GET(A : out ADJECTIVE_ENTRY);
         procedure PUT(F : in FILE_TYPE; A : in ADJECTIVE_ENTRY);
         procedure PUT(A : in ADJECTIVE_ENTRY);
         procedure GET(S : in STRING; A : out ADJECTIVE_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; A : in ADJECTIVE_ENTRY);  
      end ADJECTIVE_ENTRY_IO;  
   
   
      type NUMERAL_ENTRY is
         record
            DECL  : DECN_RECORD := (0,0);
            SORT  : NUMERAL_SORT_TYPE := X;
            VALUE : NUMERAL_VALUE_TYPE := 0;
         end record;
   
      package NUMERAL_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; NUM : out NUMERAL_ENTRY);
         procedure GET(NUM : out NUMERAL_ENTRY);
         procedure PUT(F : in FILE_TYPE; NUM : in NUMERAL_ENTRY);
         procedure PUT(NUM : in NUMERAL_ENTRY);
         procedure GET(S : in STRING; NUM : out NUMERAL_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; NUM : in NUMERAL_ENTRY);  
      end NUMERAL_ENTRY_IO;  
   
   
   
      type ADVERB_ENTRY is
         record
            CO   : COMPARISON_TYPE := X;
         end record;
   
   
   
      package ADVERB_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; A : out ADVERB_ENTRY);
         procedure GET(A : out ADVERB_ENTRY);
         procedure PUT(F : in FILE_TYPE; A : in ADVERB_ENTRY);
         procedure PUT(A : in ADVERB_ENTRY);
         procedure GET(S : in STRING; A : out ADVERB_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; A : in ADVERB_ENTRY);  
      end ADVERB_ENTRY_IO;  
   
   
      type VERB_ENTRY is
         record
            CON  : DECN_RECORD := (0,0);
            KIND : VERB_KIND_TYPE := X;
         end record;
   
      package VERB_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; V : out VERB_ENTRY);
         procedure GET(V : out VERB_ENTRY);
         procedure PUT(F : in FILE_TYPE; V : in VERB_ENTRY);
         procedure PUT(V : in VERB_ENTRY);
         procedure GET(S : in STRING; V : out VERB_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; V : in VERB_ENTRY);  
      end VERB_ENTRY_IO;  
   
   
      type PREPOSITION_ENTRY is
         record
            OBJ : CASE_TYPE := X;
         end record;
   
      package PREPOSITION_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; P : out PREPOSITION_ENTRY);
         procedure GET(P : out PREPOSITION_ENTRY);
         procedure PUT(F : in FILE_TYPE; P : in PREPOSITION_ENTRY);
         procedure PUT(P : in PREPOSITION_ENTRY);
         procedure GET(S : in STRING; P : out PREPOSITION_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; P : in PREPOSITION_ENTRY);  
      end PREPOSITION_ENTRY_IO;  
   
   
      type CONJUNCTION_ENTRY is
         record
         null;
         end record;
   
      package CONJUNCTION_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; C : out CONJUNCTION_ENTRY);
         procedure GET(C : out CONJUNCTION_ENTRY);
         procedure PUT(F : in FILE_TYPE; C : in CONJUNCTION_ENTRY);
         procedure PUT(C : in CONJUNCTION_ENTRY);
         procedure GET(S : in STRING; C : out CONJUNCTION_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; C : in CONJUNCTION_ENTRY);  
      end CONJUNCTION_ENTRY_IO;  
   
   
      type INTERJECTION_ENTRY is
         record
         null;
         end record;
   
      package INTERJECTION_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; I : out INTERJECTION_ENTRY);
         procedure GET(I : out INTERJECTION_ENTRY);
         procedure PUT(F : in FILE_TYPE; I : in INTERJECTION_ENTRY);
         procedure PUT(I : in INTERJECTION_ENTRY);
         procedure GET(S : in STRING; I : out INTERJECTION_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; I : in INTERJECTION_ENTRY);  
      end INTERJECTION_ENTRY_IO;  
   
   
      type PART_ENTRY(POFS : PART_OF_SPEECH_TYPE := X) is
         record
            case POFS is
               when N =>
                  N : NOUN_ENTRY;
               when PRON =>
                  PRON : PRONOUN_ENTRY;
               when PACK =>
                  PACK : PROPACK_ENTRY;
               when ADJ =>
                  ADJ : ADJECTIVE_ENTRY;
               when NUM =>
                  NUM : NUMERAL_ENTRY;
               when ADV =>
                  ADV : ADVERB_ENTRY;
               when V =>
                  V : VERB_ENTRY;
               when VPAR =>
               null;                 --  There will be no VPAR dictionary entries
               when SUPINE =>
               null;                 --  There will be no SUPINE dictionary entries
               when PREP =>
                  PREP : PREPOSITION_ENTRY;
               when CONJ =>
                  CONJ : CONJUNCTION_ENTRY;
               when INTERJ =>
                  INTERJ : INTERJECTION_ENTRY;
               when others =>
               null;
            end case;
         end record;
   
      package PART_ENTRY_IO is
         DEFAULT_WIDTH : NATURAL;
         procedure GET(F : in FILE_TYPE; P : out PART_ENTRY);
         procedure GET(P : out PART_ENTRY);
         procedure PUT(F : in FILE_TYPE; P : in PART_ENTRY);
         procedure PUT(P : in PART_ENTRY);
         procedure GET(S : in STRING; P : out PART_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; P : in PART_ENTRY);  
      end PART_ENTRY_IO;  
   
      NULL_PART_ENTRY : PART_ENTRY;
      
      
      function "<" (LEFT, RIGHT : PART_ENTRY) return BOOLEAN;
     
   
      type DICTIONARY_ENTRY is 
         record
            STEMS : STEMS_TYPE         := NULL_STEMS_TYPE;
            PART  : PART_ENTRY         := NULL_PART_ENTRY;
--            KIND  : KIND_ENTRY         := NULL_KIND_ENTRY;  
            TRAN  : TRANSLATION_RECORD := NULL_TRANSLATION_RECORD;
            MEAN  : MEANING_TYPE       := NULL_MEANING_TYPE;
         end record;
   
      package DICTIONARY_ENTRY_IO is
         DEFAULT_WIDTH : FIELD;
         procedure GET(F : in FILE_TYPE; D : out DICTIONARY_ENTRY);
         procedure GET(D : out DICTIONARY_ENTRY);
         procedure PUT(F : in FILE_TYPE; D : in DICTIONARY_ENTRY);
         procedure PUT(D : in DICTIONARY_ENTRY);
         procedure GET(S : in STRING; D : out DICTIONARY_ENTRY; LAST : out INTEGER);
         procedure PUT(S : out STRING; D : in DICTIONARY_ENTRY);  
      end DICTIONARY_ENTRY_IO;  
   
      NULL_DICTIONARY_ENTRY : DICTIONARY_ENTRY;
   
      package DICT_IO is new DIRECT_IO(DICTIONARY_ENTRY);
      DICT_FILE : array (DICTIONARY_KIND) of DICT_IO.FILE_TYPE;
   
   
      package MNPC_IO is new TEXT_IO.INTEGER_IO(DICT_IO.COUNT); 
      subtype MNPC_TYPE is DICT_IO.COUNT;
      NULL_MNPC : DICT_IO.COUNT := DICT_IO.COUNT'FIRST;
   
   
      type PARSE_RECORD is
         record
            STEM  : STEM_TYPE := NULL_STEM_TYPE;
            IR    : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
            D_K   : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
            MNPC  : DICT_IO.COUNT := NULL_MNPC;
         end record;
   
      NULL_PARSE_RECORD : PARSE_RECORD;
   
      package PARSE_RECORD_IO is
         DEFAULT_WIDTH : TEXT_IO.FIELD;
         procedure GET(F : in TEXT_IO.FILE_TYPE; PR : out PARSE_RECORD);
         procedure GET(PR : out PARSE_RECORD);
         procedure PUT(F : in TEXT_IO.FILE_TYPE; PR : in PARSE_RECORD);
         procedure PUT(PR : in PARSE_RECORD);
         procedure GET(S : in STRING; PR : out PARSE_RECORD; LAST : out INTEGER);
         procedure PUT(S : out STRING; PR : in PARSE_RECORD);  
      end PARSE_RECORD_IO;  
   
   
      type PARSE_ARRAY is array (INTEGER range <>) of PARSE_RECORD;
   
   
      function NUMBER_OF_STEMS(P : PART_OF_SPEECH_TYPE) return STEM_KEY_TYPE;
   
      function "<=" (LEFT, RIGHT : AREA_TYPE) return BOOLEAN;  
   
   end DICTIONARY_PACKAGE;


