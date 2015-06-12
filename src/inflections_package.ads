with TEXT_IO; 
with DIRECT_IO;
package INFLECTIONS_PACKAGE is
  package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
  use TEXT_IO;

  --  Generally simple/enumeration types have names ending in _TYPE
  --            complex/record     types have names ending in _RECORD
  --            array              types have names ending in _ARRAY
  
  MAX_STEM_SIZE    : constant := 18;
  MAX_MEANING_SIZE : constant := 80;

  subtype STEM_TYPE is STRING(1..MAX_STEM_SIZE);
  NULL_STEM_TYPE : constant STEM_TYPE := (others => ' ');

  package STEM_TYPE_IO is
    DEFAULT_WIDTH : NATURAL := MAX_STEM_SIZE;
    procedure GET(F : in FILE_TYPE; D : out STEM_TYPE);
    procedure GET(D : out STEM_TYPE);
    procedure PUT(F : in FILE_TYPE; D : in STEM_TYPE);
    procedure PUT(D : in STEM_TYPE);
    procedure GET(S : in STRING; D : out STEM_TYPE; 
                                 LAST : out INTEGER);
    procedure PUT(S : out STRING; D : in STEM_TYPE);  
  end STEM_TYPE_IO;  
  
  
  subtype MEANING_TYPE is STRING(1..MAX_MEANING_SIZE);
  NULL_MEANING_TYPE : constant MEANING_TYPE := (others => ' ');

  type PART_OF_SPEECH_TYPE is (
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
                                );                                   

  package PART_OF_SPEECH_TYPE_IO is 
      new TEXT_IO.ENUMERATION_IO(PART_OF_SPEECH_TYPE);


  subtype WHICH_TYPE is NATURAL range 0..9;

  subtype VARIANT_TYPE is NATURAL range 0..9;

  WHICH_TYPE_IO_DEFAULT_WIDTH : INTEGER := 1;
  VARIANT_TYPE_IO_DEFAULT_WIDTH : INTEGER := 1;
  
  type DECN_RECORD is
    record
      WHICH        : WHICH_TYPE := 0;
      VAR          : VARIANT_TYPE := 0;
    end record;

   function "<" (LEFT, RIGHT : DECN_RECORD) return BOOLEAN;
    
    package DECN_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; D : out DECN_RECORD);
    procedure GET(D : out DECN_RECORD);
    procedure PUT(F : in FILE_TYPE; D : in DECN_RECORD);
    procedure PUT(D : in DECN_RECORD);
    procedure GET(S : in STRING; D : out DECN_RECORD; 
                                 LAST : out INTEGER);
    procedure PUT(S : out STRING; D : in DECN_RECORD);  
  end DECN_RECORD_IO;  


  type GENDER_TYPE is (
          X,         --  all, none, or unknown
          M,         --  Masculine
          F,         --  Feminine
          N,         --  Neuter
          C          --  Common (masculine and/or feminine)
                       );

  package GENDER_TYPE_IO is new TEXT_IO.ENUMERATION_IO(GENDER_TYPE);

  type CASE_TYPE is (
          X,         --  all, none, or unknown
          NOM,       --  NOMinative
          VOC,       --  VOCative
          GEN,       --  GENitive
          LOC,       --  LOCative
          DAT,       --  DATive
          ABL,       --  ABLative
          ACC        --  ACCusitive
                     );

  package CASE_TYPE_IO is new TEXT_IO.ENUMERATION_IO(CASE_TYPE);
  
  type NUMBER_TYPE is (
          X,         --  all, none, or unknown
          S,         --  Singular
          P          --  Plural
                       );

  package NUMBER_TYPE_IO is new TEXT_IO.ENUMERATION_IO(NUMBER_TYPE);
  
  type PERSON_TYPE is range 0..3;
  package PERSON_TYPE_IO is new TEXT_IO.INTEGER_IO(PERSON_TYPE);
  
  type COMPARISON_TYPE is (
          X,         --  all, none, or unknown
          POS,       --  POSitive
          COMP,      --  COMParative
          SUPER      --  SUPERlative
                           );   

  package COMPARISON_TYPE_IO is new TEXT_IO.ENUMERATION_IO(COMPARISON_TYPE);

  type STEM_KEY_TYPE is new NATURAL range 0..9;
    
  package STEM_KEY_TYPE_IO is new TEXT_IO.INTEGER_IO(STEM_KEY_TYPE);
  STEM_KEY_TYPE_IO_DEFAULT_WIDTH : INTEGER := 1;
  
  
  
  type NUMERAL_SORT_TYPE is (
         X,          --  all, none, or unknown
         CARD,       --  CARDinal
         ORD,        --  ORDinal
         DIST,       --  DISTributive
         ADVERB      --  numeral ADVERB
                            );


  package NUMERAL_SORT_TYPE_IO is 
    new TEXT_IO.ENUMERATION_IO(NUMERAL_SORT_TYPE);
 
 
   
  type TENSE_TYPE is (
          X,         --  all, none, or unknown
          PRES,      --  PRESent
          IMPF,      --  IMPerFect
          FUT,       --  FUTure
          PERF,      --  PERFect
          PLUP,      --  PLUPerfect
          FUTP       --  FUTure Perfect
                      );                        

  package TENSE_TYPE_IO is new TEXT_IO.ENUMERATION_IO(TENSE_TYPE);
  
  type VOICE_TYPE is (
          X,         --  all, none, or unknown
          ACTIVE,    --  ACTIVE
          PASSIVE    --  PASSIVE
                      );       
  
  package VOICE_TYPE_IO is new TEXT_IO.ENUMERATION_IO(VOICE_TYPE);
  
  type MOOD_TYPE is (
          X,         --  all, none, or unknown
          IND,       --  INDicative
          SUB,       --  SUBjunctive
          IMP,       --  IMPerative
          INF,       --  INFinative
          PPL        --  ParticiPLe
                     );                    
                                        
  package MOOD_TYPE_IO is new TEXT_IO.ENUMERATION_IO(MOOD_TYPE);
  
  type TENSE_VOICE_MOOD_RECORD is 
    record
      TENSE : TENSE_TYPE := X;
      VOICE : VOICE_TYPE := X;
      MOOD  : MOOD_TYPE  := X;
    end record;
  
  package TENSE_VOICE_MOOD_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; T : out TENSE_VOICE_MOOD_RECORD);
    procedure GET(T : out TENSE_VOICE_MOOD_RECORD);
    procedure PUT(F : in FILE_TYPE; T : in TENSE_VOICE_MOOD_RECORD);
    procedure PUT(T : in TENSE_VOICE_MOOD_RECORD);
    procedure GET(S : in STRING; T : out TENSE_VOICE_MOOD_RECORD;
                                 LAST : out INTEGER);
    procedure PUT(S : out STRING; T : in TENSE_VOICE_MOOD_RECORD);  
  end TENSE_VOICE_MOOD_RECORD_IO;  


  type NOUN_KIND_TYPE is (
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
                           ); 
  
  package NOUN_KIND_TYPE_IO is new TEXT_IO.ENUMERATION_IO(NOUN_KIND_TYPE);

  type PRONOUN_KIND_TYPE is (
          X,            --  unknown, nondescript
          PERS,         --  PERSonal
          REL,          --  RELative
          REFLEX,       --  REFLEXive
          DEMONS,       --  DEMONStrative
          INTERR,       --  INTERRogative
          INDEF,        --  INDEFinite
          ADJECT        --  ADJECTival
                             ); 

  package PRONOUN_KIND_TYPE_IO is 
      new TEXT_IO.ENUMERATION_IO(PRONOUN_KIND_TYPE);
  
   subtype NUMERAL_VALUE_TYPE is NATURAL range 0..1000;

   NUMERAL_VALUE_TYPE_IO_DEFAULT_WIDTH : INTEGER := 5;
  
  
   type VERB_KIND_TYPE is (
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
                          );             

  package VERB_KIND_TYPE_IO is 
      new TEXT_IO.ENUMERATION_IO(VERB_KIND_TYPE);

  
type NOUN_RECORD is
  record
    DECL        : DECN_RECORD;
    CS          : CASE_TYPE := X;
    NUMBER      : NUMBER_TYPE := X;
    GENDER      : GENDER_TYPE := X;
  end record;
 
 package NOUN_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; N : out NOUN_RECORD);
   procedure GET(N : out NOUN_RECORD);
   procedure PUT(F : in FILE_TYPE; N : in NOUN_RECORD);
   procedure PUT(N : in NOUN_RECORD);
   procedure GET(S : in STRING; N : out NOUN_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; N : in NOUN_RECORD);  
 end NOUN_RECORD_IO;  


type PRONOUN_RECORD is
  record
    DECL        : DECN_RECORD;
    CS          : CASE_TYPE := X;
    NUMBER      : NUMBER_TYPE := X;
    GENDER      : GENDER_TYPE := X;
  end record;

 package PRONOUN_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; P : out PRONOUN_RECORD);
   procedure GET(P : out PRONOUN_RECORD);
   procedure PUT(F : in FILE_TYPE; P : in PRONOUN_RECORD);
   procedure PUT(P : in PRONOUN_RECORD);
   procedure GET(S : in STRING; P : out PRONOUN_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; P : in PRONOUN_RECORD);  
 end PRONOUN_RECORD_IO;  


type PROPACK_RECORD is
  record
    DECL        : DECN_RECORD;
    CS          : CASE_TYPE := X;
    NUMBER      : NUMBER_TYPE := X;
    GENDER      : GENDER_TYPE := X;
  end record;

 package PROPACK_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; P : out PROPACK_RECORD);
   procedure GET(P : out PROPACK_RECORD);
   procedure PUT(F : in FILE_TYPE; P : in PROPACK_RECORD);
   procedure PUT(P : in PROPACK_RECORD);
   procedure GET(S : in STRING; P : out PROPACK_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; P : in PROPACK_RECORD);  
 end PROPACK_RECORD_IO;  


type ADJECTIVE_RECORD is
  record
    DECL        : DECN_RECORD;
    CS          : CASE_TYPE := X;
    NUMBER      : NUMBER_TYPE := X;
    GENDER      : GENDER_TYPE := X;
    CO          : COMPARISON_TYPE := X;
  end record;

 package ADJECTIVE_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; A : out ADJECTIVE_RECORD);
   procedure GET(A : out ADJECTIVE_RECORD);
   procedure PUT(F : in FILE_TYPE; A : in ADJECTIVE_RECORD);
   procedure PUT(A : in ADJECTIVE_RECORD);
   procedure GET(S : in STRING; A : out ADJECTIVE_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; A : in ADJECTIVE_RECORD);  
 end ADJECTIVE_RECORD_IO;  


  type NUMERAL_RECORD is
   record
     DECL        : DECN_RECORD;
     CS          : CASE_TYPE := X;
     NUMBER      : NUMBER_TYPE := X;
     GENDER      : GENDER_TYPE := X;
     SORT        : NUMERAL_SORT_TYPE := X;
  end record;
 
  
  package NUMERAL_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; NUM : out NUMERAL_RECORD);
    procedure GET(NUM : out NUMERAL_RECORD);
    procedure PUT(F : in FILE_TYPE; NUM : in NUMERAL_RECORD);
    procedure PUT(NUM : in NUMERAL_RECORD);
    procedure GET(S : in STRING; NUM : out NUMERAL_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; NUM : in NUMERAL_RECORD);  
  end NUMERAL_RECORD_IO;  
  
type ADVERB_RECORD is
  record
    CO   : COMPARISON_TYPE := X;
  end record;

 package ADVERB_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; A : out ADVERB_RECORD);
   procedure GET(A : out ADVERB_RECORD);
   procedure PUT(F : in FILE_TYPE; A : in ADVERB_RECORD);
   procedure PUT(A : in ADVERB_RECORD);
   procedure GET(S : in STRING; A : out ADVERB_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; A : in ADVERB_RECORD);  
 end ADVERB_RECORD_IO;  


type VERB_RECORD is
  record
    CON         : DECN_RECORD;
    TENSE_VOICE_MOOD  : TENSE_VOICE_MOOD_RECORD;    
    PERSON      : PERSON_TYPE := 0;
    NUMBER      : NUMBER_TYPE := X;
  end record;

 package VERB_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; V : out VERB_RECORD);
   procedure GET(V : out VERB_RECORD);
   procedure PUT(F : in FILE_TYPE; V : in VERB_RECORD);
   procedure PUT(V : in VERB_RECORD);
   procedure GET(S : in STRING; V : out VERB_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; V : in VERB_RECORD);  
 end VERB_RECORD_IO;  


type VPAR_RECORD is
  record
    CON         : DECN_RECORD;
    CS          : CASE_TYPE := X;
    NUMBER      : NUMBER_TYPE := X;
    GENDER      : GENDER_TYPE := X;
    TENSE_VOICE_MOOD  : TENSE_VOICE_MOOD_RECORD;    
  end record;

 package VPAR_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; VP : out VPAR_RECORD);
   procedure GET(VP : out VPAR_RECORD);
   procedure PUT(F : in FILE_TYPE; VP : in VPAR_RECORD);
   procedure PUT(VP : in VPAR_RECORD);
   procedure GET(S : in STRING; VP : out VPAR_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; VP : in VPAR_RECORD);  
 end VPAR_RECORD_IO;  


type SUPINE_RECORD is
  record
    CON         : DECN_RECORD;
    CS          : CASE_TYPE := X;
    NUMBER      : NUMBER_TYPE := X;
    GENDER      : GENDER_TYPE := X;
  end record;

 package SUPINE_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; VP : out SUPINE_RECORD);
   procedure GET(VP : out SUPINE_RECORD);
   procedure PUT(F : in FILE_TYPE; VP : in SUPINE_RECORD);
   procedure PUT(VP : in SUPINE_RECORD);
   procedure GET(S : in STRING; VP : out SUPINE_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; VP : in SUPINE_RECORD);  
 end SUPINE_RECORD_IO;  


type PREPOSITION_RECORD is
  record
    OBJ : CASE_TYPE := X;
  end record;

 package PREPOSITION_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; P : out PREPOSITION_RECORD);
   procedure GET(P : out PREPOSITION_RECORD);
   procedure PUT(F : in FILE_TYPE; P : in PREPOSITION_RECORD);
   procedure PUT(P : in PREPOSITION_RECORD);
   procedure GET(S : in STRING; P : out PREPOSITION_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; P : in PREPOSITION_RECORD);  
 end PREPOSITION_RECORD_IO;  


type CONJUNCTION_RECORD is
  record
    null;
  end record;

 package CONJUNCTION_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; C : out CONJUNCTION_RECORD);
   procedure GET(C : out CONJUNCTION_RECORD);
   procedure PUT(F : in FILE_TYPE; C : in CONJUNCTION_RECORD);
   procedure PUT(C : in CONJUNCTION_RECORD);
   procedure GET(S : in STRING; C : out CONJUNCTION_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; C : in CONJUNCTION_RECORD);  
 end CONJUNCTION_RECORD_IO;  


type INTERJECTION_RECORD is
  record
    null;
  end record;
 
 package INTERJECTION_RECORD_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; I : out INTERJECTION_RECORD);
   procedure GET(I : out INTERJECTION_RECORD);
   procedure PUT(F : in FILE_TYPE; I : in INTERJECTION_RECORD);
   procedure PUT(I : in INTERJECTION_RECORD);
   procedure GET(S : in STRING; I : out INTERJECTION_RECORD; LAST : out INTEGER);
   procedure PUT(S : out STRING; I : in INTERJECTION_RECORD);  
 end INTERJECTION_RECORD_IO;  


  --  TACKON, PREFIX, SUFFIX are nulls put in to allow easy printing later

  type TACKON_RECORD is
    record
      null;
    end record;
  
  NULL_TACKON_RECORD : TACKON_RECORD;

  package TACKON_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; I : out TACKON_RECORD);
    procedure GET(I : out TACKON_RECORD);
    procedure PUT(F : in FILE_TYPE; I : in TACKON_RECORD);
    procedure PUT(I : in TACKON_RECORD);
    procedure GET(S : in STRING; I : out TACKON_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; I : in TACKON_RECORD);  
  end TACKON_RECORD_IO;  
 
  
  type PREFIX_RECORD is 
    record
      null;
    end record;

  NULL_PREFIX_RECORD : PREFIX_RECORD;

  package PREFIX_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out PREFIX_RECORD);
    procedure GET(P : out PREFIX_RECORD);
    procedure PUT(F : in FILE_TYPE; P : in PREFIX_RECORD);
    procedure PUT(P : in PREFIX_RECORD);
    procedure GET(S : in STRING; P : out PREFIX_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in PREFIX_RECORD);  
  end PREFIX_RECORD_IO;  

  type SUFFIX_RECORD is 
    record
      null;
    end record;

  NULL_SUFFIX_RECORD : SUFFIX_RECORD;
 
  package SUFFIX_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out SUFFIX_RECORD);
    procedure GET(P : out SUFFIX_RECORD);
    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_RECORD);
    procedure PUT(P : in SUFFIX_RECORD);
    procedure GET(S : in STRING; P : out SUFFIX_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in SUFFIX_RECORD);  
  end SUFFIX_RECORD_IO;  
 

  type QUALITY_RECORD(POFS : PART_OF_SPEECH_TYPE := X) is
    record
      case POFS is
        when N =>
          N : NOUN_RECORD;
        when PRON =>
          PRON : PRONOUN_RECORD;
        when PACK =>
          PACK : PROPACK_RECORD;
        when ADJ =>
          ADJ : ADJECTIVE_RECORD;
        when NUM =>
          NUM : NUMERAL_RECORD;
        when ADV =>
          ADV : ADVERB_RECORD;
        when V =>
          V : VERB_RECORD;
        when VPAR =>
          VPAR : VPAR_RECORD;
        when SUPINE =>
          SUPINE : SUPINE_RECORD;
        when PREP =>
          PREP : PREPOSITION_RECORD;
        when CONJ =>
          CONJ : CONJUNCTION_RECORD;
        when INTERJ =>
          INTERJ : INTERJECTION_RECORD;
        when TACKON =>
          TACKON : TACKON_RECORD;
        when PREFIX =>
          PREFIX : PREFIX_RECORD;
        when SUFFIX =>
          SUFFIX : SUFFIX_RECORD;
        when others =>
          null;
      end case;
    end record;

  NULL_QUALITY_RECORD : QUALITY_RECORD;

  function "<" (LEFT, RIGHT : QUALITY_RECORD) return BOOLEAN;

  package QUALITY_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out QUALITY_RECORD);
    procedure GET(P : out QUALITY_RECORD);
    procedure PUT(F : in FILE_TYPE; P : in QUALITY_RECORD);
    procedure PUT(P : in QUALITY_RECORD);
    procedure GET(S : in STRING; P : out QUALITY_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in QUALITY_RECORD);  
  end QUALITY_RECORD_IO;  

  type QUALITY_ARRAY is array (INTEGER range <>) of QUALITY_RECORD;

  MAX_ENDING_SIZE : constant := 7;     
  subtype ENDING_SIZE_TYPE is INTEGER range 0..MAX_ENDING_SIZE;

  ENDING_SIZE_TYPE_IO_DEFAULT_WIDTH : INTEGER := 3;

  subtype ENDING is STRING(1..MAX_ENDING_SIZE);

  type ENDING_RECORD is
    record
      SIZE : ENDING_SIZE_TYPE := 0;
      SUF  : ENDING := (others => ' ');
    end record;

  package ENDING_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; X : out ENDING_RECORD);
    procedure GET(X : out ENDING_RECORD);
    procedure PUT(F : in FILE_TYPE; X : in ENDING_RECORD);
    procedure PUT(X : in ENDING_RECORD);
    procedure GET(S : in STRING; X : out ENDING_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; X : in ENDING_RECORD);  
  end ENDING_RECORD_IO;  
  
  NULL_ENDING_RECORD : ENDING_RECORD;
  
  
  type AGE_TYPE is (
    X,   --              --  In use throughout the ages/unknown -- the default
    A,   --  archaic     --  Very early forms, obsolete by classical times
    B,   --  early       --  Early Latin, pre-classical, used for effect/poetry
    C,   --  classical   --  Limited to classical (~150 BC - 200 AD)
    D,   --  late        --  Late, post-classical (3rd-5th centuries)
    E,   --  later       --  Latin not in use in Classical times (6-10), Christian
    F,   --  medieval    --  Medieval (11th-15th centuries)
    G,   --  scholar     --  Latin post 15th - Scholarly/Scientific   (16-18)
    H    --  modern      --  Coined recently, words for new things (19-20)
                      );
  package AGE_TYPE_IO is new TEXT_IO.ENUMERATION_IO(AGE_TYPE);
  

  type FREQUENCY_TYPE is (     --  For dictionary entries
    X,    --              --  Unknown or unspecified
    A,    --  very freq   --  Very frequent, in all Elementry Latin books
    B,    --  frequent    --  Frequent, in top 10 percent           
    C,    --  common      --  For Dictionary, in top 10,000 words
    D,    --  lesser      --  For Dictionary, in top 20,000 words
    E,    --  uncommon    --  2 or 3 citations
    F,    --  very rare   --  Having only single citation in OLD or L+S
    I,    --  inscription --  Only citation is inscription
    M,    --  graffiti    --  Presently not much used
    N     --  Pliny       --  Things that appear (almost) only in Pliny Natural History
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


  package FREQUENCY_TYPE_IO is new TEXT_IO.ENUMERATION_IO(FREQUENCY_TYPE);
  


  type INFLECTION_RECORD is
    record
      QUAL   : QUALITY_RECORD   := NULL_QUALITY_RECORD;
      KEY    : STEM_KEY_TYPE := 0;               
      ENDING : ENDING_RECORD := NULL_ENDING_RECORD;
      AGE    : AGE_TYPE      := X;
      FREQ   : FREQUENCY_TYPE      := X;
    end record;

  NULL_INFLECTION_RECORD : INFLECTION_RECORD;

  package INFLECTION_RECORD_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out INFLECTION_RECORD);
    procedure GET(P : out INFLECTION_RECORD);
    procedure PUT(F : in FILE_TYPE; P : in INFLECTION_RECORD);
    procedure PUT(P : in INFLECTION_RECORD);
    procedure GET(S : in STRING; P : out INFLECTION_RECORD; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in INFLECTION_RECORD);  
  end INFLECTION_RECORD_IO;  
 
  --  This implies a knowledge of the inflections last character
  subtype INFLECTIONS_SECTION_1 is CHARACTER range 'a'..'i';
  subtype INFLECTIONS_SECTION_2 is CHARACTER range 'm'..'r';
  subtype INFLECTIONS_SECTION_3 is CHARACTER range 's'..'s';
  subtype INFLECTIONS_SECTION_4 is CHARACTER range 't'..'u';

  SIZE_OF_BLANK_INFLECTIONS   : constant INTEGER :=  80;    --  ############
  SIZE_OF_INFLECTIONS_SECTION : constant INTEGER := 570;    --  ############

  type INFLECTION_ARRAY is array (POSITIVE range <>) of INFLECTION_RECORD;
  subtype LEL_SECTION is INFLECTION_ARRAY(1..SIZE_OF_INFLECTIONS_SECTION);
  package LEL_SECTION_IO is new DIRECT_IO(LEL_SECTION);
  
  BEL : INFLECTION_ARRAY(1..SIZE_OF_BLANK_INFLECTIONS);

  LEL : LEL_SECTION;

  type INFLECTION_ARRAY_INDEX is array (INTEGER range <>, 
                                        CHARACTER range <>) of INTEGER;

  BELF, BELL : INFLECTION_ARRAY_INDEX(0..0, ' '..' ') := (0 => (others => 0));
  LELF, LELL : INFLECTION_ARRAY_INDEX(1..MAX_ENDING_SIZE, 
                                      'a'..'z') := (others => (others => 0));
  PELF, PELL : INFLECTION_ARRAY_INDEX(1..MAX_ENDING_SIZE, 
                                      'a'..'z') := (others => (others => 0));
  
  NUMBER_OF_INFLECTIONS : INTEGER := 0;
  
 
  procedure ESTABLISH_INFLECTIONS_SECTION;    




  --  <=   means for this purpose "contained in"

  function "<=" (LEFT, RIGHT : PART_OF_SPEECH_TYPE) return BOOLEAN;  
  function "<=" (LEFT, RIGHT : DECN_RECORD) return BOOLEAN;  
  function "<=" (LEFT, RIGHT : GENDER_TYPE) return BOOLEAN;  
  function "<=" (LEFT, RIGHT : CASE_TYPE)   return BOOLEAN;  
  function "<=" (LEFT, RIGHT : NUMBER_TYPE) return BOOLEAN;  
  function "<=" (LEFT, RIGHT : PERSON_TYPE) return BOOLEAN;  
  function "<=" (LEFT, RIGHT : COMPARISON_TYPE) return BOOLEAN;  
  function "<=" (LEFT, RIGHT : TENSE_VOICE_MOOD_RECORD)  return BOOLEAN;  
  function "<=" (LEFT, RIGHT : NOUN_KIND_TYPE)   return BOOLEAN;  
  function "<=" (LEFT, RIGHT : PRONOUN_KIND_TYPE)   return BOOLEAN;  
  function "<=" (LEFT, RIGHT : STEM_KEY_TYPE)   return BOOLEAN;  -- not verbs  
  function "<=" (LEFT, RIGHT : AGE_TYPE)   return BOOLEAN;  
  function "<=" (LEFT, RIGHT : FREQUENCY_TYPE)   return BOOLEAN;  

       
  GIVE_UP : exception;
  

end INFLECTIONS_PACKAGE;  


                   
