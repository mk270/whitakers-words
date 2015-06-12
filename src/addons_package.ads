with TEXT_IO; 
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package ADDONS_PACKAGE is
  use TEXT_IO;

  subtype FIX_TYPE is STEM_TYPE;
  NULL_FIX_TYPE : constant FIX_TYPE := NULL_STEM_TYPE;
  MAX_FIX_SIZE : constant := MAX_STEM_SIZE;

  subtype TARGET_POFS_TYPE is PART_OF_SPEECH_TYPE range X..V;  


  type TARGET_ENTRY(POFS: TARGET_POFS_TYPE := X) is
    record
      case POFS is
        when N  => 
          N : NOUN_ENTRY;
          --NOUN_KIND : NOUN_KIND_TYPE;
        when PRON  =>
          PRON : PRONOUN_ENTRY;
          --PRONOUN_KIND : PRONOUN_KIND_TYPE;
        when PACK  =>
          PACK : PROPACK_ENTRY;
          --PROPACK_KIND : PRONOUN_KIND_TYPE;
        when ADJ  => 
          ADJ : ADJECTIVE_ENTRY;
        when NUM  => 
          NUM : NUMERAL_ENTRY;
          --NUMERAL_VALUE : NUMERAL_VALUE_TYPE;
        when ADV  => 
          ADV : ADVERB_ENTRY;
        when V  => 
          V : VERB_ENTRY;
          --VERB_KIND : VERB_KIND_TYPE;
        when others  =>
          null;
      end case;
    end record;                                        

  NULL_TARGET_ENTRY : TARGET_ENTRY;

 package TARGET_ENTRY_IO is
   DEFAULT_WIDTH : NATURAL;
   procedure GET(F : in FILE_TYPE; P : out TARGET_ENTRY);
   procedure GET(P : out TARGET_ENTRY);
   procedure PUT(F : in FILE_TYPE; P : in TARGET_ENTRY);
   procedure PUT(P : in TARGET_ENTRY);
   procedure GET(S : in STRING; P : out TARGET_ENTRY; LAST : out INTEGER);
   procedure PUT(S : out STRING; P : in TARGET_ENTRY);  
 end TARGET_ENTRY_IO;  

  type TACKON_ENTRY is
    record
      BASE : TARGET_ENTRY;
    end record;

  NULL_TACKON_ENTRY : TACKON_ENTRY; 
 
  package TACKON_ENTRY_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; I : out TACKON_ENTRY);
    procedure GET(I : out TACKON_ENTRY);
    procedure PUT(F : in FILE_TYPE; I : in TACKON_ENTRY);
    procedure PUT(I : in TACKON_ENTRY);
    procedure GET(S : in STRING; I : out TACKON_ENTRY; LAST : out INTEGER);
    procedure PUT(S : out STRING; I : in TACKON_ENTRY);  
  end TACKON_ENTRY_IO;  


  type PREFIX_ENTRY is 
    record
      ROOT    : PART_OF_SPEECH_TYPE := X;
      TARGET  : PART_OF_SPEECH_TYPE := X; 
    end record;

  NULL_PREFIX_ENTRY : PREFIX_ENTRY;

 package PREFIX_ENTRY_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out PREFIX_ENTRY);
    procedure GET(P : out PREFIX_ENTRY);
    procedure PUT(F : in FILE_TYPE; P : in PREFIX_ENTRY);
    procedure PUT(P : in PREFIX_ENTRY);
    procedure GET(S : in STRING; P : out PREFIX_ENTRY; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in PREFIX_ENTRY);  
  end PREFIX_ENTRY_IO;  
 
 
  type SUFFIX_ENTRY is 
    record
      ROOT       : PART_OF_SPEECH_TYPE := X;
      ROOT_KEY   : STEM_KEY_TYPE := 0;
      TARGET     : TARGET_ENTRY := NULL_TARGET_ENTRY; 
      TARGET_KEY : STEM_KEY_TYPE := 0;
    end record;

  NULL_SUFFIX_ENTRY : SUFFIX_ENTRY; 

  package SUFFIX_ENTRY_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out SUFFIX_ENTRY);
    procedure GET(P : out SUFFIX_ENTRY);
    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_ENTRY);
    procedure PUT(P : in SUFFIX_ENTRY);
    procedure GET(S : in STRING; P : out SUFFIX_ENTRY; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in SUFFIX_ENTRY);  
  end SUFFIX_ENTRY_IO;  

 type TACKON_ITEM is
    record
      POFS: PART_OF_SPEECH_TYPE := TACKON;
      TACK : STEM_TYPE := NULL_STEM_TYPE;
      ENTR : TACKON_ENTRY := NULL_TACKON_ENTRY;
      MNPC : INTEGER := 0;
    end record;

  NULL_TACKON_ITEM : TACKON_ITEM;


  type PREFIX_ITEM is
    record
      POFS: PART_OF_SPEECH_TYPE := PREFIX;
      FIX  : FIX_TYPE := NULL_FIX_TYPE;
      CONNECT : CHARACTER := ' ';
      ENTR : PREFIX_ENTRY := NULL_PREFIX_ENTRY;
      MNPC : INTEGER := 0;
    end record;
  
  NULL_PREFIX_ITEM : PREFIX_ITEM;
 

  type SUFFIX_ITEM is
    record
      POFS: PART_OF_SPEECH_TYPE := SUFFIX;
      FIX  : FIX_TYPE := NULL_FIX_TYPE;
      CONNECT    : CHARACTER := ' ';
      ENTR : SUFFIX_ENTRY := NULL_SUFFIX_ENTRY;
      MNPC : INTEGER := 0;
    end record;

  NULL_SUFFIX_ITEM : SUFFIX_ITEM;

 
  type PREFIX_ARRAY is array (INTEGER range <>) of PREFIX_ITEM; 
  type TICKON_ARRAY is array (INTEGER range <>) of PREFIX_ITEM; 
  type SUFFIX_ARRAY is array (INTEGER range <>) of SUFFIX_ITEM; 
  type TACKON_ARRAY is array (INTEGER range <>) of TACKON_ITEM; 
  type MEANS_ARRAY  is array (INTEGER range <>) of MEANING_TYPE;
    --  To simulate a DICT_IO file, as used previously
  
  TACKONS  : TACKON_ARRAY(1..20);
  PACKONS  : TACKON_ARRAY(1..25);
  TICKONS  : PREFIX_ARRAY(1..10);
  PREFIXES : PREFIX_ARRAY(1..130);
  SUFFIXES : SUFFIX_ARRAY(1..185);
  MEANS    : MEANS_ARRAY(1..370);
  
  NUMBER_OF_TICKONS  : INTEGER := 0;
  NUMBER_OF_TACKONS  : INTEGER := 0;
  NUMBER_OF_PACKONS  : INTEGER := 0;
  NUMBER_OF_PREFIXES : INTEGER := 0;
  NUMBER_OF_SUFFIXES : INTEGER := 0;

  procedure LOAD_ADDONS (FILE_NAME : in STRING);   

  function SUBTRACT_TACKON(W : STRING; X : TACKON_ITEM) return STRING;      
  function SUBTRACT_PREFIX(W : STRING; X : PREFIX_ITEM) return STEM_TYPE;  
  function SUBTRACT_TICKON(W : STRING; X : PREFIX_ITEM) return STEM_TYPE 
   renames SUBTRACT_PREFIX;
  function SUBTRACT_SUFFIX(W : STRING; X : SUFFIX_ITEM) return STEM_TYPE;   
  
  function ADD_PREFIX(STEM : STEM_TYPE; 
                    PREFIX : PREFIX_ITEM) return STEM_TYPE;  
  function ADD_SUFFIX(STEM : STEM_TYPE; 
                    SUFFIX : SUFFIX_ITEM) return STEM_TYPE;   


end ADDONS_PACKAGE;
