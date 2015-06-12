with TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with UNIQUES_PACKAGE; use UNIQUES_PACKAGE;
package LINE_STUFF is
  use TEXT_IO;



  type DICTIONARY_ITEM;
  type DICTIONARY_LIST is access DICTIONARY_ITEM;
  type DICTIONARY_ITEM is
    record
      DE   : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
      SUCC : DICTIONARY_LIST;
    end record;

  type DICTIONARY is array (CHARACTER) of DICTIONARY_LIST;
  NULL_DICTIONARY : DICTIONARY := (others => null);
  --DICT, UNIQUES, QUES : DICTIONARY := NULL_DICTIONARY;  
  DICT, UNIQUES : DICTIONARY := NULL_DICTIONARY;

  DICT_LOC : DICTIONARY := NULL_DICTIONARY;

 type TACKON_LINE is
    record
      POFS : PART_OF_SPEECH_TYPE := TACKON;
      TACK : STEM_TYPE := NULL_STEM_TYPE;
      ENTR : TACKON_ENTRY := NULL_TACKON_ENTRY;
      MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    end record;

  NULL_TACKON_LINE : TACKON_LINE;

  package TACKON_LINE_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out TACKON_LINE);
    procedure GET(P : out TACKON_LINE);
    procedure PUT(F : in FILE_TYPE; P : in TACKON_LINE);
    procedure PUT(P : in TACKON_LINE);
    procedure GET(S : in STRING; P : out TACKON_LINE; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in TACKON_LINE);
  end TACKON_LINE_IO;


  type PREFIX_LINE is
    record
      POFS : PART_OF_SPEECH_TYPE := PREFIX;
      FIX  : FIX_TYPE := NULL_FIX_TYPE;
      CONNECT : CHARACTER := ' ';
      ENTR : PREFIX_ENTRY := NULL_PREFIX_ENTRY;
      MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    end record;

  NULL_PREFIX_LINE : PREFIX_LINE;

   package PREFIX_LINE_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out PREFIX_LINE);
    procedure GET(P : out PREFIX_LINE);
    procedure PUT(F : in FILE_TYPE; P : in PREFIX_LINE);
    procedure PUT(P : in PREFIX_LINE);
    procedure GET(S : in STRING; P : out PREFIX_LINE; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in PREFIX_LINE);
  end PREFIX_LINE_IO;

  type SUFFIX_LINE is
    record
      POFS : PART_OF_SPEECH_TYPE := SUFFIX;
      FIX  : FIX_TYPE := NULL_FIX_TYPE;
      CONNECT    : CHARACTER := ' ';
      ENTR : SUFFIX_ENTRY := NULL_SUFFIX_ENTRY;
      MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    end record;

  NULL_SUFFIX_LINE : SUFFIX_LINE;

  package SUFFIX_LINE_IO is
    DEFAULT_WIDTH : NATURAL;
    procedure GET(F : in FILE_TYPE; P : out SUFFIX_LINE);
    procedure GET(P : out SUFFIX_LINE);
    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_LINE);
    procedure PUT(P : in SUFFIX_LINE);
    procedure GET(S : in STRING; P : out SUFFIX_LINE; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in SUFFIX_LINE);
  end SUFFIX_LINE_IO;

  type UNIQUE_ENTRY is
    record
      STEM : STEM_TYPE          := NULL_STEM_TYPE;
      QUAL : QUALITY_RECORD     := NULL_QUALITY_RECORD;
      KIND : KIND_ENTRY         := NULL_KIND_ENTRY;
      TRAN : TRANSLATION_RECORD := NULL_TRANSLATION_RECORD;
    end record;

  package UNIQUE_ENTRY_IO is
    DEFAULT_WIDTH : FIELD;
    procedure GET(F : in FILE_TYPE; P : out UNIQUE_ENTRY);
    procedure GET(P : out UNIQUE_ENTRY);
    procedure PUT(F : in FILE_TYPE; P : in UNIQUE_ENTRY);
    procedure PUT(P : in UNIQUE_ENTRY);
    procedure GET(S : in STRING; P : out UNIQUE_ENTRY; LAST : out INTEGER);
    procedure PUT(S : out STRING; P : in UNIQUE_ENTRY);
  end UNIQUE_ENTRY_IO;

  procedure LOAD_STEM_FILE(D_K : DICTIONARY_KIND);

  procedure LOAD_DICTIONARY(DICT : in out DICTIONARY;
                            DICTIONARY_FILE_NAME : STRING);

  procedure LOAD_UNIQUES(UNQ : in out LATIN_UNIQUES; FILE_NAME : in STRING);

 end LINE_STUFF;
