with TEXT_IO;
with DIRECT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package WORD_SUPPORT_PACKAGE is
  
  FOLLOWED_BY_PERIOD, FOLLOWS_PERIOD, CAPITALIZED, ALL_CAPS : 
                                                    BOOLEAN := FALSE;
  

  type DICTIONARY_STEM is
    record
      STEM : STEM_TYPE := NULL_STEM_TYPE;
      PART : PART_ENTRY := NULL_PART_ENTRY;  
      KEY  : STEM_KEY_TYPE := 0;
      MNPC : DICT_IO.COUNT := NULL_MNPC;
    end record;

  package STEM_IO is new DIRECT_IO(DICTIONARY_STEM);
  package COUNT_IO is new TEXT_IO.INTEGER_IO(STEM_IO.COUNT);

  subtype DICTIONARY_FILE_KIND is DICTIONARY_KIND range GENERAL..LOCAL;
  DEFAULT_DICTIONARY_FILE_KIND : DICTIONARY_FILE_KIND := GENERAL;

  STEM_FILE : array (DICTIONARY_FILE_KIND) of STEM_IO.FILE_TYPE;

  STEM_LIST : array (DICTIONARY_FILE_KIND) of TEXT_IO.FILE_TYPE;
  INDX_FILE : array (DICTIONARY_FILE_KIND) of TEXT_IO.FILE_TYPE;

    
  type DICT_ARRAY is array (POSITIVE range <>) of DICTIONARY_STEM; 
  BDL : DICT_ARRAY(1..100);
  BDL_LAST : INTEGER := 0;
--SIZE_OF_DICTIONARY_ARRAY : constant INTEGER := 120;    --  ###################
--DDL : DICT_ARRAY(1..SIZE_OF_DICTIONARY_ARRAY);
  type DICT_ARRAY_INDEX is array (CHARACTER range <>,
                                  CHARACTER range <>,
                       DICTIONARY_FILE_KIND range <>) of STEM_IO.COUNT;

  BBLF, BBLL : DICT_ARRAY_INDEX(' '..' ', ' '..' ', DICTIONARY_FILE_KIND) := 
                               (others => (others => (others => 0)));
  BDLF, BDLL : DICT_ARRAY_INDEX('a'..'z', ' '..' ', DICTIONARY_FILE_KIND) := 
                               (others => (others => (others => 0)));
  DDLF, DDLL : DICT_ARRAY_INDEX('a'..'z', 'a'..'z', DICTIONARY_FILE_KIND) := 
                               (others => (others => (others => 0)));
                             
    
  function ADJ_COMP_FROM_KEY(KEY : STEM_KEY_TYPE) return COMPARISON_TYPE;

  function ADV_COMP_FROM_KEY(KEY : STEM_KEY_TYPE) return COMPARISON_TYPE;

  function NUM_SORT_FROM_KEY(KEY : STEM_KEY_TYPE) return NUMERAL_SORT_TYPE;

  function EFF_PART(PART : PART_OF_SPEECH_TYPE) return PART_OF_SPEECH_TYPE;

  function LEN(S : STRING) return INTEGER;

  function FIRST_INDEX(INPUT_WORD : STRING; 
                   D_K : DICTIONARY_FILE_KIND := DEFAULT_DICTIONARY_FILE_KIND) 
                  return STEM_IO.COUNT;  

  function  LAST_INDEX(INPUT_WORD : STRING; 
              D_K : DICTIONARY_FILE_KIND := DEFAULT_DICTIONARY_FILE_KIND) 
                  return STEM_IO.COUNT;  

  procedure LOAD_INDICES_FROM_INDX_FILE(INDXFILE_NAME : STRING;
                                        D_K : DICTIONARY_KIND);  

  procedure LOAD_BDL_FROM_DISK;  


end WORD_SUPPORT_PACKAGE;
