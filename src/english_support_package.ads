   with Text_IO;   
   with Direct_IO; 
   with Strings_Package; use Strings_Package;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with Inflections_Package; use Inflections_Package;
   with Dictionary_Package; use Dictionary_Package;
   with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
   package ENGLISH_SUPPORT_PACKAGE is 
  
      EWORD_SIZE    : constant := 24;
      AUX_WORD_SIZE : constant := 12;
      LINE_NUMBER_WIDTH : constant := 10;
      PRIORITY_WIDTH : constant := 3;
      
      subtype EWORD is STRING(1..EWORD_SIZE);
      NULL_EWORD : EWORD := (others => ' ');
      subtype AUXWORD is STRING(1..AUX_WORD_SIZE);
      NULL_AUXWORD : AUXWORD := (others => ' ');
      subtype PRIORITY_TYPE is INTEGER range 0..99;
      
      NUMBER_OF_EWORDS : INTEGER := 0;
       
      type EWDS_RECORD is 
        record
          W    : EWORD := NULL_EWORD;
          AUX  : AUXWORD := NULL_AUXWORD;
          N    : INTEGER := 0;
          POFS : PART_OF_SPEECH_TYPE := X;
          FREQ : FREQUENCY_TYPE := X;
          SEMI : INTEGER := 0;
          KIND : INTEGER := 0;
          RANK : INTEGER := 0;
        end record;
      
      NULL_EWDS_RECORD : EWDS_RECORD := ((others => ' '), 
                         (others => ' '), 0, X, X, 0, 0, 0);
     
      type EWDS_ARRAY is array (POSITIVE range <>) of EWDS_RECORD;
     
      package EWDS_DIRECT_IO is new DIRECT_IO(EWDS_RECORD);
      
      package EWDS_RECORD_IO is 
        DEFAULT_WIDTH : NATURAL;
        procedure GET(F : in TEXT_IO.FILE_TYPE; P : out EWDS_RECORD);
        procedure GET(P : out EWDS_RECORD);
        procedure PUT(F : in TEXT_IO.FILE_TYPE; P : in EWDS_RECORD);
        procedure PUT(P : in EWDS_RECORD);
        procedure GET(S : in STRING; P : out EWDS_RECORD; 
                                  LAST : out INTEGER);
        procedure PUT(S : out STRING; P : in EWDS_RECORD);  
      end EWDS_RECORD_IO; 
      
  ENGLISH_DICTIONARY_AVAILABLE : array (DICTIONARY_KIND) of BOOLEAN := (FALSE,
                                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  --  don't SEARCH
                                 FALSE, FALSE, FALSE, FALSE);  

  EWDS_FILE : EWDS_DIRECT_IO.FILE_TYPE;
      
                 

end ENGLISH_SUPPORT_PACKAGE;

