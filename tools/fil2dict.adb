   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure FIL2DICT is 
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use TEXT_IO;
      use STEM_KEY_TYPE_IO;
      use DICTIONARY_ENTRY_IO;
      use PART_ENTRY_IO;
      use KIND_ENTRY_IO;
      use TRANSLATION_RECORD_IO;
      use AGE_TYPE_IO;
      use AREA_TYPE_IO;
      use GEO_TYPE_IO;
      use FREQUENCY_TYPE_IO;
      use SOURCE_TYPE_IO;
      use DICT_IO;
   
   
      D_K : DICTIONARY_KIND := XXX;       
      DE: DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
 
      LINE : STRING(1..200) := (others => ' ');
      LAST : INTEGER := 0;
      
      DICTFILE : DICT_IO.FILE_TYPE;
      DICTLINE : TEXT_IO.FILE_TYPE;
   
   begin
      PUT_LINE(
        "Takes a DICTFILE.D_K and reconstructs the DICTLINE.D_K it came from");
        
      PUT("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
      TEXT_IO.GET_LINE(LINE, LAST);
      if LAST > 0  then
         if TRIM(LINE(1..LAST))(1) = 'G'  or else
         TRIM(LINE(1..LAST))(1) = 'g'     then
            D_K := GENERAL;
         elsif TRIM(LINE(1..LAST))(1) = 'S'  or else
         TRIM(LINE(1..LAST))(1) = 's'     then
            D_K := SPECIAL;
         else
            PUT_LINE("No such dictionary");
            raise TEXT_IO.DATA_ERROR;
         end if; 
      end if;
   
   
      DICT_IO.OPEN(DICTFILE, IN_FILE, ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, 
                                                DICTIONARY_KIND'IMAGE(D_K))); 
   
     
      
      CREATE(DICTLINE, OUT_FILE, ADD_FILE_NAME_EXTENSION(DICT_LINE_NAME, 
                                                 "NEW"));
                                                 --DICTIONARY_KIND'IMAGE(D_K)));
      
   
      while not END_OF_FILE(DICTFILE)  loop
        READ(DICTFILE, DE);
        PUT(DICTLINE, DE);
      end loop;
    
   end FIL2DICT;
