   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure LINEFILE is 
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use TEXT_IO;
      use DICTIONARY_ENTRY_IO;
      use DICT_IO;
   
      DICTFILE : DICT_IO.FILE_TYPE;
      OUTPUT : TEXT_IO.FILE_TYPE;
      DE : DICTIONARY_ENTRY;
      D_K : DICTIONARY_KIND := GENERAL;
      LINE : STRING(1..40) := (others => ' ');
      LAST : INTEGER := 0;
      
   begin
      PUT_LINE("Takes a DICTFILE.D_K and produces a DICTLINE.D_K");
      PUT("What dictionary to convert, GENERAL or SPECIAL  (Reply G or S) =>");
      GET_LINE(LINE, LAST);
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
   
   
      OPEN(DICTFILE, IN_FILE, ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, 
                                                DICTIONARY_KIND'IMAGE(D_K))); 
      
   
      CREATE(OUTPUT, OUT_FILE, ADD_FILE_NAME_EXTENSION("DICT_NEW", 
                                                 DICTIONARY_KIND'IMAGE(D_K)));
   

     while not END_OF_FILE(DICTFILE)  loop
       READ(DICTFILE, DE);
       PUT(OUTPUT, DE); 
       TEXT_IO.NEW_LINE(OUTPUT);
     end loop;
     
   
   end LINEFILE;
