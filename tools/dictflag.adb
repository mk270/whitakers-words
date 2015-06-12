   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure DICTFLAG is 
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use TEXT_IO;
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
   
    
   
      BE_VE : VERB_ENTRY := (CON => (5, 1), KIND => TO_BE);
   
      D_K : DICTIONARY_KIND := XXX;       --  ######################
   
   
      START_STEM_1  : constant := 1;
      START_STEM_2  : constant := START_STEM_1 + MAX_STEM_SIZE + 1;
      START_STEM_3  : constant := START_STEM_2 + MAX_STEM_SIZE + 1;
      START_STEM_4  : constant := START_STEM_3 + MAX_STEM_SIZE + 1;
      START_PART    : constant := START_STEM_4 + MAX_STEM_SIZE + 1;
      START_TRAN    : constant INTEGER := 
         START_PART + 
         INTEGER(PART_ENTRY_IO.DEFAULT_WIDTH + 1);
      FINISH_LINE   : constant INTEGER := 
         START_TRAN +
         TRANSLATION_RECORD_IO.DEFAULT_WIDTH - 1;
   
   
     AGE_ARRAY : array (AGE_TYPE'RANGE) of INTEGER := (others => 0);
    
     AREA_ARRAY : array (AREA_TYPE'RANGE) of INTEGER := (others => 0);
    
     GEO_ARRAY : array (GEO_TYPE'RANGE) of INTEGER := (others => 0);
    
     FREQ_ARRAY : array (FREQUENCY_TYPE'RANGE) of INTEGER := (others => 0);
    
     SOURCE_ARRAY : array (SOURCE_TYPE'RANGE) of INTEGER := (others => 0);
    
         
         
         
          
   
   
      DICTFILE : DICT_IO.FILE_TYPE;
      INPUT, OUTPUT : TEXT_IO.FILE_TYPE;
      DE : DICTIONARY_ENTRY;
   
      S, LINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      L, LL, LAST : INTEGER := 0;
      J : DICT_IO.COUNT := 0;
      MEAN_TO_BE : constant MEANING_TYPE := 
         HEAD("to be, exist; also used to form verb perfect passive tenses" &
              " with NOM PERF PPL", MAX_MEANING_SIZE);
   
   begin
      PUT_LINE(
              "Takes a DICTLINE.D_K and produces a numeration of FLAGS");
      PUT("What dictionary to list, GENERAL or SPECIAL  =>");
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
   
   
      OPEN(INPUT, IN_FILE, ADD_FILE_NAME_EXTENSION(DICT_LINE_NAME, 
                                                DICTIONARY_KIND'IMAGE(D_K))); 
          
      CREATE(OUTPUT, OUT_FILE, ADD_FILE_NAME_EXTENSION("FLAGS", 
                                                    DICTIONARY_KIND'IMAGE(D_K)));
                                      
     
   
 
      while not END_OF_FILE(INPUT) loop
         S := BLANK_LINE;
         GET_LINE(INPUT, S, LAST);
         if TRIM(S(1..LAST)) /= ""  then
            L := 0;
         
            FORM_DE:
            begin
            
               DE.STEMS(1) := S(START_STEM_1..MAX_STEM_SIZE);
            --NEW_LINE; PUT(DE.STEMS(1));
               DE.STEMS(2) := S(START_STEM_2..START_STEM_2+MAX_STEM_SIZE-1);
               DE.STEMS(3) := S(START_STEM_3..START_STEM_3+MAX_STEM_SIZE-1);
               DE.STEMS(4) := S(START_STEM_4..START_STEM_4+MAX_STEM_SIZE-1);
            --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('@'); 
               GET(S(START_PART..LAST), DE.PART, L);
            --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('&'); PUT(S(L+1..LAST)); PUT('3'); 
            --   GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
               
               GET(S(L+1..LAST), DE.TRAN.AGE, L);   
                 AGE_ARRAY(DE.TRAN.AGE) := AGE_ARRAY(DE.TRAN.AGE) + 1;
               GET(S(L+1..LAST), DE.TRAN.AREA, L);
                  AREA_ARRAY(DE.TRAN.AREA) := AREA_ARRAY(DE.TRAN.AREA) + 1;
               GET(S(L+1..LAST), DE.TRAN.GEO, L);
                  GEO_ARRAY(DE.TRAN.GEO) := GEO_ARRAY(DE.TRAN.GEO) + 1;
               GET(S(L+1..LAST), DE.TRAN.FREQ, L);
                  FREQ_ARRAY(DE.TRAN.FREQ) := FREQ_ARRAY(DE.TRAN.FREQ) + 1;
               GET(S(L+1..LAST), DE.TRAN.SOURCE, L);
                  SOURCE_ARRAY(DE.TRAN.SOURCE) := SOURCE_ARRAY(DE.TRAN.SOURCE) + 1;
                  
               DE.MEAN := HEAD(S(L+2..LAST), MAX_MEANING_SIZE);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING, not ENUM
            
               exception
                  when others =>
                     NEW_LINE;
                     PUT_LINE("Exception");
                     PUT_LINE(S(1..LAST));
                     INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
                     PUT(DE); NEW_LINE;
            end FORM_DE;
         
            J := J + 1;
         
         
           
         end if;
      end loop;
      
      TEXT_IO.PUT(OUTPUT, "Number of lines in DICTLINE "  & 
          DICTIONARY_KIND'IMAGE(D_K) & "  ");
      INTEGER_IO.PUT(OUTPUT, INTEGER(J)); 
      TEXT_IO.NEW_LINE(OUTPUT);
            
      
      TEXT_IO.NEW_LINE(OUTPUT, 4);
      TEXT_IO.PUT_LINE(OUTPUT, "AGE");
      for I in AGE_TYPE'RANGE  loop
          TEXT_IO.PUT(OUTPUT, AGE_TYPE'IMAGE(I));
          TEXT_IO.SET_COL(OUTPUT, 10);
          TEXT_IO.PUT_LINE(OUTPUT, INTEGER'IMAGE(AGE_ARRAY(I)));
      end loop;
      
      
      TEXT_IO.NEW_LINE(OUTPUT, 4);
      TEXT_IO.PUT_LINE(OUTPUT, "AREA");
      for I in AREA_TYPE'RANGE  loop
          TEXT_IO.PUT(OUTPUT, AREA_TYPE'IMAGE(I));
          TEXT_IO.SET_COL(OUTPUT, 10);
          TEXT_IO.PUT_LINE(OUTPUT, INTEGER'IMAGE(AREA_ARRAY(I)));
      end loop;
      
      
      TEXT_IO.NEW_LINE(OUTPUT, 4);
      TEXT_IO.PUT_LINE(OUTPUT, "GEO");
      for I in GEO_TYPE'RANGE  loop
          TEXT_IO.PUT(OUTPUT, GEO_TYPE'IMAGE(I));
          TEXT_IO.SET_COL(OUTPUT, 10);
          TEXT_IO.PUT_LINE(OUTPUT, INTEGER'IMAGE(GEO_ARRAY(I)));
      end loop;
      
      
      TEXT_IO.NEW_LINE(OUTPUT, 4);
      TEXT_IO.PUT_LINE(OUTPUT, "FREQ");
      for I in FREQUENCY_TYPE'RANGE  loop
          TEXT_IO.PUT(OUTPUT, FREQUENCY_TYPE'IMAGE(I));
          TEXT_IO.SET_COL(OUTPUT, 10);
          TEXT_IO.PUT_LINE(OUTPUT, INTEGER'IMAGE(FREQ_ARRAY(I)));
      end loop;
      
      
      TEXT_IO.NEW_LINE(OUTPUT, 4);
      TEXT_IO.PUT_LINE(OUTPUT, "SOURCE");
      for I in SOURCE_TYPE'RANGE  loop
          TEXT_IO.PUT(OUTPUT, SOURCE_TYPE'IMAGE(I));
          TEXT_IO.SET_COL(OUTPUT, 10);
          TEXT_IO.PUT_LINE(OUTPUT, INTEGER'IMAGE(SOURCE_ARRAY(I)));
      end loop;
      
      
  
         CLOSE(OUTPUT);
     
   
      exception
         when TEXT_IO.DATA_ERROR  =>
            null;
         when others =>
            PUT_LINE(S(1..LAST));
            INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
            CLOSE(OUTPUT);
   
   end DICTFLAG;
