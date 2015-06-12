   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure LISTORD is 
   --   LISTORD    Takes # (DICTORD) long format to ED file
   --   (3 lines per entry so it is all on one screen)
   --   LISTORD.IN -> LISTORD.OUT
    package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use TEXT_IO;
      use DICTIONARY_ENTRY_IO;
      use PART_ENTRY_IO;
      use TRANSLATION_RECORD_IO;
      use KIND_ENTRY_IO;
      use AGE_TYPE_IO;
      use AREA_TYPE_IO;
      use GEO_TYPE_IO;
      use FREQUENCY_TYPE_IO;
      use SOURCE_TYPE_IO;
   
   
      START_STEM_1  : constant := 81;
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
   
   
      INPUT, OUTPUT : TEXT_IO.FILE_TYPE;
      DE : DICTIONARY_ENTRY;
   
      S, LINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      L, LL, LAST : INTEGER := 0;
      J : NATURAL := 0;

   
      function ADD(STEM, INFL : STRING) return STRING is
      begin
         return HEAD(TRIM(STEM) & TRIM(INFL), 20);
      end ADD;
   
   
   
   begin
      PUT_LINE("LISTORD    Takes # (DICTORD) long format to ED file");
      PUT_LINE("(3 lines per entry so it is all on one screen)");
      PUT_LINE("LISTORD.IN -> LISTORD.OUT");

      CREATE(OUTPUT, OUT_FILE, "LISTORD.OUT");
      OPEN(INPUT, IN_FILE, "LISTORD.IN");
   
   
   OVER_LINES:
      while not END_OF_FILE(INPUT) loop
         J := J + 1;
         S := BLANK_LINE;
         GET_LINE(INPUT, S, LAST);
         if TRIM(S(1..LAST)) /= ""  then   --  Rejecting blank lines
         
            FORM_DE:
            begin
            
               DE.STEMS(1) := S(START_STEM_1..START_STEM_1+MAX_STEM_SIZE-1);
               DE.STEMS(2) := S(START_STEM_2..START_STEM_2+MAX_STEM_SIZE-1);
               DE.STEMS(3) := S(START_STEM_3..START_STEM_3+MAX_STEM_SIZE-1);
               DE.STEMS(4) := S(START_STEM_4..START_STEM_4+MAX_STEM_SIZE-1);
               GET(S(START_PART..LAST), DE.PART, L);
               --GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
               GET(S(L+1..LAST), DE.TRAN.AGE, L);
               GET(S(L+1..LAST), DE.TRAN.AREA, L);
               GET(S(L+1..LAST), DE.TRAN.GEO, L);
               GET(S(L+1..LAST), DE.TRAN.FREQ, L);
               GET(S(L+1..LAST), DE.TRAN.SOURCE, L);
               DE.MEAN := HEAD(S(L+2..LAST), MAX_MEANING_SIZE);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING, not ENUM
            
               exception
                  when others =>
                     PUT_LINE("Exception");
                     PUT_LINE(S(1..LAST));
                     INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
                     PUT(DE); NEW_LINE;
                     raise;
            end FORM_DE;
         
         
         
         
            PUT_LINE(OUTPUT, S(1..78)); 
            PUT_LINE(OUTPUT, S(START_STEM_1..START_PART-1)); 
            PUT(OUTPUT, S(START_PART..START_TRAN-1)); PUT(OUTPUT, "      ");
            PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, " ");
            PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, " ");
            PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, " ");
            PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, " ");
            PUT(OUTPUT, DE.TRAN.SOURCE); NEW_LINE(OUTPUT);
            PUT_LINE(OUTPUT, TRIM(DE.MEAN)); 
         
         end if;  --  Rejecting blank lines
      end loop OVER_LINES;
   
      CLOSE(OUTPUT);
      exception
         when TEXT_IO.DATA_ERROR  =>
            null;
         when others =>
            PUT_LINE(S(1..LAST));
            INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
            CLOSE(OUTPUT);
   
   end LISTORD;
