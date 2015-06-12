   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   with DICTIONARY_FORM;
   procedure DICTPAGE is 
   --  DICTPAGE.IN -> DICTPAGE.OUT  
   --  Takes DICTLINE form, puts # and dictionary form at begining,
   --  a file that can be sorted to produce word order of paper dictionary
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
   
   
      INPUT, OUTPUT : TEXT_IO.FILE_TYPE;
      DE : DICTIONARY_ENTRY;
   
      S, LINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      L, LL, LAST : INTEGER := 0;
      J : INTEGER := 1;

      function ADD(STEM, INFL : STRING) return STRING is
      begin
         return HEAD(TRIM(STEM) & TRIM(INFL), 20);
      end ADD;
   
   
   
   begin
     PUT_LINE("DICTPAGE.IN -> DICTPAGE.OUT");  
     PUT_LINE("Takes DICTLINE form, puts # and dictionary form at begining, a file");
     PUT_LINE("for sorting with ::, a DICTPAGE.RAW to process for paper-like dictionary");
     PUT_LINE("Process result with PAGE2HTM to create a pretty display");
     
     CREATE(OUTPUT, OUT_FILE, "DICTPAGE.OUT");
     OPEN(INPUT, IN_FILE, "DICTPAGE.IN");
     
   
   OVER_LINES:
      while not END_OF_FILE(INPUT) loop
         S := BLANK_LINE;
         GET_LINE(INPUT, S, LAST);
         if TRIM(S(1..LAST)) /= ""  then   --  Rejecting blank lines
         
            FORM_DE:
            begin
            
               DE.STEMS(1) := S(START_STEM_1..MAX_STEM_SIZE);
               DE.STEMS(2) := S(START_STEM_2..START_STEM_2+MAX_STEM_SIZE-1);
               DE.STEMS(3) := S(START_STEM_3..START_STEM_3+MAX_STEM_SIZE-1);
               DE.STEMS(4) := S(START_STEM_4..START_STEM_4+MAX_STEM_SIZE-1);
               GET(S(START_PART..LAST), DE.PART, L);
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
            end FORM_DE;
         
         
            PUT(OUTPUT, "#" & DICTIONARY_FORM(DE)); 
            
--            if DE.PART.POFS = N  then
--              TEXT_IO.PUT(OUTPUT, "  " & GENDER_TYPE'IMAGE(DE.PART.N.GENDER) & "  ");
--            end if;
--            if (DE.PART.POFS = V)  and then  (DE.PART.V.KIND in GEN..PERFDEF)  then
--              TEXT_IO.PUT(OUTPUT, "  " & VERB_KIND_TYPE'IMAGE(DE.PART.V.KIND) & "  ");
--            end if;
            
                 TEXT_IO.PUT(OUTPUT, " [");
                 AGE_TYPE_IO.PUT(OUTPUT, DE.TRAN.AGE);
                 AREA_TYPE_IO.PUT(OUTPUT, DE.TRAN.AREA);
                 GEO_TYPE_IO.PUT(OUTPUT, DE.TRAN.GEO);
                 FREQUENCY_TYPE_IO.PUT(OUTPUT, DE.TRAN.FREQ);
                 SOURCE_TYPE_IO.PUT(OUTPUT, DE.TRAN.SOURCE);
                 TEXT_IO.PUT(OUTPUT, "]");
             
        
            PUT(OUTPUT, " :: ");
            PUT_LINE(OUTPUT, DE.MEAN); 
         
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
   
   end DICTPAGE;
