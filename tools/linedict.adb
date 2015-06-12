   with TEXT_IO; use TEXT_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure LINEDICT is 
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use DICTIONARY_ENTRY_IO;
      use PART_ENTRY_IO;
      use KIND_ENTRY_IO;
      use AGE_TYPE_IO;
      use AREA_TYPE_IO;
      use GEO_TYPE_IO;
      use FREQUENCY_TYPE_IO;
      use SOURCE_TYPE_IO;
   
      DE : DICTIONARY_ENTRY;
   
      DICTIONARY_FILE : FILE_TYPE;
      OUTPUT : FILE_TYPE;
   
      ST : STEM_TYPE := NULL_STEM_TYPE;
      BLK_STEM : constant STEM_TYPE := NULL_STEM_TYPE;
      STS : STEMS_TYPE := NULL_STEMS_TYPE;
      MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
      PT  : PART_ENTRY  := NULL_PART_ENTRY;
   
      LINE, ST_LINE, PT_LINE, MN_LINE, BLANK_LINE : STRING(1..300) := 
         (others => ' ');
      L, LL, LAST, LEN : INTEGER := 0;
      NUMBER_OF_DICTIONARY_ENTRIES : INTEGER := 0;
   
      procedure GET_STEM(S : in STRING; 
                      STEM : out STEM_TYPE; LAST : out INTEGER) is
         I  : INTEGER := 1;
         L  : INTEGER := S'FIRST;
      begin
         STEM := NULL_STEM_TYPE;
      --  Squeeze left
         while L <= S'LAST and then S(L) = ' '  loop
            L := L + 1;
         end loop;
      --  Count until the first blank
      --  Return that string
         while L <= S'LAST and then S(L) /= ' '  loop
            STEM(I) := S(L);
            I := I + 1;
            L := L + 1;
         end loop;
      --  Return  last
         LAST := L;
      end GET_STEM;
   
   
   
   
   begin
      PUT_LINE("LINEDICT.IN (EDIT format - 3 lines) -> LINEDICT.OUT (DICTLINE format)");
   
      CREATE(OUTPUT, OUT_FILE, "LINEDICT.OUT");
   
      OPEN(DICTIONARY_FILE, IN_FILE, "LINEDICT.IN");
      PUT("Dictionary loading");
   
   
      while not END_OF_FILE(DICTIONARY_FILE)  loop
         ST_LINE := BLANK_LINE;
         PT_LINE := BLANK_LINE;
         MN_LINE := BLANK_LINE; 
         ERROR_CHECK:
         begin
         
            GET_NON_COMMENT_LINE(DICTIONARY_FILE, ST_LINE, LAST);      --  STEMS
         
            LINE := BLANK_LINE;
            GET_NON_COMMENT_LINE(DICTIONARY_FILE, PT_LINE, L);           --  PART
            GET(PT_LINE(1..L), DE.PART, LL);                
--            GET(PT_LINE(LL+1..L), DE.PART.POFS, DE.KIND, LL);
            
            GET(PT_LINE(LL+1..L), DE.TRAN.AGE, LL);                
            GET(PT_LINE(LL+1..L), DE.TRAN.AREA, LL);                
            GET(PT_LINE(LL+1..L), DE.TRAN.GEO, LL);                
            GET(PT_LINE(LL+1..L), DE.TRAN.FREQ, LL);                
            GET(PT_LINE(LL+1..L), DE.TRAN.SOURCE, LL);                
 
       
            DE.STEMS := NULL_STEMS_TYPE;
            LL := 1;
         --  Extract up to 4 stems                  
         
            for I in 1..NUMBER_OF_STEMS(DE.PART.POFS)  loop   --  EXTRACT STEMS
              GET_STEM(ST_LINE(LL..LAST), DE.STEMS(I), LL);
            end loop;
 
        
            LINE := BLANK_LINE;
            GET_NON_COMMENT_LINE(DICTIONARY_FILE, MN_LINE, L);         --  MEANING
 
            DE.MEAN := HEAD(TRIM(MN_LINE(1..L)), MAX_MEANING_SIZE);
         
            
            PUT(OUTPUT, DE); NEW_LINE(OUTPUT);
         
            NUMBER_OF_DICTIONARY_ENTRIES := NUMBER_OF_DICTIONARY_ENTRIES + 1;
         
            exception
               when others  =>
                  PUT_LINE("-------------------------------------------------------------");
                  PUT_LINE(HEAD(ST_LINE, 78));
                  PUT_LINE(HEAD(PT_LINE, 78));
                  PUT_LINE(HEAD(MN_LINE, 78));
         end ERROR_CHECK;
      
      
      end loop;
      CLOSE(DICTIONARY_FILE);
      CLOSE(OUTPUT);
      SET_COL(33); PUT("--  "); INTEGER_IO.PUT(NUMBER_OF_DICTIONARY_ENTRIES);
      PUT(" entries");    SET_COL(55); PUT_LINE("--  Loaded correctly");
   end LINEDICT;

