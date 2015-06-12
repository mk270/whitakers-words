   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   with DICTIONARY_FORM;
   procedure UNIQPAGE is 
   
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
   
      
      UNIQUES_FILE, UNIQPAGE : TEXT_IO.FILE_TYPE;
   
      S, LINE, BLANK_LINE, BLANKS : STRING(1..400) := (others => ' ');
      L, LL, LAST : INTEGER := 0;

      STEM : STEM_TYPE := NULL_STEM_TYPE;
      QUAL : QUALITY_RECORD;
      KIND : KIND_ENTRY;
      TRAN : TRANSLATION_RECORD;
      MEAN : MEANING_TYPE;
      
              
    
      procedure GET_LINE_UNIQUE(INPUT : in TEXT_IO.FILE_TYPE; S : out STRING; LAST : out NATURAL) is
      begin
        LAST := 0;
        TEXT_IO.GET_LINE(INPUT, S, LAST);
        if TRIM(S(1..LAST)) /= ""  then   --  Rejecting blank lines
          null; 
        end if;   
      end GET_LINE_UNIQUE;

   begin
     PUT_LINE("UNIQUES.LAT -> UNIQPAGE.PG");  
     PUT_LINE("Takes UNIQUES form, single lines it, puts # at begining,");
     PUT_LINE("producing a .PG file for sorting to produce paper dictionary");
      CREATE(UNIQPAGE, OUT_FILE, "UNIQPAGE.PG");
      OPEN(UNIQUES_FILE, IN_FILE, "UNIQUES.LAT");
   
   
    OVER_LINES:       
    while not END_OF_FILE(UNIQUES_FILE)  loop
      LINE := BLANKS;
      GET_LINE_UNIQUE(UNIQUES_FILE, LINE, LAST);      --  STEM 
      STEM := HEAD(TRIM(LINE(1..LAST)), MAX_STEM_SIZE);

      LINE := BLANKS;
      GET_LINE_UNIQUE(UNIQUES_FILE, LINE, LAST);    --  QUAL, KIND, TRAN       
      QUALITY_RECORD_IO.GET(LINE(1..LAST), QUAL, L);
      GET(LINE(L+1..LAST), QUAL.POFS, KIND, L);
      AGE_TYPE_IO.GET(LINE(L+1..LAST), TRAN.AGE, L);
      AREA_TYPE_IO.GET(LINE(L+1..LAST), TRAN.AREA, L);
      GEO_TYPE_IO.GET(LINE(L+1..LAST), TRAN.GEO, L);
      FREQUENCY_TYPE_IO.GET(LINE(L+1..LAST), TRAN.FREQ, L);
      SOURCE_TYPE_IO.GET(LINE(L+1..LAST), TRAN.SOURCE, L);


      LINE := BLANKS;
      GET_LINE_UNIQUE(UNIQUES_FILE, LINE, L);         --  MEAN
      MEAN := HEAD(TRIM(LINE(1..L)), MAX_MEANING_SIZE);    
    
    
    
     
--      while not END_OF_FILE(UNIQUES_FILE) loop
--         S := BLANK_LINE;
--         GET_LINE(INPUT, S, LAST);
--         if TRIM(S(1..LAST)) /= ""  then   --  Rejecting blank lines
--         
--                  
         
            TEXT_IO.PUT(UNIQPAGE, "#" & STEM); 
            
            QUALITY_RECORD_IO.PUT(UNIQPAGE, QUAL);
            
            
           -- PART := (V, (QUAL.V.CON, KIND.V_KIND));
            
            if (QUAL.POFS = V)  and then  (KIND.V_KIND in GEN..PERFDEF)  then
              TEXT_IO.PUT(UNIQPAGE, "  " & VERB_KIND_TYPE'IMAGE(KIND.V_KIND) & "  ");
            end if;                                              
            
                        
                 TEXT_IO.PUT(UNIQPAGE, " [");
                 AGE_TYPE_IO.PUT(UNIQPAGE, TRAN.AGE);
                 AREA_TYPE_IO.PUT(UNIQPAGE, TRAN.AREA);
                 GEO_TYPE_IO.PUT(UNIQPAGE, TRAN.GEO);
                 FREQUENCY_TYPE_IO.PUT(UNIQPAGE, TRAN.FREQ);
                 SOURCE_TYPE_IO.PUT(UNIQPAGE, TRAN.SOURCE);
                 TEXT_IO.PUT(UNIQPAGE, "]");
             
        
            PUT(UNIQPAGE, " :: ");
            PUT_LINE(UNIQPAGE, MEAN); 
         
         --end if;  --  Rejecting blank lines
      end loop OVER_LINES;
   
      CLOSE(UNIQPAGE);
      exception
         when TEXT_IO.DATA_ERROR  =>
            null;
         when others =>
            PUT_LINE(S(1..LAST));
            CLOSE(UNIQPAGE);
   
   end UNIQPAGE;
