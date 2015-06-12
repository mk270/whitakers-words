   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   procedure DUPS is 
      package INTEGER_TEXT_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use INTEGER_TEXT_IO;
      use TEXT_IO;
   
   
   
      INPUT, OUTPUT : FILE_TYPE;
      S, LINE, OLDLINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      J, L, LL, LAST : INTEGER := 0;
      MX, NX : NATURAL := 0;
   
      LINE_NUMBER : INTEGER := 0;
      NUMBER : INTEGER := 0;
   
   
      procedure GET_ENTRY (MX, NX  : out NATURAL) is
         Z : NATURAL := 0;
         LS : INTEGER := 0;
         ENTER_LINE : STRING(1..20);
      
         procedure PROMPT_FOR_ENTRY(ENTRY_NUMBER : STRING) is
         begin
            PUT("Give starting and ending column ");
         end PROMPT_FOR_ENTRY;
      
      
      begin
      
         GET_LINE(ENTER_LINE, LS);
         GET(ENTER_LINE(1..LS), MX, LAST);
         GET(ENTER_LINE(LAST+1..LS), NX, LAST);
      
      end GET_ENTRY;
   
   begin
      PUT_LINE("DUPS.IN -> DUPS.OUT    For sorted files");
      PUT_LINE("DUPS  checks for columns MX..NX being duplicates");
      GET_ENTRY(MX, NX);
   
      CREATE(OUTPUT, OUT_FILE, "DUPS.OUT");
      OPEN(INPUT, IN_FILE, "DUPS.IN");
   
      while not END_OF_FILE(INPUT) loop
         OLDLINE := LINE;
         LINE := BLANK_LINE;
         GET_LINE(INPUT, LINE, LAST);
         LINE_NUMBER := LINE_NUMBER + 1;
         if LINE(MX..NX) = OLDLINE(MX..NX)  and then 
            (LINE(111) /= '|') then
            NUMBER := NUMBER + 1 ;
            PUT(OUTPUT, LINE_NUMBER); PUT(OUTPUT, "  ");
            PUT_LINE(OUTPUT, LINE(1..NX));
         end if;
      end loop;
   
      CLOSE(OUTPUT);
   
      NEW_LINE;
      PUT("Number of entries = "); PUT(LINE_NUMBER); NEW_LINE;
      PUT("Number of DUPS    = "); PUT(NUMBER); NEW_LINE;
      PUT("Ratio             = 1 :"); PUT(LINE_NUMBER/NUMBER); NEW_LINE;
   
      exception
         when NAME_ERROR  => 
            PUT_LINE("No file to process");
            CLOSE(OUTPUT);
         
         when others =>
            PUT("Exception on LINE"); PUT(LINE_NUMBER); NEW_LINE;
            PUT_LINE(S(1..LAST));
            CLOSE(OUTPUT);
   
   end DUPS;
