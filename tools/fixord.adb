   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure FIXORD is 
      use TEXT_IO;
   
      INPUT, OUTPUT : TEXT_IO.FILE_TYPE;
   
      S, LINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      L, LL, LAST : INTEGER := 0;
   
   begin
      PUT_LINE("FIXORD.IN -> FIXORD.OUT");
      PUT_LINE("Makes a clean (no #) 3 line ED format from LISTORD output");
      
      CREATE(OUTPUT, OUT_FILE, "FIXORD.OUT");
      OPEN(INPUT, IN_FILE, "FIXORD.IN");
   
   
   OVER_LINES:
      while not END_OF_FILE(INPUT) loop
         S := BLANK_LINE;
         GET_LINE(INPUT, S, LAST);
         if TRIM(S(1..LAST)) /= ""  then   --  Rejecting blank lines
         
            if S(1) /= '#'  then
               PUT_LINE(OUTPUT, S(1..LAST)); 
            end if;
         
         end if;  --  Rejecting blank lines
      end loop OVER_LINES;
   
      CLOSE(OUTPUT);
      exception
         when TEXT_IO.DATA_ERROR  =>
            CLOSE(OUTPUT);
   
   end FIXORD;
