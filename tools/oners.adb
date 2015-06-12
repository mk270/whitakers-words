   with TEXT_IO; use TEXT_IO;
   procedure ONERS is
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use INTEGER_IO;
   
      LINE, OLD_LINE : STRING(1..250) := (others => ' ');
      LAST, OLD_LAST : INTEGER := 0;
      N : INTEGER := 0;
   
      INPUT, OUTPUT : FILE_TYPE;
   
   begin
      PUT_LINE("ONERS.IN -> ONERS.OUT");
      PUT_LINE("Takes a sorted file to produce a file having just one of each identical line.");
      PUT_LINE("Puts a count of how many identical lines at the begining of each."); 
      
      OPEN(INPUT, IN_FILE, "ONERS.IN");
      CREATE(OUTPUT, OUT_FILE, "ONERS.OUT");
   
      GET_LINE(INPUT, OLD_LINE, OLD_LAST);
   
      while not END_OF_FILE(INPUT)  loop
         GET_LINE(INPUT, LINE, LAST);
         N := N + 1;
         if LINE(1..LAST) /= OLD_LINE(1..OLD_LAST)  then
            PUT(OUTPUT, N); 
            PUT_LINE(OUTPUT, "  " & OLD_LINE(1..OLD_LAST));
            N := 0;
            OLD_LAST := LAST;
            OLD_LINE(1..OLD_LAST) := LINE(1..LAST);
         end if;
      end loop;
   
      CLOSE(OUTPUT);
   end ONERS;
