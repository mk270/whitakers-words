   with TEXT_IO; use TEXT_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   procedure INVSTEMS is
      LINE : STRING(1..250);
      LL : INTEGER;
      subtype STEM is STRING (1..18);
      BLANK_STEM : constant STEM := (others => ' ');
      STS : array (1..4) of STEM;
   
      INPUT, OUTPUT : FILE_TYPE;
   
      function INVERT(S : STRING) return STRING is
         T : STRING(S'FIRST..S'LAST);
      begin
         if S(1) = ' '  then 
            return BLANK_STEM;  
         else
            for I in S'RANGE  loop
               T(I) := S(S'LAST-I+1);
            end loop;
            return HEAD(TRIM(T), 18);
         end if;
      end INVERT;
   
   begin
      PUT_LINE("Inverts the 4 stems of a DICTLINE form file INVERT_S.IN -> INVERT_S.OUT");
      
      CREATE(OUTPUT, OUT_FILE, "INVERT_S.OUT");
      OPEN(INPUT, IN_FILE, "INVERT_S.IN");
   
      while not END_OF_FILE(INPUT)  loop
         GET_LINE(INPUT, LINE, LL);
         STS(1) := LINE(1..18);
         STS(2) := LINE(20..37);
         STS(3) := LINE(39..56);
         STS(4) := LINE(58..75);
         for I in 1..4  loop
            STS(I) := INVERT(STS(I));
         end loop;
         LINE(1..18)  := STS(1) ;
         LINE(20..37) := STS(2);
         LINE(39..56) := STS(3);
         LINE(58..75) := STS(4);
         PUT_LINE(OUTPUT, LINE(1..LL));
      
      end loop;
   
      CLOSE(OUTPUT);
   
   end INVSTEMS;
