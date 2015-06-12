   with TEXT_IO; use TEXT_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   procedure INVERT is
      LINE, PARM : STRING(1..250);
      L, LAST : INTEGER;
      N1, N2 : INTEGER;
   
      INPUT, OUTPUT : FILE_TYPE;
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
   
      function INVERT(S : STRING) return STRING is
         T : STRING(1..S'LENGTH);
      begin
         for I in 1..T'LENGTH  loop
            T(I) := S(S'LAST-I+1);
         end loop;
         return HEAD(TRIM(T), S'LENGTH);
      
      end INVERT;
   
   begin
      PUT_LINE("Inverts/reverses the order of columns N1..N2 of INVERT.IN -> INVERT.OUT");
      PUT("Give an N1 and N2 => ");
      GET_LINE(PARM, LAST);
   
      INTEGER_IO.GET(PARM(1..LAST), N1, L);
      INTEGER_IO.GET(PARM(L+1..LAST), N2, L);
   
      CREATE(OUTPUT, OUT_FILE, "INVERT.OUT");
      OPEN(INPUT, IN_FILE, "INVERT.IN");
   
   
      while not END_OF_FILE(INPUT)  loop
         GET_LINE(INPUT, LINE, LAST);
      
         LINE(N1..N2)  := INVERT(LINE(N1..N2));
         PUT('.');
         PUT_LINE(OUTPUT, LINE(1..LAST));
      
      end loop;
   
      CLOSE(OUTPUT);
   
      exception
         when others  =>
            CLOSE(OUTPUT);
   
   end INVERT;
