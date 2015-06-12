   with TEXT_IO; use TEXT_IO;
   package body STRINGS_PACKAGE is
   
      function MAX(A, B : INTEGER) return INTEGER is
      begin
         if A >= B  then 
            return A; end if; 
         return B;
      end MAX;
   
      function MIN(A, B : INTEGER) return INTEGER is
      begin
         if A <= B  then 
            return A; end if; 
         return B;
      end MIN;
   
   
      function LOWER_CASE(C : CHARACTER) return CHARACTER is
      begin
         if C in 'A'..'Z'  then
            return CHARACTER'VAL(CHARACTER'POS(C) + 32);
         else
            return C;
         end if;
      end LOWER_CASE;
   
      function LOWER_CASE(S : STRING) return STRING is
         T : STRING(S'RANGE);
      begin
         for I in S'RANGE  loop
            T(I) := LOWER_CASE(S(I));
         end loop;
         return T;
      end LOWER_CASE;
   
   
      function UPPER_CASE(C : CHARACTER) return CHARACTER is
      begin
         if C in 'a'..'z'  then
            return CHARACTER'VAL(CHARACTER'POS(C) - 32);
         else
            return C;
         end if;
      end UPPER_CASE;
   
      function UPPER_CASE(S : STRING) return STRING is
         T : STRING(S'RANGE);
      begin
         for I in S'RANGE  loop
            T(I) := UPPER_CASE(S(I));
         end loop;
         return T;
      end UPPER_CASE;
   
   
      function TRIM(SOURCE : in STRING;
                    SIDE   : in TRIM_END := BOTH) return STRING is
      --  Removes leading and trailing blanks and returns a STRING staring at 1
      --  For a string of all blanks as input it returns NULL_STRING
         T : STRING(1..SOURCE'LENGTH) := SOURCE;
         FIRST: NATURAL := SOURCE'FIRST; 
         LAST : NATURAL := SOURCE'LAST;

      begin
         if SIDE /= RIGHT  then
            FIRST := SOURCE'LAST + 1;
            for I in SOURCE'RANGE  loop
               if SOURCE(I) /= ' '  then
                  FIRST := I;
                  exit;
               end if;
            end loop;
         else
            FIRST := SOURCE'FIRST;
         end if;
      
         if SIDE /= LEFT  then
            LAST := SOURCE'FIRST - 1;
            for I in reverse SOURCE'RANGE  loop
               if SOURCE(I) /= ' '  then
                  LAST := I;
                  exit;
               end if;
            end loop;
         else
            LAST := SOURCE'LAST;
         end if;
      
         if FIRST > LAST  then
            return NULL_STRING;
         else
            T(1..LAST-FIRST+1) := SOURCE(FIRST..LAST);
            return T(1..LAST-FIRST+1);
         end if;
      end TRIM;        
   
   
      function HEAD(SOURCE : in STRING; 
                    COUNT  : in NATURAL; 
                    PAD    : in CHARACTER := ' ') return STRING is
      --  Truncates or fills a string to exactly N in length
         T : STRING(1..COUNT) := (others => ' ');
      begin
         if COUNT < SOURCE'LENGTH  then
            T(1..COUNT) := SOURCE(SOURCE'FIRST..SOURCE'FIRST+COUNT-1);
         else
            T(1..SOURCE'LENGTH) := SOURCE(SOURCE'FIRST..SOURCE'LAST);
         end if;
         return T;
      end HEAD; 
   
   
      procedure GET_NON_COMMENT_LINE(F : in TEXT_IO.FILE_TYPE; 
                                     S : out STRING; LAST : out INTEGER) is
      --  Reads a text file and outs a string that is as much of the 
      --  first line encountered that is not a comment, that is not a comment   
      
         T : STRING(1..250) := (others => ' ');
         L, LX : INTEGER := 0;
      begin
         LAST := 0;
      FILE_LOOP:
         while not TEXT_IO.END_OF_FILE(F)  loop  --  Loop until data - Finish on EOF
            TEXT_IO.GET_LINE(F, T, L);
            if (HEAD(TRIM(T), 250)(1..2) = "  "  or 
                HEAD(TRIM(T), 250)(1..2) = "--")  then
               null;
            else
               LX := L;
            LINE_LOOP:
               for I in 2..L  loop
               --  Any leading comment does not get to here
                  if (T(I-1) = '-')  and  (T(I) = '-')  then   --  We have a comment
                     LX := I - 2;
                     exit FILE_LOOP;
                  end if;
               end loop LINE_LOOP;
               exit FILE_LOOP;
            end if;
         end loop FILE_LOOP;
         S(1..LX) := T(1..LX);
         LAST := LX;
      end GET_NON_COMMENT_LINE;
   
   
   end STRINGS_PACKAGE;  
