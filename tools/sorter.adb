   with TEXT_IO;
   with DIRECT_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   procedure SORTER is   
   --  This program sorts a file of lines (strings) on 4 substrings Mx..Nx
   --  Sort by stringwise (different cases), numeric, or POS enumeration
   
      package BOOLEAN_IO is new TEXT_IO.ENUMERATION_IO(BOOLEAN);
      use BOOLEAN_IO;
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use INTEGER_IO;
      package FLOAT_IO is new TEXT_IO.FLOAT_IO(FLOAT);
      use FLOAT_IO;
      use TEXT_IO;
   
      NAME_LENGTH : constant := 80; 
      ST, ENTER_LINE : STRING(1..NAME_LENGTH) := (others => ' ');
      LS, LAST : INTEGER := 0;
      INPUT_NAME : STRING(1..80) := (others => ' ');
   
      LINE_LENGTH : constant := 300;        --  ##################################
                                        --  Max line length on input file
                                        --  Shorter => less disk space to sort
      CURRENT_LENGTH : INTEGER := 0;
      subtype TEXT_TYPE is STRING(1..LINE_LENGTH);
   --type LINE_TYPE is 
   -- record
   --   CURRENT_LENGTH : CURRENT_LINE_LENGTH_TYPE := 0;
   --   TEXT : TEXT_TYPE;
   -- end record;
      package LINE_IO is new DIRECT_IO(TEXT_TYPE);
      use LINE_IO;
      BLANK_TEXT : TEXT_TYPE := (others => ' ');
   
      LINE_TEXT : TEXT_TYPE := BLANK_TEXT;
      OLD_LINE : TEXT_TYPE := BLANK_TEXT;
      P_LINE : TEXT_TYPE := BLANK_TEXT;
   
      type SORT_TYPE is (A, C, G, U, N, F, P, S);
      package SORT_TYPE_IO is new TEXT_IO.ENUMERATION_IO(SORT_TYPE);
      use SORT_TYPE_IO;
   
      type WAY_TYPE is (I, D);
      package WAY_TYPE_IO is new TEXT_IO.ENUMERATION_IO(WAY_TYPE);
      use WAY_TYPE_IO;
   
   
      INPUT  : TEXT_IO.FILE_TYPE;
      OUTPUT : TEXT_IO.FILE_TYPE;
      WORK   : LINE_IO.FILE_TYPE;
   
      M1, M2, M3, M4 : NATURAL := 1;
      N1, N2, N3, N4 : NATURAL := LINE_LENGTH;
      Z1, Z2, Z3, Z4 : NATURAL := 0;           
   
      S1, S2, S3, S4 : SORT_TYPE := A;
      W1, W2, W3, W4 : WAY_TYPE := I;
   
      ENTRY_FINISHED : exception;
   
   
   --  For section numbering of large documents and standards
      type SECTION_TYPE is 
         record
            FIRST_LEVEL   : INTEGER := 0;
            SECOND_LEVEL  : INTEGER := 0;
            THIRD_LEVEL   : INTEGER := 0;
            FOURTH_LEVEL  : INTEGER := 0;
            FIFTH_LEVEL   : INTEGER := 0;
         end record;
   
      NO_SECTION : constant SECTION_TYPE := (0, 0, 0, 0, 0);
   
      type APPENDIX_TYPE is (NONE, A, B, C, D, E, F, G, H, I, J, K, L, M,
                             N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
      package APPENDIX_IO is new TEXT_IO.ENUMERATION_IO(APPENDIX_TYPE);
   
      type APPENDIX_SECTION_TYPE is    record
            APPENDIX : APPENDIX_TYPE := NONE;     
            SECTION  : SECTION_TYPE  := NO_SECTION;
         end record;
   
      NO_APPENDIX_SECTION : constant APPENDIX_SECTION_TYPE := 
         (NONE, (0, 0, 0, 0, 0));
   
   --  procedure PUT(OUTPUT : TEXT_IO.FILE_TYPE; S : SECTION_TYPE);
   --  procedure PUT(S : SECTION_TYPE);  
   --  procedure GET(FROM : in STRING; 
   --                   S : out SECTION_TYPE; LAST : out POSITIVE);  
   --  function "<"(A, B : SECTION_TYPE) return BOOLEAN;  
   -- 
   --  procedure PUT(OUTPUT : TEXT_IO.FILE_TYPE; S : APPENDIX_SECTION_TYPE);
   --  procedure PUT(S : APPENDIX_SECTION_TYPE);  
   --  procedure GET(FROM : in STRING; 
   --                   S : out APPENDIX_SECTION_TYPE; LAST : out POSITIVE);  
   --  function "<"(A, B : APPENDIX_SECTION_TYPE) return BOOLEAN;  
   -- 
   
   
      procedure PUT(OUTPUT : TEXT_IO.FILE_TYPE; S : SECTION_TYPE) is
         LEVEL : INTEGER := 0;
      
         procedure PUT_LEVEL(OUTPUT : TEXT_IO.FILE_TYPE; L : INTEGER) is
         begin
            if L > 9999  then
               PUT(OUTPUT, "****");
            elsif L > 999  then
               PUT(OUTPUT, L, 4);
            elsif L > 99  then
               PUT(OUTPUT, L, 3);
            elsif L > 9  then
               PUT(OUTPUT, L, 2);
            elsif L >= 0  then
               PUT(OUTPUT, L, 1);
            else  
               PUT(OUTPUT, "**");
            end if;
         end PUT_LEVEL;
      
      begin
         if S.FIFTH_LEVEL <= 0  then
            if S.FOURTH_LEVEL <= 0  then
               if S.THIRD_LEVEL <= 0  then
                  if S.SECOND_LEVEL <= 0  then
                     LEVEL := 1;
                  else
                     LEVEL := 2;
                  end if;
               else
                  LEVEL := 3;
               end if;
            else
               LEVEL := 4;
            end if;
         else
            LEVEL := 5;
         end if;
      
         if S.FIRST_LEVEL <= 9  then
            PUT(OUTPUT, ' ');
         end if;
         PUT_LEVEL(OUTPUT, S.FIRST_LEVEL);
         if LEVEL = 1  then
            PUT(OUTPUT, '.');
            PUT(OUTPUT, '0');           --  To match the ATLAS index convention
         end if;
         if LEVEL >= 2  then
            PUT(OUTPUT, '.');
            PUT_LEVEL(OUTPUT, S.SECOND_LEVEL);
         end if;
         if LEVEL >= 3  then
            PUT(OUTPUT, '.');
            PUT_LEVEL(OUTPUT, S.THIRD_LEVEL);
         end if;
         if LEVEL >= 4  then
            PUT(OUTPUT, '.');
            PUT_LEVEL(OUTPUT, S.FOURTH_LEVEL);
         end if;
         if LEVEL >= 5  then
            PUT(OUTPUT, '.');
            PUT_LEVEL(OUTPUT, S.FIFTH_LEVEL);
         end if;
      end PUT;
   
      procedure PUT(S : SECTION_TYPE) is
         LEVEL : INTEGER := 0;
      
         procedure PUT_LEVEL(L : INTEGER) is
         begin
            if L > 9999  then
               PUT("****");
            elsif L > 999  then
               PUT(L, 4);
            elsif L > 99  then
               PUT(L, 3);
            elsif L > 9  then
               PUT(L, 2);
            elsif L >= 0  then
               PUT(L, 1);
            else  
               PUT("**");
            end if;
         end PUT_LEVEL;
      
      begin
         if S.FIFTH_LEVEL = 0  then
            if S.FOURTH_LEVEL = 0  then
               if S.THIRD_LEVEL = 0  then
                  if S.SECOND_LEVEL = 0  then
                     LEVEL := 1;
                  else
                     LEVEL := 2;
                  end if;
               else
                  LEVEL := 3;
               end if;
            else
               LEVEL := 4;
            end if;
         else
            LEVEL := 5;
         end if;
      
         if S.FIRST_LEVEL <= 9  then
            PUT(' ');
         end if;
         PUT_LEVEL(S.FIRST_LEVEL);
         PUT('.');
         if LEVEL = 1  then
            PUT('0');           --  To match the ATLAS index convention
         end if;
         if LEVEL >= 2  then
            PUT_LEVEL(S.SECOND_LEVEL);
         end if;
         if LEVEL >= 3  then
            PUT('.');
            PUT_LEVEL(S.THIRD_LEVEL);
         end if;
         if LEVEL >= 4  then
            PUT('.');
            PUT_LEVEL(S.FOURTH_LEVEL);
         end if;
         if LEVEL >= 5  then
            PUT('.');
            PUT_LEVEL(S.FIFTH_LEVEL);
         end if;
      end PUT;
   
      procedure GET(FROM : in STRING; 
                    S : out SECTION_TYPE; LAST : out INTEGER) is
         L  : INTEGER := 0;
         FT : INTEGER := FROM'FIRST;
         LT : INTEGER := FROM'LAST;
      begin
         S := NO_SECTION;
         if TRIM(FROM)'LAST < FROM'FIRST   then   
            return;   --  Empty string, no data         --  Return default
         end if;
      
         GET(FROM, S.FIRST_LEVEL, L);
         if L+1 >= LT  then
            LAST := L;
            return;
         end if;
         GET(FROM(L+2..LT), S.SECOND_LEVEL, L);
         if L+1 >= LT  then
            LAST := L;
            return;
         end if;
         GET(FROM(L+2..LT), S.THIRD_LEVEL, L);
         if L+1 >= LT  then
            LAST := L;
            return;
         end if;
         GET(FROM(L+2..LT), S.FOURTH_LEVEL, L);
         if L+1 >= LT  then
            LAST := L;
            return;
         end if;
         GET(FROM(L+2..LT), S.FIFTH_LEVEL, L);
         LAST := L;
         return;
         exception
            when TEXT_IO.END_ERROR =>
               LAST := L;
               return;
            when TEXT_IO.DATA_ERROR =>
               LAST := L;
               return;
            when others =>
               PUT(" Unexpected exception in GET(FROM; SECTION_TYPE) with input =>");
               PUT(FROM);
               NEW_LINE;
               LAST := L;
               raise; 
      end GET;
   
      function "<"(A, B : SECTION_TYPE) return BOOLEAN is
      begin
      
         if A.FIRST_LEVEL > B.FIRST_LEVEL  then
            return FALSE;
         elsif A.FIRST_LEVEL < B.FIRST_LEVEL  then
            return TRUE; 
         else
            if A.SECOND_LEVEL > B.SECOND_LEVEL  then
               return FALSE;
            elsif A.SECOND_LEVEL < B.SECOND_LEVEL  then
               return TRUE;
            else
               if A.THIRD_LEVEL > B.THIRD_LEVEL  then
                  return FALSE;
               elsif A.THIRD_LEVEL < B.THIRD_LEVEL  then
                  return TRUE;
               else
                  if A.FOURTH_LEVEL > B.FOURTH_LEVEL  then
                     return FALSE;
                  elsif A.FOURTH_LEVEL < B.FOURTH_LEVEL  then
                     return TRUE;
                  else
                     if A.FIFTH_LEVEL > B.FIFTH_LEVEL  then
                        return FALSE;
                     elsif A.FIFTH_LEVEL < B.FIFTH_LEVEL  then
                        return TRUE;
                     else
                        return FALSE;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      
         return FALSE;
      
      end "<";
   
      procedure PUT(OUTPUT : TEXT_IO.FILE_TYPE; S : APPENDIX_SECTION_TYPE) is
         use APPENDIX_IO;
      begin
         PUT(OUTPUT, S.APPENDIX);
         PUT(OUTPUT, ' ');
         PUT(OUTPUT, S.SECTION);
      end PUT;
   
      procedure PUT(S : APPENDIX_SECTION_TYPE) is
         use APPENDIX_IO;
      begin
         PUT(S.APPENDIX);
         PUT(' ');
         PUT(S.SECTION);
      end PUT;
   
      procedure GET(FROM : in STRING; 
                    S : out APPENDIX_SECTION_TYPE; LAST : out INTEGER) is
         use APPENDIX_IO;
         L  : INTEGER := 0;
         FT : INTEGER := FROM'FIRST;
         LT : INTEGER := FROM'LAST;
      begin
      
         S := NO_APPENDIX_SECTION;
         if (FT = LT)  or else
            (TRIM(FROM)'LENGTH = 0)  then   --  Empty/blank string, no data
            PUT("@");
            return;                      --  Return default
         end if;
      
      --PUT_LINE("In GET =>" & FROM & '|');
      
         begin
            GET(FROM, S.APPENDIX, L);
         --PUT("A");
            if L+1 >= LT  then
               LAST := L;
               return;
            end if;
            exception
               when others  =>
                  S.APPENDIX := NONE;
                  L := FT - 2;
         end;
      
      --    PUT("B");
      --    GET(FROM(L+2..LT), S.SECTION.FIRST_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT("C");
      --    GET(FROM(L+2..LT), S.SECTION.SECOND_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT("D");
      --    GET(FROM(L+2..LT), S.SECTION.THIRD_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT("E");
      --    GET(FROM(L+2..LT), S.SECTION.FOURTH_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT("F");
      --    GET(FROM(L+2..LT), S.SECTION.FIFTH_LEVEL, L);
      --    LAST := L;
      --PUT("G");
      
      
         GET(FROM(L+2..LT), S.SECTION, L);
      --PUT("F");
         return;
         exception
            when TEXT_IO.END_ERROR =>
               LAST := L;
               return;
            when TEXT_IO.DATA_ERROR =>
               LAST := L;
               return;
            when others =>
               PUT
                     (" Unexpected exception in GET(FROM; APPENDIX_SECTION_TYPE) with input =>");
               PUT(FROM);
               NEW_LINE;
               LAST := L;
               return;
      end GET;
   
      function "<"(A, B : APPENDIX_SECTION_TYPE) return BOOLEAN is
      begin
      
         if A.APPENDIX > B.APPENDIX  then
            return FALSE;
         elsif A.APPENDIX < B.APPENDIX  then
            return TRUE; 
         else
            if A.SECTION.FIRST_LEVEL > B.SECTION.FIRST_LEVEL  then
               return FALSE;
            elsif A.SECTION.FIRST_LEVEL < B.SECTION.FIRST_LEVEL  then
               return TRUE; 
            else
               if A.SECTION.SECOND_LEVEL > B.SECTION.SECOND_LEVEL  then
                  return FALSE;
               elsif A.SECTION.SECOND_LEVEL < B.SECTION.SECOND_LEVEL  then
                  return TRUE;
               else
                  if A.SECTION.THIRD_LEVEL > B.SECTION.THIRD_LEVEL  then
                     return FALSE;
                  elsif A.SECTION.THIRD_LEVEL < B.SECTION.THIRD_LEVEL  then
                     return TRUE;
                  else
                     if A.SECTION.FOURTH_LEVEL > B.SECTION.FOURTH_LEVEL  then
                        return FALSE;
                     elsif A.SECTION.FOURTH_LEVEL < B.SECTION.FOURTH_LEVEL  then
                        return TRUE;
                     else
                        if A.SECTION.FIFTH_LEVEL > B.SECTION.FIFTH_LEVEL  then
                           return FALSE;
                        elsif A.SECTION.FIFTH_LEVEL < B.SECTION.FIFTH_LEVEL  then
                           return TRUE;
                        else
                           return FALSE;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;  
      end "<";
   
      procedure PROMPT_FOR_ENTRY(ENTRY_NUMBER : STRING) is
      begin
         PUT("Give starting column and size of ");
         PUT(ENTRY_NUMBER);
         PUT_LINE(" significant sort field ");
         PUT("  with optional sort type and way  => ");
      end PROMPT_FOR_ENTRY;
   
   
      procedure GET_ENTRY (MX, NX  : out NATURAL;
                           SX  : out SORT_TYPE;
                           WX  : out WAY_TYPE ) is
         M : NATURAL := 1;
         N : NATURAL := LINE_LENGTH;
         S : SORT_TYPE := A;
         W : WAY_TYPE := I;
         Z : NATURAL := 0;
      
         procedure ECHO_ENTRY is
         begin
            PUT("                    Sorting on LINE("); PUT(M,3); 
            PUT(".."); PUT(N, 3); PUT(")");
            PUT("  with S = "); PUT(S); PUT(" and W = "); PUT(W); 
            NEW_LINE(2);
         end ECHO_ENTRY;
      
      begin
      
         M := 0;
         N := LINE_LENGTH;
         S := A;
         W := I;
      
      
         GET_LINE(ENTER_LINE, LS);
         if LS = 0  then
            raise ENTRY_FINISHED;
         end if;
         INTEGER_IO.GET(ENTER_LINE(1..LS), M, LAST);
         begin
            INTEGER_IO.GET(ENTER_LINE(LAST+1..LS), Z, LAST);
            if  M = 0 or Z = 0  then
               PUT_LINE("Start or size of zero, you must be kidding, aborting");
               raise PROGRAM_ERROR;
            elsif M + Z > LINE_LENGTH  then
               PUT_LINE("Size too large, going to end of line");
               N := LINE_LENGTH;
            else
               N := M + Z - 1;
            end if;
            SORT_TYPE_IO.GET(ENTER_LINE(LAST+1..LS), S, LAST);
            WAY_TYPE_IO.GET(ENTER_LINE(LAST+1..LS), W, LAST);
            MX := M; NX := N;  SX := S; WX := W;
            ECHO_ENTRY;
            return;
            exception
               when PROGRAM_ERROR  =>
                  PUT_LINE("PROGRAM_ERROR raised in GET_ENTRY");
                  raise;
               when others =>
                  MX := M; NX := N; SX := S; WX := W;
                  ECHO_ENTRY;
                  return;
         end;
      end GET_ENTRY;
   
      function IGNORE_SEPARATORS(S : STRING) return STRING is
         T : STRING(S'FIRST..S'LAST) := LOWER_CASE(S);
      begin
         for I in S'FIRST+1..S'LAST-1  loop
            if (S(I-1) /= '-'  and then S(I-1) /= '_')  and then
               (S(I) = '-'  or else S(I) = '_')  and then
               (S(I+1) /= '-'  and then S(I+1) /= '_')  then
               T(I) := ' ';
            end if;
         end loop;
         return T;
      end IGNORE_SEPARATORS;
   
      function LTU(C, D : CHARACTER) return BOOLEAN is
      begin
         if (D = 'v')  then
            if (C < 'u')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif (D = 'j')  then
            if (C < 'i')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif (D = 'V')  then
            if (C < 'U')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif (D = 'J')  then
            if (C < 'I')  then
               return TRUE;
            else
               return FALSE;
            end if;
         else
            return C < D; 
         end if;
      end LTU; 
   
      function EQU(C, D : CHARACTER) return BOOLEAN is
      begin
         if (D = 'u') or (D = 'v')  then
            if (C = 'u') or (C = 'v')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif (D = 'i') or (D = 'j')  then
            if (C = 'i') or (C = 'j')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif (D = 'U') or (D = 'V')  then
            if (C = 'U') or (C = 'V')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif (D = 'I') or (D = 'J')  then
            if (C = 'I') or (C = 'J')  then
               return TRUE;
            else
               return FALSE;
            end if;
         else
            return C = D; 
         end if;
      end EQU; 
   
      function GTU(C, D : CHARACTER) return BOOLEAN is
      begin
         if D = 'u'  then
            if (C > 'v')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif D = 'i'  then
            if (C > 'j')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif D = 'U'  then
            if (C > 'V')  then
               return TRUE;
            else
               return FALSE;
            end if;
         elsif D = 'I'  then
            if (C > 'J')  then
               return TRUE;
            else
               return FALSE;
            end if;
         else
            return C > D; 
         end if;
      end GTU; 
   
   
      function LTU(S, T : STRING) return BOOLEAN is
      begin
         for I in 1..S'LENGTH  loop   --  Not TRIMed, so same length
            if EQU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               null;
            elsif GTU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               return FALSE;
            elsif LTU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               return TRUE;
            end if;
         end loop;
         return FALSE;
      end LTU;
   
      function GTU(S, T : STRING) return BOOLEAN is
      begin
         for I in 1..S'LENGTH  loop   
            if EQU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               null;
            elsif LTU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               return FALSE;
            elsif GTU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               return TRUE;
            end if;
         end loop;
         return FALSE;
      end GTU;
   
   
      function EQU(S, T : STRING) return BOOLEAN is
      begin
         if S'LENGTH /= T'LENGTH  then
            return FALSE;
         end if;
      
         for I in 1..S'LENGTH  loop     
            if not EQU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
               return FALSE;
            end if;
         end loop;
      
         return TRUE;
      end EQU;
   
   
   
   
      function SLT (X, Y : STRING;         --  Make LEFT and RIGHT
                    ST : SORT_TYPE := A; 
                    WT : WAY_TYPE := I) return BOOLEAN is
         AS : STRING(X'RANGE) := X;
         BS : STRING(Y'RANGE) := Y;
         MN, NN : INTEGER := 0;
         FN, GN : FLOAT := 0.0;
      --FS, GS : SECTION_TYPE := NO_SECTION;
         FS, GS : APPENDIX_SECTION_TYPE := NO_APPENDIX_SECTION;
         PX, PY : PART_ENTRY;       --  So I can X here
      begin
         if ST = A  then
            AS := LOWER_CASE(AS);
            BS := LOWER_CASE(BS);
            if WT = I  then 
               return AS < BS;
            else
               return AS > BS;
            end if;
         
         elsif ST = C  then
            if WT = I  then 
               return AS < BS;
            else
               return AS > BS;
            end if;
         
         elsif ST = G  then
            AS := IGNORE_SEPARATORS(AS);
            BS := IGNORE_SEPARATORS(BS);
            if WT = I  then 
               return AS < BS;
            else
               return AS > BS;
            end if;
         
         elsif ST = U  then
            AS := LOWER_CASE(AS);
            BS := LOWER_CASE(BS);
            if WT = I  then 
               return LTU(AS, BS);
            else
               return GTU(AS, BS);
            end if;
         
         elsif ST = N  then
            INTEGER_IO.GET(AS, MN, LAST);
            INTEGER_IO.GET(BS, NN, LAST);
            if WT = I  then 
               return MN < NN;
            else
               return MN > NN;
            end if;
         
         elsif ST = F  then
            FLOAT_IO.GET(AS, FN, LAST);
            FLOAT_IO.GET(BS, GN, LAST);
            if WT = I  then 
               return FN < GN;
            else
               return FN > GN;
            end if;
         
         elsif ST = P  then
            PART_ENTRY_IO.GET(AS, PX, LAST);
            PART_ENTRY_IO.GET(BS, PY, LAST);
            if WT = I  then 
               return PX < PY;
            else
               return (not (PX < PY)) and (not (PX = PY));
            end if;
         
         elsif ST = S  then
         --PUT_LINE("AS =>" & AS & '|');
            GET(AS, FS, LAST);
         --PUT_LINE("BS =>" & BS & '|');
            GET(BS, GS, LAST);
         --PUT_LINE("GOT AS & BS");
            if WT = I  then 
               return FS < GS;
            else
               return (not (FS < GS)) and (not (FS = GS));
            end if;
         
         else
            return FALSE;
         end if;
      
         exception  
            when others  =>
               TEXT_IO.PUT_LINE("exception in SLT    showing LEFT and RIGHT");
               TEXT_IO.PUT_LINE(X & "&");
               TEXT_IO.PUT_LINE(Y & "|");
               raise;
      
      end SLT;
   
      function SORT_EQUAL (X, Y : STRING; 
                           ST : SORT_TYPE := A; 
                           WT : WAY_TYPE := I) return BOOLEAN is
         AS : STRING(X'RANGE) := X;
         BS : STRING(Y'RANGE) := Y;
         MN, NN : INTEGER := 0;
         FN, GN : FLOAT := 0.0;
         FS, GS : APPENDIX_SECTION_TYPE := NO_APPENDIX_SECTION;
         PX, PY : PART_ENTRY;
      begin
         if ST = A  then
            AS := LOWER_CASE(AS);
            BS := LOWER_CASE(BS);
            return AS = BS;
         
         elsif ST = C  then
            return AS = BS;
         
         elsif ST = G  then
            AS := IGNORE_SEPARATORS(AS);
            BS := IGNORE_SEPARATORS(BS);
            return AS = BS;
         
         elsif ST = U  then
            AS := LOWER_CASE(AS);
            BS := LOWER_CASE(BS);
            return EQU(AS,  BS);
         
         elsif ST = N  then
            INTEGER_IO.GET(AS, MN, LAST);
            INTEGER_IO.GET(BS, NN, LAST);
            return MN = NN;
         
         elsif ST = F  then
            FLOAT_IO.GET(AS, FN, LAST);
            FLOAT_IO.GET(BS, GN, LAST);
            return FN = GN;
         
         elsif ST = P  then
            PART_ENTRY_IO.GET(AS, PX, LAST);
            PART_ENTRY_IO.GET(BS, PY, LAST);
            return PX = PY;
         
         elsif ST = S  then
            GET(AS, FS, LAST);
            GET(BS, GS, LAST);
            return FS = GS;
         
         
         else
            return FALSE;
         end if;
      
         exception
            when others  =>
               TEXT_IO.PUT_LINE("exception in LT    showing LEFT and RIGHT");
               TEXT_IO.PUT_LINE(X & "|");
               TEXT_IO.PUT_LINE(Y & "|");
               raise;
      
      end SORT_EQUAL;
   
   
      function LT  (LEFT, RIGHT : TEXT_TYPE) return BOOLEAN is
      begin
      
         if SLT(LEFT(M1..N1),  RIGHT(M1..N1), S1, W1)  then
            return TRUE;
         elsif SORT_EQUAL(LEFT(M1..N1),  RIGHT(M1..N1), S1, W1) then
            if ((N2 > 0) and then 
                SLT(LEFT(M2..N2),  RIGHT(M2..N2), S2, W2) ) then
               return TRUE;
            elsif ((N2 > 0) and then 
                   SORT_EQUAL(LEFT(M2..N2),  RIGHT(M2..N2), S2, W2)) then
               if ((N3 > 0) and then 
                   SLT(LEFT(M3..N3),  RIGHT(M3..N3), S3, W3 ))  then
                  return TRUE;
               elsif ((N3 > 0) and then 
                      SORT_EQUAL(LEFT(M3..N3),  RIGHT(M3..N3), S3, W3))  then
                  if ((N4 > 0) and then 
                      SLT(LEFT(M4..N4),  RIGHT(M4..N4), S4, W4) )  then
                     return TRUE;
                  end if;
               end if;
            end if;
         end if;
         return FALSE;
         exception 
            when others =>
               TEXT_IO.PUT_LINE("exception in LT    showing LEFT and RIGHT");
               TEXT_IO.PUT_LINE(LEFT & "|");
               TEXT_IO.PUT_LINE(RIGHT & "|");
               raise;
      end LT; 
   
      procedure OPEN_FILE_FOR_INPUT(INPUT : in out TEXT_IO.FILE_TYPE;
                                    PROMPT : STRING := "File for input => ") is
         LAST : NATURAL := 0;
      begin
      GET_INPUT_FILE:
         loop
            CHECK_INPUT:
            begin
               NEW_LINE;
            
               PUT(PROMPT);
               GET_LINE(INPUT_NAME, LAST);
               OPEN(INPUT, IN_FILE, INPUT_NAME(1..LAST));
               exit;
               exception
                  when others  =>
                     PUT_LINE("   !!!!!!!!!  Try Again  !!!!!!!!");
            end CHECK_INPUT;
         end loop GET_INPUT_FILE;
      
      end OPEN_FILE_FOR_INPUT;
   
   
      procedure CREATE_FILE_FOR_OUTPUT(OUTPUT : in out TEXT_IO.FILE_TYPE;
                                       PROMPT : STRING := "File for output => ") is
         NAME : STRING(1..80) := (others => ' ');
         LAST : NATURAL := 0;
      begin
      
      GET_OUTPUT_FILE:
         loop
            CHECK_OUTPUT:
            begin
               NEW_LINE;
            
               PUT(PROMPT);
               GET_LINE(NAME, LAST);
               if TRIM(NAME(1..LAST))'LENGTH /= 0  then
                  CREATE(OUTPUT, OUT_FILE, NAME(1..LAST));
               else
                  CREATE(OUTPUT, OUT_FILE, TRIM(INPUT_NAME));
               end if;                                    
               exit;
               exception
                  when others  =>
                     PUT_LINE("   !!!!!!!!!  Try Again  !!!!!!!!");
            end CHECK_OUTPUT;
         end loop GET_OUTPUT_FILE;
      
      end CREATE_FILE_FOR_OUTPUT;
   
      function GRAPHIC(S : STRING) return STRING is
         T : STRING(1..S'LENGTH) := S;
      begin
         for I in S'RANGE  loop
            if CHARACTER'POS(S(I)) < 32  then
               T(I) := ' ';
            end if;
         end loop;
         return T;
      end GRAPHIC;
   
   begin
   
      NEW_LINE;
      PUT_LINE("Sorts a text file of lines four times on substrings M..N");
      PUT_LINE(
              "A)lphabetic (all case) C)ase sensitive, iG)nore seperators, U)i_is_vj,");
      PUT_LINE("    iN)teger, F)loating point, S)ection, or P)art entry");
      PUT_LINE("         I)ncreasing or D)ecreasing");
      NEW_LINE;
   
      OPEN_FILE_FOR_INPUT(INPUT, "What file to sort from => ");
      NEW_LINE;
   
      PROMPT_FOR_ENTRY("first");
      begin
         GET_ENTRY(M1, N1, S1, W1);
         exception
            when PROGRAM_ERROR  =>
               raise;
            when others => 
               null;
      end;
   
   
      begin
         PROMPT_FOR_ENTRY("second");
         GET_ENTRY(M2, N2, S2, W2);
         PROMPT_FOR_ENTRY("third");
         GET_ENTRY(M3, N3, S3, W3);
         PROMPT_FOR_ENTRY("fourth");
         GET_ENTRY(M4, N4, S4, W4);
         exception
            when PROGRAM_ERROR  =>
               raise;
            when ENTRY_FINISHED =>
               null;  
            when TEXT_IO.DATA_ERROR  | TEXT_IO.END_ERROR  => 
               null;
      end;
   
    --PUT_LINE("CREATING WORK FILE");
      NEW_LINE;
      CREATE (WORK, INOUT_FILE, "WORK."); 
      PUT_LINE("CREATED  WORK FILE");
   
      while not END_OF_FILE(INPUT)  loop
      --begin
         GET_LINE(INPUT, LINE_TEXT, CURRENT_LENGTH);
      --exception when others  =>
      --TEXT_IO.PUT_LINE("INPUT GET exception");
      --TEXT_IO.PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH) & "|");
      --end;
      --PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH));
      --PUT_LINE("=>" & HEAD(LINE_TEXT(1..CURRENT_LENGTH), LINE_LENGTH) & "|");
         if TRIM(LINE_TEXT(1..CURRENT_LENGTH)) /= ""  then
         --begin
            WRITE(WORK, HEAD(LINE_TEXT(1..CURRENT_LENGTH), LINE_LENGTH)  );
         --exception when others  =>
         --TEXT_IO.PUT_LINE("WORK WRITE exception");
         --TEXT_IO.PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH) & "|");
         --end;
         end if;
      end loop;
      CLOSE(INPUT);
   
      PUT_LINE("Begin sorting");
   
   
      LINE_HEAPSORT:  
      declare
      
         L    : LINE_IO.POSITIVE_COUNT := SIZE(WORK) / 2 + 1; 
         IR   : LINE_IO.POSITIVE_COUNT := SIZE(WORK);
         I, J : LINE_IO.POSITIVE_COUNT;
      
      begin
         TEXT_IO.PUT_LINE("SIZE OF WORK = " & INTEGER'IMAGE(INTEGER(SIZE(WORK))));
      MAIN:
         loop
         
            if L > 1  then
               L := L - 1;
               READ(WORK, LINE_TEXT, L);
               OLD_LINE := LINE_TEXT;
            else
               READ(WORK, LINE_TEXT, IR);
               OLD_LINE := LINE_TEXT;
               READ(WORK, LINE_TEXT, 1);
               WRITE(WORK, LINE_TEXT, IR);
               IR := IR - 1;
               if IR = 1 THEN
                  WRITE(WORK, OLD_LINE, 1);
                  exit MAIN;  
               end if;
            end if;
            I := L;
            J := L + L;
         
            while J <= IR   loop
               if J < IR  then
                  READ(WORK, LINE_TEXT, J);
                  READ(WORK, P_LINE, J+1);
               --if LT (LINE.TEXT, P_LINE.TEXT)  then
                  if LT (LINE_TEXT, P_LINE)  then
                     J := J + 1;
                  end if;
               end if;
               READ(WORK, LINE_TEXT, J);
            --if OLD_LINE.TEXT < LINE.TEXT  then
               if LT (OLD_LINE , LINE_TEXT)  then
                  WRITE(WORK, LINE_TEXT, I);
                  I := J;
                  J := J + J;
               else
                  J := IR + 1;
               end if;
            end loop;
            WRITE(WORK, OLD_LINE, I);
         
         end loop MAIN;
      
         exception
            when CONSTRAINT_ERROR => PUT_LINE("HEAP CONSTRAINT_ERROR"); 
            when others           => PUT_LINE("HEAP other_ERROR"); 
      end LINE_HEAPSORT;
   
      PUT_LINE("Finished sorting in WORK");
   
      CREATE_FILE_FOR_OUTPUT(OUTPUT, "Where to put the output => ");
   
   
   --RESET(WORK);
      Set_Index(WORK, 1);
      while not END_OF_FILE(WORK)  loop
         READ(WORK, LINE_TEXT);
         if TRIM(GRAPHIC(LINE_TEXT))'LENGTH > 0  then
         --PUT_LINE(TRIM(LINE_TEXT, RIGHT));
            PUT_LINE(OUTPUT, TRIM(LINE_TEXT, RIGHT));
         end if;
      end loop;
   
      CLOSE(WORK);
      CLOSE(OUTPUT);
      PUT_LINE("Done!");
      NEW_LINE;
   
      exception
         when PROGRAM_ERROR  =>
            PUT_LINE("SORT terminated on a PROGRAM_ERROR");
            CLOSE(OUTPUT);
         when TEXT_IO.DATA_ERROR =>     --Terminate on primary start or size = 0
            PUT_LINE("SORT terminated on a DATA_ERROR");
            PUT_LINE(LINE_TEXT);
            CLOSE(OUTPUT);
         when CONSTRAINT_ERROR =>       --Terminate on blank line for file name
            PUT_LINE("SORT terminated on a CONSTRAINT_ERROR");
            CLOSE(OUTPUT);
         when TEXT_IO.DEVICE_ERROR  =>     --Ran out of space to write output file
            PUT_LINE("SORT terminated on a DEVICE_ERROR");
            DELETE(OUTPUT);
            CREATE_FILE_FOR_OUTPUT(OUTPUT, "Wherelse to put the output => ");
            RESET(WORK);
            while not END_OF_FILE(WORK)  loop
               READ(WORK, LINE_TEXT);
               PUT_LINE(OUTPUT, LINE_TEXT);    --(1..LINE.CURRENT_LENGTH));
            end loop;
            CLOSE(OUTPUT);
   end SORTER;
