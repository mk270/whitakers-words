   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure CHECK is 
      use INFLECTIONS_PACKAGE.INTEGER_IO;
      use TEXT_IO;
      use DICTIONARY_ENTRY_IO;
      use PART_ENTRY_IO;
      use KIND_ENTRY_IO;
      
   
   
      INPUT, OUTPUT : FILE_TYPE;
      DE : DICTIONARY_ENTRY;
   
      S, LINE, OLDLINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      J, L, LL, LAST : INTEGER := 0;
   
      LINE_NUMBER : INTEGER := 0;
      NUMBER : INTEGER := 0;
   
   
      function IS_VOWEL(C : CHARACTER) return Boolean is
      begin
         if (LOWER_CASE(C) = 'a') or
            (LOWER_CASE(C) = 'e') or
            (LOWER_CASE(C) = 'i') or
            (LOWER_CASE(C) = 'o') or
            (LOWER_CASE(C) = 'u')     then
            return TRUE;
         else
            return FALSE;
         end if;
      end IS_VOWEL;
   
      function HAS_PUNCTUATION(S : STRING) return Boolean is
         J : INTEGER := 0;
      begin
         for I in reverse S'range  loop
            J := I;
            exit when S(I) /= ' ';
         end loop;
         for I in S'FIRST..J-1  loop
            if LOWER_CASE(S(I)) not in 'a'..'z'  then
               return TRUE;
            end if;
         end loop;
         return FALSE;
      end HAS_PUNCTUATION;
   
   
      function MT(S : STEM_TYPE) return BOOLEAN is
      begin
         if S = NULL_STEM_TYPE  then
            return TRUE;
         elsif S(1..3) = "zzz"  then
            return FALSE;
         else 
            return FALSE;
         end if;
      end MT;
   
      function BK(S : STEM_TYPE) return BOOLEAN is
      begin
         if S = NULL_STEM_TYPE  then
            return TRUE;
         else 
            return FALSE;
         end if;
      end BK;
   
   
      function LEN(ST : STEM_TYPE) return INTEGER is
         L : INTEGER := 0;
      begin
         for I in ST'range  loop
            exit when ST(I) = ' ';
            L := L + 1;
         end loop;
         return L;
      end LEN;
   
      procedure VERIFY_STEMS is
         STS : STEMS_TYPE := DE.STEMS;
         PT  : PART_ENTRY := DE.PART;
         MEAN : MEANING_TYPE := DE.MEAN;
      
      
         procedure PROB(MESSAGE : STRING) is
         begin
            NUMBER := NUMBER + 1;
            PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
            PUT(OUTPUT, "  " & MESSAGE & "  ");
            PART_ENTRY_IO.PUT(OUTPUT, PT);
            NEW_LINE(OUTPUT);
            PUT_LINE(OUTPUT, S(1..LAST));
         end PROB;
      
      
      begin
      
      
      --  Process parts
         if PT.POFS = N  then
         
         --  Check that there are two and only two stems for a noun
            if (    BK(STS(1)) or
                    BK(STS(2)) or
                    not BK(STS(3)) or
                    not BK(STS(4)) )  and then
            not (PT.N.DECL = (9, 9) or PT.N.DECL = (9, 8))   then  --  Undeclined
               PROB("    EXPECTED  exactly 2  NOUN STEMS");
            end if;
         
         
         --  Check that the stems are the same when expected
            if PT.N.DECL = (1, 1) or
               PT.N.DECL = (2, 1) or
               PT.N.DECL = (2, 2) or
               PT.N.DECL = (4, 1) or
               PT.N.DECL = (5, 1)    then
               if STS(1) /= STS(2)  and then 
                  ((STS(1) /= ZZZ_STEM) and (STS(2) /= ZZZ_STEM))  then
                  PROB("    EXPECTED IDENTICAL NOUN STEMS");
               end if;
            end if;
         
         
         --  Check that the stems progress as expected
            if PT.N.DECL = (1, 2)   and then
               ((STS(1) /= ZZZ_STEM)  or (STS(2) /= ZZZ_STEM))  then
               if LEN(STS(1)) >= 3                           and then  
               STS(1)(LEN(STS(1))..LEN(STS(1))) /= "r"  and then  
               STS(2)(LEN(STS(2))..LEN(STS(2))) /= "r"          then  
                  PROB("    EXPECTED  r and r   (1, 2)  NOUN STEMS");
               end if;
            end if;
         
         --  Check that N 2 4 has the 'i's
            if PT.N.DECL = (2, 4)    then
               if (LEN(STS(1)) >= 3)                          and then   
               
                  (( (STS(1) /= ZZZ_STEM)   and
                         (STS(1)(LEN(STS(1))..LEN(STS(1))) /= "i") )  or else  
                      ( (STS(1) /= ZZZ_STEM)   and
                           (STS(2)(LEN(STS(2))..LEN(STS(2))) /= "i") ))        then 
               
                  PROB("    EXPECTED  i and i   (2, 4)  NOUN STEMS");
               end if;
            end if;
         
         --   N 3 1 
         
         --  Check N 3 1 er is M/C
            if PT.N.DECL = (3, 1)     then
               if (LEN(STS(1)) >= 3                           and then  
                   STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "er")  and then  
                  (PT.N.GENDER /= M  and   PT.N.GENDER /= C)    then           
                  PROB("    EXPECTED  -er M/C for  N 3 1 NOUN STEMS");
               end if;
            end if;
         
         --  Check er -> er 
            if PT.N.DECL = (3, 1) and
            PT.N.GENDER = M        then
               if (LEN(STS(1)) >= 3                           and then  
                   STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "er")  and then  
                  ((STS(2)(LEN(STS(2))-1..LEN(STS(2))) /= "er")         or    
                      (STS(1)(1..LEN(STS(1))-2) /= STS(2)(1..LEN(STS(2))-2)))   then  
                  PROB("    EXPECTED  er -> er   N 3 1 NOUN STEMS");
               end if;
            end if;
         
         
         --  Check N 3 1 or is M
            if PT.N.DECL = (3, 1)     then
               if (LEN(STS(1)) >= 3                           and then  
                   STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "or")  and then  
               PT.N.GENDER /= M    then
                  PROB("    EXPECTED  -or M for  N 3 1 NOUN STEMS");
               end if;
            end if;
         
         --  Check or -> or 
            if PT.N.DECL = (3, 1) and
            PT.N.GENDER = M        then
               if (LEN(STS(1)) >= 3                           and then  
                   STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "or")  and then  
                  ((STS(2)(LEN(STS(2))-1..LEN(STS(2))) /= "or")         or    
                      (STS(1)(1..LEN(STS(1))-2) /= STS(2)(1..LEN(STS(2))-2)))   then  
                  PROB("    EXPECTED  or -> or   and S1=S2   N 3 1 NOUN STEMS");
               end if;
            end if;
         
         --  Check N 3 1 -o is M except -do, -go, -io
            if PT.N.DECL = (3, 1) and then
               (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "o")   and then             
            PT.N.GENDER /= M        then
               if (LEN(STS(1)) >= 3)                          and then  
                  ((STS(1)(LEN(STS(1))-2..LEN(STS(1))) /= "do")   and   
                      (STS(1)(LEN(STS(1))-2..LEN(STS(1))) /= "go")   and   
                      (STS(1)(LEN(STS(1))-2..LEN(STS(1))) /= "io"))  and then
               PT.N.GENDER /= F        then
                  PROB("    EXPECTED  N 3 1 -o M excpt -do, -go, -io F");
               end if;
            end if;
         
         --  Check io -> ion
            if PT.N.DECL = (3, 1) and
            PT.N.GENDER = F        then
               if LEN(STS(1)) >= 3                           and then  
               STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "io"  and then  
               STS(2)(LEN(STS(2))-2..LEN(STS(2))) /= "ion"          then  
                  PROB("    EXPECTED  io -> ion  NOUN STEMS");
               elsif LEN(STS(1)) >= 3                            and then  
               STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "io"   and then  
               STS(2)(LEN(STS(2))-2..LEN(STS(2))) = "ion"       then  
                  if STS(1)(1..LEN(STS(1))-1) /=    
                  STS(2)(1..LEN(STS(2))-2)        then  
                     PROB("    SUSPECT  io - ion  NOUN STEMS  misspelling");
                  end if;
               end if;
            end if;
         
         
         
         
            if PT.N.DECL = (3, 1) and
            PT.N.GENDER = F     and 
            LEN(STS(1)) >= 3                            then  
               if STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "do"  and then  
               STS(2)(LEN(STS(2))-2..LEN(STS(2))) /= "din"          then  
                  PROB("    EXPECTED  do -> din  NOUN  3 1 STEMS");
               end if;
            elsif STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "do"   and then  
            STS(2)(LEN(STS(2))-2..LEN(STS(2))) = "din"  then  
               if STS(1)(1..LEN(STS(1))-1) /=    
               STS(2)(1..LEN(STS(2))-2)        then  
                  PROB("    SUSPECT  do - din  NOUN STEMS  misspelling");
               end if;
            end if;
         
         --  Check as -> at/ad 
            if PT.N.DECL = (3, 1) and
            PT.N.GENDER = F        then
               if LEN(STS(1)) >= 3                           and then  
               STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "as"  and then  
              (STS(2)(LEN(STS(2))-1..LEN(STS(2))) /= "at"    and  
               STS(2)(LEN(STS(2))-1..LEN(STS(2))) /= "ad" )         then  
                  PROB("    EXPECTED  as -> at/ad   NOUN STEMS");
               end if;
            end if;
         
         
         
         --  Check -a, -at is N 3 2 N          
            if PT.N.DECL.WHICH  = 3   then
               if (LEN(STS(1)) >= 3)                          and then  
                  (STS(1)(LEN(STS(1))..LEN(STS(1))) = "a")  and then  
                  ((STS(2)(LEN(STS(2))-1..LEN(STS(2))) /= "at")    or 
                      (PT.N.DECL /= (3, 2)) )      then  
                  PROB("    EXPECTED  a -> at   NOUN STEMS  to be N 3 2 N");
               end if;
            end if;
         
         
         --  Check es/is -> I-stem
            if PT.N.DECL.WHICH = 3 and
               (PT.N.GENDER = M or PT.N.GENDER = F or PT.N.GENDER = C)    then
               if LEN(STS(1)) >= 3                           and then  
                  (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "es"  or        
                   STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "is")    and then  
               STS(1)(1..LEN(STS(1))-2) = STS(2)(1..LEN(STS(2)))    then  
                  if (PT.N.DECL.VAR /= 3  and PT.N.DECL.VAR /= 9) then
                     PROB("    EXPECTED  es/is I-stem (3, 3)");
                  end if;
               end if;
            end if;
         
         
         --  Check is I-stem -es is F                 G&L 58
            if PT.N.DECL = (3, 3) then
               if (LEN(STS(1)) >= 3)                           and then  
                  (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "es")    then  
                  if PT.N.GENDER /= F  then
                     PROB("    EXPECTED  -es to be F  N 3 3");
                  end if;
               end if;
            end if;
         
         
         --  Check ns/rs -> I-stem
            if PT.N.DECL.WHICH = 3 and
               (PT.N.GENDER = M or PT.N.GENDER = F or PT.N.GENDER = C)    then
               if LEN(STS(1)) >= 3                           and then  
                  (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "ns"  or        
                   STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "rs")    then  
                  if PT.N.DECL.VAR /= 3  then
                     PROB("    EXPECTED  ns/rs M/F I-stem (3, 3)");
                  end if;
               end if;
            end if;
         
         
         --  Check al/e  -> I-stem
            if PT.N.DECL.WHICH = 3 and
               (PT.N.GENDER = N)    then
               if LEN(STS(1)) >= 3                           and then  
                  (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "al"  or        
                   STS(1)(LEN(STS(1))..LEN(STS(1))) = "e")    then  
                  if PT.N.DECL.VAR /= 4  then
                     PROB("    EXPECTED  al/e neuter I-stem (3, 4)");
                  end if;
               end if;
            end if;
         
         
         --  Check N 3 starts the same
            if PT.N.DECL.WHICH = 3  then
               if LEN(STS(1)) >= 4                           and then  
                  (STS(1) /= ZZZ_STEM and STS(2) /= ZZZ_STEM)  and then
                  (STS(1)(LEN(STS(1))..LEN(STS(1))-1) /= 
                   STS(2)(LEN(STS(1))..LEN(STS(1))-1))    then  
                  PROB("    EXPECTED  1st and 2nd stems similiar for N 3 X");
               end if;
            end if;
         
         --  Check N 3 GENDER
            if (PT.N.DECL = (3, 1))  and  (PT.N.GENDER = N)   then
               PROB("    EXPECTED  N 3 1 not to be N");
            elsif (PT.N.DECL = (3, 2))  and  (PT.N.GENDER /= N)   then
               PROB("    EXPECTED  N 3 2  to be N");
            elsif (PT.N.DECL = (3, 3))  and  (PT.N.GENDER = N)   then
               PROB("    EXPECTED  N 3 3 not to be N");
            elsif (PT.N.DECL = (3, 4))  and  (PT.N.GENDER /= N)   then
               PROB("    EXPECTED  N 3 4 to be N");
            end if;
         
         
         
         elsif PT.POFS = PRON  then
            null;
         
         elsif PT.POFS = ADJ  then
         
         --  Can only check consistency if more than one stem, CO /= COMP | SUPER
            if (PT.ADJ.CO = POS or PT.ADJ.CO = X)  and  
               (PT.ADJ.DECL /= (9, 9)   and 
                PT.ADJ.DECL /= (9, 8))        then
            
            --  Check that the stems are the same when expected
               if (
                   (PT.ADJ.DECL = (3, 2)) or
                   ( (PT.ADJ.DECL = (3, 3)) and then
                     (STS(1)( LEN(STS(1))..LEN(STS(1)) ) /= "r")  ) ) and then
                       
                ( (STS(1) /= ZZZ_STEM) and (STS(2) /= ZZZ_STEM)  )    then
                  if STS(1) /= STS(2)  then 
                     PROB("    EXPECTED IDENTICAL ADJ STEMS");
                  end if;
               end if;
            
            
            --  Check that the stems progress as expected
               if PT.ADJ.DECL = (1, 2)   then
                  if LEN(STS(1)) >= 3                           and then  
                  STS(1)(LEN(STS(1))..LEN(STS(1))) /= "r"  and then  
                  STS(2)(LEN(STS(2))..LEN(STS(2))) /= "r"          then  
                     PROB("    EXPECTED  r and r   (1, 2)  ADJ STEMS");
                  end if;
               end if;
            
               if PT.ADJ.DECL = (3, 1)   then
                  if LEN(STS(1)) >= 3                           and then  
                  STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "ns"  and then  
                  STS(2)(LEN(STS(2))-1..LEN(STS(2))) /= "nt"          then  
                     PROB("    EXPECTED  ns -> nt  (3, 1)  ADJ STEMS");
                  end if;
               end if;
            
            
               if PT.ADJ.DECL.WHICH = 3   then
                  if LEN(STS(1)) >= 3                           and then  
                  STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "er"  and then  
                  PT.ADJ.DECL /= (3, 3)   then  
                     PROB("    EXPECTED  ADJ 3 with -er to be (3, 3)");
                  end if;
               end if;
            
            
               if PT.ADJ.DECL = (3, 1)   then
                  if (LEN(STS(1)) > LEN(STS(2)))    then  
                     PROB("    EXPECTED ADJ (3, 1)  1st stem to be shorter");
                  end if;
                  if (STS(1)(1..LEN(STS(1))-1) /=   
                      STS(2)(1..LEN(STS(1))-1))         then  
                     PROB("    EXPECTED ADJ (3, 1)  stems to agree in first letters");
                  end if;
               end if;
            
             end if;
               
            --  General ADJ things
            
            --  Check that ADJ 9 is POS    
               if PT.ADJ.DECL.WHICH = 9 and     
                  PT.ADJ.CO /= POS  then
                 PROB("    EXPECTED  ADJ 9 to be POS");
               end if;
               
              --  Check that ADJ 9 has 1 stem    
               if (PT.ADJ.DECL.WHICH = 9)    and     
                  (STS(2) /= NULL_STEM_TYPE)     then
                 PROB("    EXPECTED  ADJ 9 have just 1 stem");
               end if;
               
             --  Check that there are two and only two stems if POS     
               if PT.ADJ.CO = POS  and PT.ADJ.DECL /= (9, 9) and     
                  PT.ADJ.DECL /= (9, 8)    then
                  if (STS(3) /= NULL_STEM_TYPE  or 
                      STS(4) /= NULL_STEM_TYPE)     then
                     PROB("    EXPECTED  exactly 2  POS ADJ  STEMS");
                  end if;
               end if;
            
            --  Check that there are more than two stems if X           
               if PT.ADJ.CO = X    then
                  if (STS(3) = NULL_STEM_TYPE  or  
                      STS(4) = NULL_STEM_TYPE)     then
                     PROB("    EXPECTED  4  X  ADJ  STEMS");
                  end if;
               end if;
            
            
         
         --  Check that COMP ends in i, mostly
            if PT.ADJ.CO = X   then
               if (STS(3) /= NULL_STEM_TYPE  and 
                   STS(3) /= ZZZ_STEM)      then
                  if (STS(3)(LEN(STS(3))) /= 'i')  then
                     PROB("    EXPECTED  ADJ  STEM 3 to end in 'i'");
                  elsif STS(3)(1..LEN(STS(3))-1) /= STS(2)(1..LEN(STS(2)))  then
                     PROB("    EXPECTED  ADJ  STEM 3  = STEM 2 & 'i'");
                  end if;
               end if;
            end if;
         
         --  Check that SUPER ends in issi, mostly
            if PT.ADJ.CO = X    then
               if ((LEN(STS(3)) > 3) and  (LEN(STS(4)) > 4))    and then
                  ((STS(3) /= NULL_STEM_TYPE  and 
                      STS(3) /= ZZZ_STEM)    and then
                      (STS(4) /= NULL_STEM_TYPE  and 
                       STS(4) /= ZZZ_STEM))     then
                  if (STS(3)(LEN(STS(3))-3..LEN(STS(3))) = "cili")  then
                     if (STS(4)(LEN(STS(4))-4..LEN(STS(4))) /= "cilli") then
                        PROB("    EXPECTED  'cil' ADJ  STEM 4 to end in 'cilli'");
                     end if;
                  elsif (STS(3)(LEN(STS(3))-3..LEN(STS(3))) = "mili")  then
                     if (STS(4)(LEN(STS(4))-4..LEN(STS(4))) /= "milli") then
                        PROB("    EXPECTED  'mil' ADJ  STEM 4 to end in 'milli'");
                     end if;
                  elsif (STS(3)(LEN(STS(3))-1..LEN(STS(3))) = "ri")  then
                     if (STS(4)(LEN(STS(4))-2..LEN(STS(4))) /= "rri") then
                        PROB("    EXPECTED  'r' ADJ  STEM 4 to end in 'rri'");
                     end if;
                  elsif STS(4)(LEN(STS(4))-3..LEN(STS(4))) /= "issi"  then
                     PROB("    EXPECTED  ADJ  STEM 4 to end in 'issi'");
                  elsif STS(4)(1..LEN(STS(4))-3) /= STS(3)(1..LEN(STS(3)))  then
                     PROB("    EXPECTED  ADJ  STEM 4 to be STEM 3 & 'ssi'");
                  elsif STS(4)(1..LEN(STS(4))-4) /= STS(2)(1..LEN(STS(2)))  then
                     PROB("    EXPECTED  ADJ  STEM 4 to be STEM 2 & 'issi'");
                  end if;
               end if;
            end if;
         
         --  Check that COMP and SUPER are (0, 0)
            if ((PT.ADJ.CO = COMP) or 
                   (PT.ADJ.CO = SUPER))     and then
               (PT.ADJ.DECL /= (0, 0))         then
               PROB("    EXPECTED  ADJ  COMP/SUPER to be (0, 0)");
            end if;
         
         --  Check that COMP and SUPER have only one stem
            if ((PT.ADJ.CO = COMP) or 
                   (PT.ADJ.CO = SUPER))     and then
               (STS(2) /= NULL_STEM_TYPE)         then
               PROB("    EXPECTED  ADJ  COMP/SUPER to have only one stem");
            end if;
         
         
         elsif PT.POFS = ADV  then
         
         
         --  Can only check consistency if more than one stem, CO /= COMP | SUPER
            if (PT.ADV.CO = POS or PT.ADV.CO = X)  then
            
            --  Check that there are two and only two stems if POS     
               if PT.ADV.CO = POS     then
                  if (STS(2) /= NULL_STEM_TYPE  or 
                      STS(3) /= NULL_STEM_TYPE)     then
                     PROB("    EXPECTED  exactly 1  POS ADV  STEM ");
                  end if;
               end if;
            
            --  Check that there are more than two stems if X           
               if PT.ADV.CO = X   then
                  if (STS(2) = NULL_STEM_TYPE  or  
                      STS(3) = NULL_STEM_TYPE)     then
                     PROB("    EXPECTED  3  X  ADV  STEMS");
                  end if;
               end if;
            
            end if;   
         
         
         --  Check that COMP  ends in ius, mostly
            if PT.ADV.CO = X        then
               if (STS(2) /= NULL_STEM_TYPE  and 
                   STS(2) /= ZZZ_STEM)     and then
                  (STS(2)(LEN(STS(2))-2..LEN(STS(2))) /= "ius")  then
                  PROB("    EXPECTED  ADV  STEM 2 to end in 'ius'");
               end if;
            end if;
         
         --  Check that SUPER ends in ime, mostly
            if PT.ADV.CO = X        then
               if (STS(3) /= NULL_STEM_TYPE  and 
                   STS(3) /= ZZZ_STEM)     and then
                  (STS(3)(LEN(STS(3))-2..LEN(STS(3))) /= "ime")  then
                  PROB("    EXPECTED  ADV  STEM 3 to end in 'ime'");
               end if;
            end if;
         
         --  Check that SUPER ends in issime, mostly
            if PT.ADV.CO = X      then
               if ((LEN(STS(2)) > 4) and  (LEN(STS(3)) > 6))    and then
                  ((STS(2) /= NULL_STEM_TYPE  and 
                      STS(2) /= ZZZ_STEM)    and then
                      (STS(3) /= NULL_STEM_TYPE  and 
                       STS(3) /= ZZZ_STEM))     then
                  if (STS(2)(LEN(STS(2))-5..LEN(STS(2))) = "cilius")  then
                     if (STS(3)(LEN(STS(3))-6..LEN(STS(3))) /= "cillime") then
                        PROB("    EXPECTED  'cil' ADV  STEM 3 to end in 'cillime'");
                     end if;
                  elsif(STS(2)(LEN(STS(2))-5..LEN(STS(2))) = "milius")  then
                     if (STS(3)(LEN(STS(3))-6..LEN(STS(3))) /= "millime") then
                        PROB("    EXPECTED  'mil' ADV  STEM 3 to end in 'millime'");
                     end if;
                  elsif (STS(2)(LEN(STS(2))-3..LEN(STS(2))) = "rius")  then
                     if (STS(3)(LEN(STS(3))-4..LEN(STS(3))) /= "rrime") then
                        PROB("    EXPECTED  'r' ADV  STEM 3 to end in 'rrime'");
                     end if;
                  elsif STS(3)(LEN(STS(3))-5..LEN(STS(3))) /= "issime"  then
                     PROB("    EXPECTED  ADV  STEM 3 to end in 'issime'");
                  end if;
               end if;
            end if;
         
         
         
         elsif PT.POFS = V  then 
               
           --  Check that V 9 9 has ony one stem
           if PT.V.CON = (9, 9)    then
             if (STS(2) /= NULL_STEM_TYPE  or 
                 STS(3) /= NULL_STEM_TYPE  or  
                 STS(4) /= NULL_STEM_TYPE)     then
                PROB("    EXPECTED  exactly 1  V (9, 9)  STEM ");
              end if;
            
            else
        
           
            --  Check to see no first verb stem has lingering 'o'
            if (STS(1)(LEN(STS(1)))  =  'o')  then 
               PROB("    EXPECTED VERB not to have -o 1st stem");
            end if;
         
         --  Check to see no third verb stem has lingering 'i'
            if (STS(3)(LEN(STS(3)))  =  'i')  and
               (PT.V.CON /= (6, 1))  then 
               PROB("    EXPECTED VERB not to have -i 3rd stem");
            end if;
         
         --  Check that the stems are the same when expected
            if PT.V.CON.WHICH < 5  and  STS(1)(1..3) /= "zzz"   then
               if (PT.V.CON  /= (3, 1) and
                   PT.V.CON  /= (3, 2) and
                   PT.V.CON  /= (3, 4))   then
                  if STS(1) /= STS(2)  then 
                     PROB("    EXPECTED IDENTICAL VERB 1 & 2 STEMS");
                  end if;
               elsif PT.V.CON.WHICH  = 2 and then
                  (STS(1)(LEN(STS(1)))  =  'e')  then 
                  PROB("    EXPECTED (2, X) not to have -e 1st stem");
               elsif PT.V.CON  = (3, 1) and then
               STS(1) /= STS(2)  and then 
                  (STS(1)(1..LEN(STS(1)))  /= 
                   STS(2)(1..LEN(STS(2))) & 'i')  then 
                  PROB("    EXPECTED (3, 1) i-stem  VERB  STEMS");
               elsif PT.V.CON  = (3, 4) and then
                  (STS(1)(1..LEN(STS(1)))  /= 
                   STS(2)(1..LEN(STS(2))) & 'i')  then 
                  PROB("    EXPECTED (3, 4) i-stem  VERB  STEMS");
               elsif PT.V.CON  = (3, 2)  then
                  if ((STS(1)(LEN(STS(1))-2..LEN(STS(1)))  /=  "fer") or      
                         (STS(2)(LEN(STS(2))-3..LEN(STS(2)))  /=  "ferr"))  or      
                     (STS(1)(1..LEN(STS(1))-3)  /= 
                      STS(2)(1..LEN(STS(2))-4))  then 
                     PROB("    EXPECTED (3, 2) fer   VERB  STEMS");
                  end if;
               end if;
            end if;
         
         
         
         
         --  Check that the last 2 verb stems progress as expected
            if PT.V.CON  = (1, 1)   and then 
               (STS(3) /= ZZZ_STEM and  STS(4) /= ZZZ_STEM)  then 
            
               if LEN(STS(3)) >= 3                           and then  
                  (STS(3)(LEN(STS(3))-1..LEN(STS(3))) = "av")     then  
                  if STS(4)(LEN(STS(4))-1..LEN(STS(4))) /= "at"   or 
                     (STS(3)(1..LEN(STS(3))-2)  /= 
                      STS(4)(1..LEN(STS(4))-2))  then 
                     PROB("    EXPECTED  (1, 1) 3/4  av -> at   VERB STEMS");
                  end if;
               elsif LEN(STS(3)) >= 4                           and then  
                  (STS(3)(LEN(STS(3))-2..LEN(STS(3))) = "ubu")     then  
                  if STS(4)(LEN(STS(4))-3..LEN(STS(4))) /= "ubit"   or 
                     (STS(3)(1..LEN(STS(3))-3)  /= 
                      STS(4)(1..LEN(STS(4))-4))  then 
                     PROB("    EXPECTED  (1, 1) 3/4 ubu -> ubit VERB STEMS");
                  end if;
               elsif LEN(STS(3)) >= 4                           and then  
                  (STS(3)(LEN(STS(3))-2..LEN(STS(3))) = "icu")     then  
                  if STS(4)(LEN(STS(4))-3..LEN(STS(4))) /= "icit"   or 
                     (STS(3)(1..LEN(STS(3))-3)  /= 
                      STS(4)(1..LEN(STS(4))-4))  then 
                     PROB("    EXPECTED  (1, 1) 3/4 icu -> icit VERB STEMS");
                  end if;
               elsif LEN(STS(3)) >= 4                           and then  
                  (STS(3)(LEN(STS(3))-2..LEN(STS(3))) = "onu")     then  
                  if STS(4)(LEN(STS(4))-3..LEN(STS(4))) /= "onit"   or 
                     (STS(3)(1..LEN(STS(3))-3)  /= 
                      STS(4)(1..LEN(STS(4))-4))  then 
                     PROB("    EXPECTED  (1, 1) 3/4 onu -> onit VERB STEMS");
                  end if;
               elsif (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "st")  then  --  sto bad
                  PROB("           V  (1, 1) 'st' verb  ???? VERB STEMS");
               else   
                  PROB("    EXPECTED  (1, 1) 3/4 regular     VERB STEMS");
               end if;
            
            end if;
         
         
         
         
            if PT.V.CON  = (3, 1)   and then 
               (STS(1) /= ZZZ_STEM  and then STS(2) /= ZZZ_STEM  and then
                STS(3) /= ZZZ_STEM  and then STS(4) /= ZZZ_STEM)  then
               if LEN(STS(1)) >= 4                           and then  
                  (STS(3)(LEN(STS(3))-1..LEN(STS(3))) = "ec")     then  
                  if (STS(1)(LEN(STS(1))-3..LEN(STS(1))) = "faci")  and then  
                     ((STS(2)(1..LEN(STS(2))) /= STS(1)(1..LEN(STS(1))-4) & "fac") or
                         (STS(3)(1..LEN(STS(3))) /= STS(1)(1..LEN(STS(1))-4) & "fec") or
                         (STS(4)(1..LEN(STS(4))) /= STS(1)(1..LEN(STS(1))-4) & "fact")) then
                     PROB("    EXPECTED  (3, 1) 3/4  feci, fec, fec, fact  VERB STEMS");
                  end if;
                  if (STS(1)(LEN(STS(1))-3..LEN(STS(1))) = "fici")  and then  
                     ((STS(2)(1..LEN(STS(2))) /= STS(1)(1..LEN(STS(1))-4) & "fic") or
                         (STS(3)(1..LEN(STS(3))) /= STS(1)(1..LEN(STS(1))-4) & "fec") or
                         (STS(4)(1..LEN(STS(4))) /= STS(1)(1..LEN(STS(1))-4) & "fect")) then
                     PROB("    EXPECTED  (3, 1) 3/4  fici, fic, fec, fect  VERB STEMS");
                  end if;
               end if;
            elsif PT.V.CON  = (3, 1)  and then 
            STS(4) /= ZZZ_STEM  then
               if LEN(STS(3)) >= 3                           and then  
                  (STS(3)(LEN(STS(3))..LEN(STS(3))) = "x")     then  
                  if STS(3)(LEN(STS(3))-1..LEN(STS(3))) = "nx"   then
                     if not ((STS(4)(LEN(STS(4))-2..LEN(STS(4))) = "nct"   and
                                STS(3)(1..LEN(STS(3))-1) = STS(4)(1..LEN(STS(4))-3))  or 
                                (STS(4)(LEN(STS(4))-1..LEN(STS(4))) = "ct"   and
                                 STS(3)(1..LEN(STS(3))-1) = STS(4)(1..LEN(STS(4))-3))) then
                        PROB("    EXPECTED  (3, 1) 3/4  nx -> (n)ct   VERB STEMS");
                     end if;
                  elsif STS(3)(LEN(STS(3))-2..LEN(STS(3))) = "fix"   then
                     if(STS(3)(1..LEN(STS(3)))  /= 
                        STS(4)(1..LEN(STS(4))))  then 
                        PROB("    EXPECTED  (3, 1) 3/4  fix -> fix  VERB STEMS");
                     end if;
                  elsif LEN(STS(3)) >= 4                           and then  
                  STS(3)(LEN(STS(3))-3..LEN(STS(3))) = "flex"   then
                     if(STS(3)(1..LEN(STS(3)))  /= 
                        STS(4)(1..LEN(STS(4))))  then 
                        PROB("    EXPECTED  (3, 1) 3/4  flex -> flex  VERB STEMS");
                     end if;
                  elsif LEN(STS(3)) >= 4                           and then  
                  STS(3)(LEN(STS(3))-3..LEN(STS(3))) = "flux"   then
                     if(STS(3)(1..LEN(STS(3)))  /= 
                        STS(4)(1..LEN(STS(4))))  then 
                        PROB("    EXPECTED  (3, 1) 3/4  flux -> flux  VERB STEMS");
                     end if;
                  elsif STS(4)(LEN(STS(4))-1..LEN(STS(4))) /= "ct"   or 
                     (STS(3)(1..LEN(STS(3))-1)  /= 
                      STS(4)(1..LEN(STS(4))-2))  then 
                     PROB("    EXPECTED  (3, 1) 3/4  x  -> ct   VERB STEMS");
                  end if;
               end if;
            end if;
         
         --  Check DEP has no third stem
            if DE.PART.V.KIND = DEP  and then
            STS(3)(1..3) /= "zzz"         then  
               PROB("    EXPECTED  3 = zzz  DEPON VERB STEMS");
            end if;
         
           end if;  -- V
           
           
           
         elsif PT.POFS = NUM  then
            null;
         
         
         else
         
            null;
         
         end if;
      
      
      --  Catch others
         if (PT.POFS = N)  or   
            (PT.POFS = ADJ)  then
            if LEN(STS(1)) >= 3                           and then  
            STS(1)(LEN(STS(1))..LEN(STS(1))) = "u"  and then  
            IS_VOWEL(STS(2)(LEN(STS(2))-1))           then  
               PROB("    CHECK for terminal u or v      ");
            end if;
            if LEN(STS(1)) >= 3                           and then  
            STS(1)(LEN(STS(1))..LEN(STS(1))) = "v"  and then  
            not IS_VOWEL(STS(2)(LEN(STS(2))-1))           then  
               PROB("    CHECK for terminal u or v      ");
            end if;
         end if;
      
         if (PT.POFS = V)  and then
            (PT.V.CON = (2, 1))  then
            if (LEN(STS(1)) >= 3)                           and then  
               (STS(1)(LEN(STS(1))-1..LEN(STS(1))) = "pl")  and then  
               (STS(3)(LEN(STS(3))-1..LEN(STS(3))) /= "ev")    then  
               PROB("    EXPECTED pleo -> plev  V 2 1   ");
            end if;
         end if;
      
      
         exception
            when others   => 
               PUT_LINE("VERIFY_STEMS exception        !!!!!!!!!!!!!!!!!!     " & STS(1));
               PUT_LINE(S(1..LAST));
               PUT_LINE(OUTPUT, "VERIFY_STEMS exception        !!!!!!!!!!!!!!!!!!     " & STS(1));
               PUT_LINE(OUTPUT, S(1..LAST));
      --CLOSE(OUTPUT);
      end VERIFY_STEMS;    
   
   begin
      PUT_LINE("Takes a DICTLINE form named CHECK.IN, analyzes for possible errors, and" );
      PUT_LINE("produces a report CHECK.OUT - Remember to process CHECK.OUT from end");
      CREATE(OUTPUT, OUT_FILE, "CHECK.OUT");
      OPEN(INPUT, IN_FILE, "CHECK.IN");
   
      while not END_OF_FILE(INPUT) loop
         S := BLANK_LINE;
         GET_LINE(INPUT, S, LAST);
      --PUT_LINE(S(1..LAST));
         LINE_NUMBER := LINE_NUMBER + 1;
         begin
         
           if S(19)  /= ' '  or
              S(38)  /= ' '  or
              S(57)  /= ' '  or
              S(76)  /= ' '  or
              S(102) /= ' '    then
               NUMBER := NUMBER + 1;
               PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
               PUT_LINE(OUTPUT, "      BLANKS not in right place");
               PUT_LINE(OUTPUT, S(1..LAST));
            end if;
         
            if S(112)  = ' ' then
               NUMBER := NUMBER + 1;
               PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
               PUT_LINE(OUTPUT, "      FLAGS may be offset");
               PUT_LINE(OUTPUT, S(1..LAST));
            end if;
         
            if (LAST > 190 and then S(191) /= ' ')  then
               NUMBER := NUMBER + 1;
               PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
               PUT_LINE(OUTPUT, "      LINE is too long");
               PUT_LINE(OUTPUT, S(1..LAST));
            end if;
         
         
            DE.STEMS(1) := S(1..MAX_STEM_SIZE);
            DE.STEMS(2) := S(MAX_STEM_SIZE+2..2*MAX_STEM_SIZE+1);
            DE.STEMS(3) := S(2*MAX_STEM_SIZE+3..3*MAX_STEM_SIZE+2);
            DE.STEMS(4) := S(3*MAX_STEM_SIZE+4..4*MAX_STEM_SIZE+3);
         
            for I in STEM_KEY_TYPE range 1..4  loop
               if HAS_PUNCTUATION(DE.STEMS(I))  then
                  PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
                  PUT_LINE(OUTPUT, "   Offset or Punctuation in line      ");
                  PUT_LINE(OUTPUT, S(1..4*MAX_STEM_SIZE+3)); 
               end if;
            end loop;
         
         
            GET(S(4*MAX_STEM_SIZE+5..LAST), DE.PART, LL);
            --GET(S(L+1..LAST), DE.PART.POFS, DE.PART.POFS.KIND, LL);
            
            DE.MEAN := S(111..110+MAX_MEANING_SIZE);
         
         --if LOWER_CASE(S(1..86)) = LOWER_CASE(OLDLINE(1..86)) and then
         --      --  This way I get N  2 1 and 2 2 not duplicate
         --      --  If I make it 1..83, I catch a lot more, a few of which might be ???
         --      --  If I make it 1..88 or 95, I catch a lot less
         --        (OLDLINE(90..95) /= "IMPERS")  and then
         --        not ((LEN(DE.STEMS(1)) >= 2)  and then
         --            (DE.STEMS(1)(LEN(DE.STEMS(1))-1..LEN(DE.STEMS(1))) = "qu"))  then
         --        NUMBER := NUMBER + 1;
         --        PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
         --        PUT_LINE(OUTPUT, "   Possible duplicate lines ");
         --        PUT_LINE(OUTPUT, S(1..LAST));
         --      end if;
         --      OLDLINE(1..190) := S(1..190);
         
            VERIFY_STEMS;
         
            exception
               when others =>
                  PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
                  PUT_LINE(OUTPUT, "      Exception");
                  PUT_LINE(OUTPUT, S(1..LAST));
                  PUT(OUTPUT, DE); NEW_LINE(OUTPUT);
         end;
      end loop;
   
      NEW_LINE(OUTPUT, 3);
      PUT(OUTPUT, "Number of entries = "); 
      PUT(OUTPUT, LINE_NUMBER); NEW_LINE(OUTPUT); 
      PUT(OUTPUT, "Number of errors  = "); 
      PUT(OUTPUT, NUMBER); NEW_LINE(OUTPUT);
      PUT(OUTPUT, "Ratio             = 1 : "); 
      PUT(OUTPUT, LINE_NUMBER/NUMBER); NEW_LINE(OUTPUT);
      CLOSE(OUTPUT);
   
      NEW_LINE;
      PUT("Number of entries = "); PUT(LINE_NUMBER); NEW_LINE;
      PUT("Number of errors  = "); PUT(NUMBER); NEW_LINE;
      PUT("Ratio             = 1 :"); PUT(LINE_NUMBER/NUMBER); NEW_LINE;
   
      exception
         when NAME_ERROR  => 
            PUT_LINE("No CHECK.IN file to process");
            CLOSE(OUTPUT);
         
         when others =>
            PUT("Exception on LINE"); PUT(LINE_NUMBER); NEW_LINE;
            PUT_LINE(S(1..LAST));
            CLOSE(OUTPUT);
   
   end CHECK;
