   with TEXT_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with UNIQUES_PACKAGE; use UNIQUES_PACKAGE;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
   procedure LIST_SWEEP(PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is
   --  This procedure is supposed to process the output PARSE_ARRAY at PA level
   --  before it get turned into SIRAA and DMNPCA in LIST_PACKAGE
   --  Since it does only PARSE_ARRAY it is just cheaking INFLECTIONS, not DICTIOARY
   
      use INFLECTION_RECORD_IO;
      use DICT_IO;
      
      PR, OPR : PARSE_RECORD := NULL_PARSE_RECORD;
      DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
      I, J, JJ : INTEGER := 0;
      DIFF_J : INTEGER := 0;
   
   
      NOT_ONLY_ARCHAIC  : BOOLEAN := FALSE;
      NOT_ONLY_MEDIEVAL : BOOLEAN := FALSE;
      NOT_ONLY_UNCOMMON : BOOLEAN := FALSE;
   
   
      function ALLOWED_STEM(PR : PARSE_RECORD) return BOOLEAN is
         ALLOWED : BOOLEAN := TRUE;   --  modify as necessary and return it
         --DE : DICTIONARY_ENTRY;
      begin
--TEXT_IO.PUT("ALLOWED? >"); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
         if PR.D_K not in GENERAL..LOCAL  then 
            return TRUE; end if;
            
           --DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.MNPC);
           --DICT_IO.READ(DICT_FILE(PR.D_K), DE);
           
           DICT_IO.READ(DICT_FILE(PR.D_K), DE, PR.MNPC);

--TEXT_IO.PUT("ALLOWED? >"); DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;
           
                    
         -- if PR.D_K in GENERAL..UNIQUE  then
            -- if (DE.TRAN.AGE = X) or else (DE.TRAN.AGE > A)  then
               -- NOT_ONLY_ARCHAIC_STEM := TRUE;
            -- end if;
            -- if DE.TRAN.AGE < F  then     --  Or E????
               -- NOT_ONLY_MEDIEVAL_STEM := TRUE;
            -- end if;
            -- if DE.TRAN.FREQ < E then  --     --  E for DICTLINE is uncommon  !!!!
               -- NOT_ONLY_UNCOMMON_STEM := TRUE;
            -- end if;
         -- end if; 
      
      --  NOUN CHECKS
      
         case  PR.IR.QUAL.POFS is
         
            when N  =>
            
            
               if  WORDS_MDEV(FOR_WORD_LIST_CHECK)  then
                  if (NOM <= PR.IR.QUAL.N.CS) and then
                     (S <= PR.IR.QUAL.N.NUMBER) then
                     ALLOWED := TRUE;
                  elsif (NOM <= PR.IR.QUAL.N.CS) and then
                     (PR.IR.QUAL.N.NUMBER = P) then
                     SEARCH_FOR_PL:
                     declare
                        DE : DICTIONARY_ENTRY;
                        MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
                     begin
                        ALLOWED := FALSE;
                        DICT_IO.READ(DICT_FILE(PR.D_K), DE, PR.MNPC);
                        MEAN := DE.MEAN;
                        for J in MEANING_TYPE'FIRST..MEANING_TYPE'LAST-2  loop
                           if MEAN(J..J+2) = "pl."  then
                              ALLOWED := TRUE;
                              exit;
                           end if;
                        end loop;
                     end SEARCH_FOR_PL;
                  --====================================
                  else
                     ALLOWED := FALSE;
                  end if;
               end if;
         
         
            when  ADJ  =>
            
            
            
               if  WORDS_MDEV(FOR_WORD_LIST_CHECK)  then
                  if (NOM <= PR.IR.QUAL.ADJ.CS) and then
                     (S <= PR.IR.QUAL.ADJ.NUMBER) and then
                     (M <= PR.IR.QUAL.ADJ.GENDER)  then
                     ALLOWED := TRUE;
                  else
                     ALLOWED := FALSE;
                  end if;
               end if;
         
         
         
         
         
         
         
         --  VERB CHECKS
         
            when  V  =>  
            --TEXT_IO.PUT("VERB  ");      
            --  Check for Verb 3 1  dic/duc/fac/fer shortened imperative
            --  See G&L 130.5
               declare
                  STEM : constant STRING := TRIM(PR.STEM);
                  LAST_THREE : STRING(1..3);
               begin
                  if (PR.IR.QUAL.V = ((3, 1), (PRES, ACTIVE, IMP), 2, S))  and 
                     (PR.IR.ENDING.SIZE = 0)  then    --  For this special case
                     if STEM'LENGTH >= 3  then
                        LAST_THREE := STEM(STEM'LAST-2..STEM'LAST);
                        if (LAST_THREE = "dic")  or
                           (LAST_THREE = "duc")  or
                           (LAST_THREE = "fac")  or
                           (LAST_THREE = "fer")  then   
                           null;
                        else 
                           ALLOWED := FALSE;
                        end if;
                     else 
                        ALLOWED := FALSE;
                     end if;
                  end if;
               end;
            
            --  Check for Verb Imperative being in permitted person
               if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = IMP) then
                  if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = PRES) and
                     (PR.IR.QUAL.V.PERSON = 2)  then
                     null;
                  elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT) and
                     (PR.IR.QUAL.V.PERSON = 2 or PR.IR.QUAL.V.PERSON = 3)  then
                     null;
                  else
                  --PUT("IMP not in permitted person  "); PUT(PR.IR); NEW_LINE;
                     ALLOWED := FALSE;
                  end if;
               end if;
            
            --  Check for V IMPERS and demand that only 3rd person    --  ???????
               if (DE.PART.V.KIND = IMPERS) then
                  if (PR.IR.QUAL.V.PERSON = 3)  then
                     null;
                  else
                  --PUT("IMPERS not in 3rd person     "); PUT(PR.IR); NEW_LINE;
                     ALLOWED := FALSE;
                  end if;
               end if;
            
            --  Check for V DEP    and demand PASSIVE   
               if (DE.PART.V.KIND = DEP) then
               --TEXT_IO.PUT("DEP  ");      
                  if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = INF)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT)  then
                  --TEXT_IO.PUT("PASSIVE  ");      
                  --TEXT_IO.PUT("DEP    FUT INF not in ACTIVE "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                     ALLOWED := TRUE;
                  elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..INF)  then
                  --TEXT_IO.PUT("ACTIVE  ");      
                  --TEXT_IO.PUT("DEP    not in PASSIVE     NOT ALLOWED   "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                     ALLOWED := FALSE;
                  else
                  --TEXT_IO.PUT("??????  ");      
                     null;
                  end if;
               end if;
            
            --  Check for V SEMIDEP    and demand PASSIVE ex Perf  
               if (DE.PART.V.KIND = SEMIDEP) then
                  if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = PASSIVE)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE in PRES..FUT)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..IMP)  then
                  --PUT("SEMIDEP    Pres not in ACTIVE "); PUT(PR.IR); NEW_LINE;
                     ALLOWED := FALSE;
                  elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE in PERF..FUTP )  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..IMP)  then
                  --PUT("SEMIDEP    Perf not in PASSIVE "); PUT(PR.IR); NEW_LINE;
                     ALLOWED := FALSE;
                  else
                     null;
                  end if;
               end if;
            
            
            
               if  WORDS_MDEV(FOR_WORD_LIST_CHECK)  then
                  if (PR.IR.QUAL.V.PERSON = 1) and then
                     (PR.IR.QUAL.V.NUMBER = S)  then
                     if ((DE.PART.V.KIND in X..INTRANS)  and
                            (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, ACTIVE, IND))) or else
                        ((DE.PART.V.KIND = DEP)  and
                            (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, PASSIVE, IND))) or else
                        ((DE.PART.V.KIND = SEMIDEP)  and
                            (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, ACTIVE, IND))) then
                        ALLOWED := TRUE;
                     elsif ((DE.PART.V.KIND = PERFDEF)  and
                               (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PERF, ACTIVE, IND))) then
                        ALLOWED := TRUE;
                     else
                        ALLOWED := FALSE;
                     end if;
                  elsif (DE.PART.V.KIND = IMPERS) then
                     if (PR.IR.QUAL.V.PERSON = 3)  and then
                        (PR.IR.QUAL.V.NUMBER = S)  and then
                        (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, ACTIVE, IND))   then
                        ALLOWED := TRUE;
                     else
                        ALLOWED := FALSE;
                     end if;
                  else
                     ALLOWED := FALSE;
                  end if;
               end if;
         
         
         
         
            when  others  =>
               null;
         
         end case;
      
      
         if  WORDS_MDEV(FOR_WORD_LIST_CHECK)   then       --  Non parts
            if (PR.IR.QUAL.POFS in VPAR..SUPINE)    then
               ALLOWED := FALSE;
            end if;
         end if;                                           --  Non parts
 --TEXT_IO.PUT_LINE("Returning FOR ALLOWED    " & BOOLEAN'IMAGE(ALLOWED));
         return ALLOWED;
      
      end ALLOWED_STEM;
   
   
              -----------------------------------------------------------
   
   
      procedure ORDER_PARSE_ARRAY(SL: in out PARSE_ARRAY; DIFF_J : out INTEGER) is
         use INFLECTION_RECORD_IO;
         use DICT_IO;
      
         HITS : INTEGER := 0;
         SL_FIRST : INTEGER := SL'FIRST;
         SL_LAST : INTEGER := SL'LAST;
         SL_LAST_INITIAL : INTEGER := SL_LAST;
         SM : PARSE_RECORD;
         --DE, ODE : DICTIONARY_ENTRY;
         ROMAN_NUMBER      : BOOLEAN := FALSE;
         HAS_NOUN_ABBREVIATION      : BOOLEAN := FALSE;
      --     HAS_ADJECTIVE_ABBREVIATION  : BOOLEAN := FALSE;
      --     HAS_VERB_ABBREVIATION  : BOOLEAN := FALSE;
         NOT_ONLY_VOCATIVE : BOOLEAN := FALSE;
         NOT_ONLY_LOCATIVE : BOOLEAN := FALSE;
      
         J : INTEGER := SL'FIRST;
      
         function DEPR (PR : PARSE_RECORD) return DICTIONARY_ENTRY is
            DE : DICTIONARY_ENTRY;
         begin
--TEXT_IO.PUT("DEPR  "); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
            if PR.MNPC = NULL_MNPC  then
               return NULL_DICTIONARY_ENTRY;
            else
              if PR.D_K in GENERAL..LOCAL  then
                --if PR.MNPC /= OMNPC  then
                  DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.MNPC);
                  DICT_IO.READ(DICT_FILE(PR.D_K), DE);
                  --OMNPC := PR.MNPC;
                  --ODE := DE;
                --else 
                  --DE := ODE;
                --end if;
              elsif PR.D_K = UNIQUE  then
                DE :=  UNIQUES_DE(PR.MNPC);
              end if;
            end if;
--  DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.MNPC);
--               DICT_IO.READ(DICT_FILE(PR.D_K), DE);
--TEXT_IO.PUT_LINE("Returning from DEPR   MNPC = " & INTEGER'IMAGE(INTEGER(PR.MNPC)) & "  ");
--DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;
           return DE;
         end DEPR;         
      
      begin
      
         if SL'LENGTH = 0              then
            return;
         end if;
      
      
      --  Bubble sort since this list should usually be very small (1-5)
      HIT_LOOP:
         loop
            HITS := 0;
         
         
         --------------------------------------------------
         
         
            SWITCH:
            declare
            
               function "<" (LEFT, RIGHT : QUALITY_RECORD) return BOOLEAN is
               begin
                  if LEFT.POFS = RIGHT.POFS  and then
                  LEFT.POFS = PRON        and then
                  LEFT.PRON.DECL.WHICH = 1    then
                     return (LEFT.PRON.DECL.VAR < RIGHT.PRON.DECL.VAR);
                  else
                     return INFLECTIONS_PACKAGE."<"(LEFT, RIGHT);
                  end if;
               end "<";
            
               function EQU (LEFT, RIGHT : QUALITY_RECORD) return BOOLEAN is
               begin
               
                  if LEFT.POFS = RIGHT.POFS  and then
                  LEFT.POFS = PRON        and then
                  LEFT.PRON.DECL.WHICH = 1    then
                  
                     return (LEFT.PRON.DECL.VAR = RIGHT.PRON.DECL.VAR);
                  else
                  
                     return INFLECTIONS_PACKAGE."="(LEFT, RIGHT);
                  end if;
               
               end EQU;
            
            
            
               function MEANING (PR : PARSE_RECORD) return MEANING_TYPE is
               begin
                  return DEPR(PR).MEAN;
               end MEANING;
            
            begin
            --  Need to remove duplicates in ARRAY_STEMS
            --  This sort is very sloppy
            --  One problem is that it can mix up some of the order of PREFIX, XXX, LOC
            --  I ought to do this for every set of results from different approaches
            --  not just in one fell swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            
            INNER_LOOP:
               for I in SL'FIRST..SL_LAST-1  loop
               --  Maybe <   =  on PR.STEM  -  will have to make up "<"   --  Actually STEM and PART  --  and check that later in print
                  if SL(I+1).D_K  > SL(I).D_K   or else  --  Let DICT.LOC list first
                  
                     (SL(I+1).D_K  = SL(I).D_K    and then
                      SL(I+1).MNPC  < SL(I).MNPC)   or else
                  
                     (SL(I+1).D_K  = SL(I).D_K    and then
                      SL(I+1).MNPC  = SL(I).MNPC    and then
                      SL(I+1).IR.QUAL < SL(I).IR.QUAL)  or else
                  
                     (SL(I+1).D_K  = SL(I).D_K    and then
                      SL(I+1).MNPC  = SL(I).MNPC    and then
                      EQU(SL(I+1).IR.QUAL, SL(I).IR.QUAL)  and then
                      MEANING(SL(I+1)) < MEANING(SL(I)))  or else   --  | is > letter
                  
                     (SL(I+1).D_K  = SL(I).D_K  and then
                      SL(I+1).MNPC  = SL(I).MNPC    and then
                      EQU(SL(I+1).IR.QUAL, SL(I).IR.QUAL)  and then
                      MEANING(SL(I+1)) = MEANING(SL(I))   and then
                      SL(I+1).IR.ENDING.SIZE < SL(I).IR.ENDING.SIZE)    or else
                  
                     (SL(I+1).D_K  = SL(I).D_K  and then
                      SL(I+1).MNPC  = SL(I).MNPC    and then
                      EQU(SL(I+1).IR.QUAL, SL(I).IR.QUAL)  and then
                      MEANING(SL(I+1)) = MEANING(SL(I))   and then
                      SL(I+1).IR.ENDING.SIZE = SL(I).IR.ENDING.SIZE  and then
                      INFLECTIONS_PACKAGE."<"(SL(I+1).IR.QUAL, SL(I).IR.QUAL))
                  then
                  
                  
                     SM := SL(I);
                     SL(I) := SL(I+1);
                     SL(I+1) := SM;
                     HITS := HITS + 1;
                  
                  end if;
               
               end loop INNER_LOOP;
            
            
            end SWITCH;
         --------------------------------------------------
         
         
            exit when HITS = 0;
         end loop HIT_LOOP;
      
      
      
      --  Fix up the Archaic/Medieval
         if WORDS_MODE(TRIM_OUTPUT)  then
         --  Remove those inflections if MDEV and there is other valid
--         TEXT_IO.PUT_LINE("SCANNING FOR TRIM   SL'FIRST = " & INTEGER'IMAGE(SL'FIRST) & "   SL'LAST = " & INTEGER'IMAGE(SL'LAST) );
--         for I in SL'FIRST..SL_LAST  loop
--         PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
--         end loop;            
         
         --  Check to see if we can afford to TRIM, if there will be something left over
            for I in SL'FIRST..SL_LAST  loop
 --TEXT_IO.PUT_LINE("SCANNING FOR TRIM   I = " & INTEGER'IMAGE(I) & "  INFL AGE = " & AGE_TYPE'IMAGE(SL(I).IR.AGE));
               if SL(I).D_K in GENERAL..LOCAL  then
               
                  DICT_IO.SET_INDEX(DICT_FILE(SL(I).D_K), SL(I).MNPC);
--TEXT_IO.PUT(INTEGER'IMAGE(INTEGER(SL(I).MNPC)));
                  DICT_IO.READ(DICT_FILE(SL(I).D_K), DE);
--DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;
              
                  if ((SL(I).IR.AGE = X) or else (SL(I).IR.AGE > A))  and
                     ((DE.TRAN.AGE = X) or else (DE.TRAN.AGE > A))  then
                     NOT_ONLY_ARCHAIC := TRUE;
                  end if;
                  if ((SL(I).IR.AGE = X) or else (SL(I).IR.AGE < F))  and     --  Or E????
                     ((DE.TRAN.AGE = X) or else (DE.TRAN.AGE < F))  then     --  Or E????
                     NOT_ONLY_MEDIEVAL := TRUE;
                  end if;
                  if ((SL(I).IR.FREQ = X) or else (SL(I).IR.FREQ < C))   and  --  A/X < C   --  C for inflections is uncommon  !!!!
                     ((DE.TRAN.FREQ = X) or else (DE.TRAN.FREQ < D)) then  --     --  E for DICTLINE is uncommon  !!!!
                     NOT_ONLY_UNCOMMON := TRUE;
                  end if;
--   TEXT_IO.PUT_LINE("NOT_ONLY_ARCHAIC = " & BOOLEAN'IMAGE(NOT_ONLY_ARCHAIC));
--   TEXT_IO.PUT_LINE("NOT_ONLY_MEDIEVAL = " & BOOLEAN'IMAGE(NOT_ONLY_MEDIEVAL));
--   TEXT_IO.PUT_LINE("NOT_ONLY_UNCOMMON = " & BOOLEAN'IMAGE(NOT_ONLY_UNCOMMON));
               
               
--                  if ((SL(I).IR.QUAL.POFS = N) and then (SL(I).IR.QUAL.N.CS /= VOC))  or 
--                     ((SL(I).IR.QUAL.POFS = ADJ) and then (SL(I).IR.QUAL.ADJ.CS /= VOC))  or 
--                     ((SL(I).IR.QUAL.POFS = VPAR) and then (SL(I).IR.QUAL.VPAR.CS /= VOC))  then
--                     NOT_ONLY_VOCATIVE := TRUE;
--                  end if;
--                  if (SL(I).IR.QUAL.POFS = N) and then (SL(I).IR.QUAL.N.CS /= LOC)  then
--                     NOT_ONLY_LOCATIVE := TRUE;
--                  end if;
--                  if (SL(I).IR.QUAL.POFS = ADJ) and then (SL(I).IR.QUAL.ADJ.CS /= VOC)  then
--                     NOT_ONLY_VOCATIVE := TRUE;
--                  end if;
--                  if (SL(I).IR.QUAL.POFS = ADJ) and then (SL(I).IR.QUAL.ADJ.CS /= LOC)  then
--                     NOT_ONLY_LOCATIVE := TRUE;
--                  end if;
--                  if (SL(I).IR.QUAL.POFS = VPAR) and then (SL(I).IR.QUAL.VPAR.CS /= VOC)  then
--                     NOT_ONLY_VOCATIVE := TRUE;
--                  end if;
--                  if (SL(I).IR.QUAL.POFS = VPAR) and then (SL(I).IR.QUAL.VPAR.CS /= LOC)  then
--                     NOT_ONLY_LOCATIVE := TRUE;
--                  end if;
--    TEXT_IO.PUT_LINE("NOT_ONLY_VOCATIVE = " & BOOLEAN'IMAGE(NOT_ONLY_VOCATIVE));
--    TEXT_IO.PUT_LINE("NOT_ONLY_LOCATIVE = " & BOOLEAN'IMAGE(NOT_ONLY_LOCATIVE));
               
                  if SL(I).IR.QUAL.POFS = N  and then
                  SL(I).IR.QUAL.N.DECL = (9, 8) then
                     HAS_NOUN_ABBREVIATION := TRUE;
                  --TEXT_IO.PUT_LINE("Has noun abbreviation   I = " & INTEGER'IMAGE(I));
                  --       elsif SL(I).IR.QUAL.POFS = ADJ  and then
                  --          SL(I).IR.QUAL.ADJ.DECL = (9, 8) then
                  --         HAS_ADJECTIVE_ABBREVIATION := TRUE;
                  --       elsif SL(I).IR.QUAL.POFS = V  and then
                  --          SL(I).IR.QUAL.V.CON = (9, 8) then
                  --         HAS_VERB_ABBREVIATION := TRUE;
                  end if;
               end if;
            end loop;
         
         
         --  We order and trim within a subset SL, but have to correct the big set PA also
         --  Kill not ALLOWED first, then check the remaining from the top
         --  I am assuming there is no trimming of FIXES for AGE/...
            I := SL_LAST;
            while I >= SL'FIRST  loop
               if (not ALLOWED_STEM(SL(I))   or               --  Remove not ALLOWED_STEM & null
                      (PA(I) = NULL_PARSE_RECORD))  then   
    --TEXT_IO.PUT_LINE("Not ALLOWED   SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
                  SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
                  SL_LAST := SL_LAST - 1;
                  TRIMMED := TRUE;
    --TEXT_IO.PUT_LINE("Not ALLOWED end  SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
               end if;            
               I := I - 1;
            end loop; 
         
            I := SL_LAST;
            while I >= SL'FIRST  loop
    --TEXT_IO.PUT_LINE("TRIMMING FOR TRIM   I = " & INTEGER'IMAGE(I));
               if (NOT_ONLY_ARCHAIC and WORDS_MDEV(OMIT_ARCHAIC)) and then
               SL(I).IR.AGE = A  then
                  SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
                  SL_LAST := SL_LAST - 1;
    --TEXT_IO.PUT_LINE("Archaic        SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
                  TRIMMED := TRUE;
               elsif (NOT_ONLY_MEDIEVAL and WORDS_MDEV(OMIT_MEDIEVAL)) and then
               SL(I).IR.AGE >= F  then
                  SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
                  SL_LAST := SL_LAST - 1;
    --TEXT_IO.PUT_LINE("Medieval       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
                  TRIMMED := TRUE;
               end if;
               I := I - 1;
            end loop; 
         
            I := SL_LAST;
            while I >= SL'FIRST  loop
               if (NOT_ONLY_UNCOMMON and WORDS_MDEV(OMIT_UNCOMMON)) and then
               SL(I).IR.FREQ >= C  then      --  Remember A < C
                  SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
                  SL_LAST := SL_LAST - 1;
     --TEXT_IO.PUT_LINE("Uncommon       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
                  TRIMMED := TRUE;
               end if;
               I := I - 1;
            end loop; 
         
         
         
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----Big problem.  This area has been generaing exceptions.
----At least one difficulty is that suffixes change POFS.
----So one has a N inflection (SL) but a V DE
----When the program checks for VOC, it wants a N
---- and then asks about KIND (P, N, T,...) 
---- But the DE (v) does not have those
---- The solution would be to fix ADD SUFFIX to do somethnig about passing the ADDON KIND
----  I do not want to face that now
----  It is likely that all this VOC/LOC is worthless anyway.  Maybe lower FREQ in INFLECTS
----
----  A further complication is the GANT and AO give different results (AO no exception)
----  That is probably because the program is in error and the result threrfore unspecified
----
----
            
--
--            I := SL_LAST;
--TEXT_IO.PUT_LINE("Checking VOC/LOC    SL_LAST = " & INTEGER'IMAGE(SL_LAST));
--           while I >= SL'FIRST  loop
--            --  Check for Vocative being person/name and Locative a place/area
----TEXT_IO.PUT_LINE("Looping down on I  I = " & INTEGER'IMAGE(I)); 
--               if (SL(I).IR.QUAL.POFS = N)  then 
--TEXT_IO.PUT_LINE("N found   I = " & INTEGER'IMAGE(I)); 
--PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
--                 if NOT_ONLY_VOCATIVE    and then 
--                     (SL(I).IR.QUAL.N.CS = VOC) and then 
--                     ((DEPR(SL(I)).PART.N.KIND /= N) and 
--                      (DEPR(SL(I)).PART.N.KIND /= P)) then
----TEXT_IO.PUT_LINE("N VOC not a P or N          I = " & INTEGER'IMAGE(I)); 
--                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
--                     SL_LAST := SL_LAST - 1;
--                     TRIMMED := TRUE;
--                  elsif NOT_ONLY_LOCATIVE    and then 
--                     (SL(I).IR.QUAL.N.CS = LOC) and then 
--                     ((DEPR(SL(I)).PART.N.KIND /= L) and 
--                      (DEPR(SL(I)).PART.N.KIND /= W)) then
----TEXT_IO.PUT_LINE("N LOC not a W or L           "); 
--                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
--                     SL_LAST := SL_LAST - 1;
--                     TRIMMED := TRUE;
--                  end if;
--               end if;
--               I := I - 1;
--            end loop; 
----TEXT_IO.PUT_LINE("Checked  VOC/LOC");
--         
--         
--              --  Cutting viciously here       
--            I := SL_LAST;
--            while I >= SL'FIRST  loop
--               if (SL(I).IR.QUAL.POFS = ADJ)  then 
--                  if NOT_ONLY_VOCATIVE    and then 
--                     (SL(I).IR.QUAL.ADJ.CS = VOC) then
--                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
--                     SL_LAST := SL_LAST - 1;
--                     TRIMMED := TRUE;
--                  elsif NOT_ONLY_LOCATIVE    and then 
--                     (SL(I).IR.QUAL.ADJ.CS = LOC) then
--                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
--                     SL_LAST := SL_LAST - 1;
--                     TRIMMED := TRUE;
--                  end if;
--               end if;
--               I := I - 1;
--            end loop; 
--         
--         
--         
--            I := SL_LAST;
--            while I >= SL'FIRST  loop
--               if (SL(I).IR.QUAL.POFS = VPAR)  then 
--                  if NOT_ONLY_VOCATIVE    and then 
--                     (SL(I).IR.QUAL.VPAR.CS = VOC) then
--                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
--                     SL_LAST := SL_LAST - 1;
--                     TRIMMED := TRUE;
--                  elsif NOT_ONLY_LOCATIVE    and then 
--                     (SL(I).IR.QUAL.VPAR.CS = LOC) then
--                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
--                     SL_LAST := SL_LAST - 1;
--                     TRIMMED := TRUE;
--                  end if;
--               end if;
--               I := I - 1;
--            end loop; 
--         
         
         
         --  This is really working much too hard!
         --  just to kill Roman numeral for three single letters
         --  Also strange in that code depends on dictionary knowledge
            I := SL_LAST;
            while I >= SL'FIRST  loop
               if HAS_NOUN_ABBREVIATION    and then 
                  (ALL_CAPS and FOLLOWED_BY_PERIOD)  then
                  if (SL(I).IR.QUAL.POFS /= N) or
                     (   (SL(I).IR.QUAL /= (N,  ((9, 8), X, X, M)))  and  
                            ( TRIM(SL(I).STEM)'LENGTH = 1  and then
                                 (SL(I).STEM(1) = 'A'  or
                                  SL(I).STEM(1) = 'C'  or
                                  SL(I).STEM(1) = 'D'  or
                                  --SL(I).STEM(1) = 'K'  or      --  No problem here
                                  SL(I).STEM(1) = 'L'  or
                                  SL(I).STEM(1) = 'M'            --  or
                                 --SL(I).STEM(1) = 'N'  or
                                 --SL(I).STEM(1) = 'P'  or
                                 --SL(I).STEM(1) = 'Q'  or
                                 --SL(I).STEM(1) = 'T'
                                 ) )    ) then
                     SL(I..SL_LAST-1) := SL(I+1..SL_LAST);
                     SL_LAST := SL_LAST - 1;
                     TRIMMED := TRUE;
                  end if;
               end if;
               I := I - 1;
            end loop; 
         
         
         
         end if;   --  On TRIM
      
         DIFF_J := SL_LAST_INITIAL - SL_LAST;
      
      end ORDER_PARSE_ARRAY;
   
   
   
   
    begin                               --  LIST_SWEEP
                       
                       
                       
-- DICT_IO.READ(DICT_FILE(GENERAL), DE, 31585);
-- DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.PUT_LINE("#########");                      
                       
   
      if PA'LENGTH = 0              then
         return;
      end if;
   
   
--   TEXT_IO.PUT_LINE("PA on entering LIST_SWEEP     PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--   for I in 1..PA_LAST  loop
--   PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--   end loop;
   
   
   
      RESET_PRONOUN_KIND:
      declare
         DE : DICTIONARY_ENTRY;
      begin
         for I in 1..PA_LAST  loop
            if PA(I).D_K = GENERAL  then
               DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
               DICT_IO.READ(DICT_FILE(PA(I).D_K), DE);
               if DE.PART.POFS = PRON  and then
               DE.PART.PRON.DECL.WHICH =1  then
                  PA(I).IR.QUAL.PRON.DECL.VAR := PRONOUN_KIND_TYPE'POS(DE.PART.PRON.KIND);
               --elsif DE.PART.POFS = PACK  and then
               -- DE.PART.PACK.DECL.WHICH =1  then
               -- PA(I).IR.QUAL.PACK.DECL.VAR := PRONOUN_KIND_TYPE'POS(DE.KIND.PRON_KIND);
               end if;
            end if;
         end loop;
      end RESET_PRONOUN_KIND;
   
   ---------------------------------------------------
   
   
   
   --  NEED TO REMOVE DISALLOWED BEFORE DOING ANYTHING - BUT WITHOUT REORDERING
   
   
   --  The problem I seem to have to face first, if not the first problem,
   --  is the situation in which there are several sets of identical IRs with different MNPC
   --  These may be variants with some other stem (e.g., K=3) not affecting the (K=1) word
   --  Or they might be identical forms with different meanings (| additional meanings)
   --  I need to group such common inflections - and pass this on somehow
   
   
--   TEXT_IO.PUT_LINE("PA before SWEEPING in LIST_SWEEP     PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--   for I in 1..PA_LAST  loop
--   PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--   end loop;
   
   
      SWEEPING:
      --  To remove disallowed stems/inflections and resulting dangling fixes
      declare
         FIX_ON : BOOLEAN := FALSE;
         PW_ON  : BOOLEAN := FALSE;
         P_FIRST : INTEGER := 1;
         P_LAST  : INTEGER := 0;   
         subtype XONS is PART_OF_SPEECH_TYPE range TACKON..SUFFIX;
      
      
      begin
      --
--      TEXT_IO.NEW_LINE;
--      TEXT_IO.PUT_LINE("SWEEPING    ======================================");
--      TEXT_IO.NEW_LINE;
--TEXT_IO.PUT("{");
         J := PA_LAST;
      
         while J >= 1  loop        --  Sweep backwards over PA
         
         
         
         --           if (not ALLOWED_STEM(PA(J))   or               --  Remove not ALLOWED_STEM & null
         --               (PA(J) = NULL_PARSE_RECORD))  then         --  and close ranks
         -- TEXT_IO.PUT_LINE("Removing dis ALLOWED STEM  J = " & INTEGER'IMAGE(J));
         --               PA(J..PA_LAST-1) := PA(J+1..PA_LAST);     --  null if J = PA_LAST
         --              PA_LAST := PA_LAST - 1;
         --              P_LAST := P_LAST - 1;
         --              TRIMMED := TRUE;
         
         
            if ((PA(J).D_K in ADDONS..YYY) or (PA(J).IR.QUAL.POFS in XONS))   and then
               (PW_ON)     then               --  first FIX/TRICK after regular
               FIX_ON := TRUE;
               PW_ON  := FALSE;
               P_FIRST := J + 1;
              --P_LAST := J + 1;
            --TEXT_IO.PUT_LINE("SWEEP  FIX/TRICK  J = " & INTEGER'IMAGE(J) & "  P_FIRST = " & INTEGER'IMAGE(P_FIRST) & 
            --"  P_LAST = " & INTEGER'IMAGE(P_LAST));
               JJ := J;
               while PA(JJ+1).IR.QUAL.POFS = PA(JJ).IR.QUAL.POFS  loop
                  P_LAST := JJ + 1;
               end loop;
            
            
             ----Order internal to this set of inflections
--  TEXT_IO.PUT_LINE("SWEEP  INTERNAL  J = " & INTEGER'IMAGE(J) & "  P_FIRST = " & INTEGER'IMAGE(P_FIRST) & 
--  "  P_LAST = " & INTEGER'IMAGE(P_LAST) & "  DIFF_J = " & INTEGER'IMAGE(DIFF_J) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
               ORDER_PARSE_ARRAY(PA(P_FIRST..P_LAST), DIFF_J);      
              --PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
               PA(P_LAST-DIFF_J+1..PA_LAST-DIFF_J) := PA(P_LAST+1..PA_LAST);
               PA_LAST := PA_LAST - DIFF_J;
-- TEXT_IO.PUT_LINE("SWEEP  INTERNAL end  J = " & INTEGER'IMAGE(J) & "  P_FIRST = " & INTEGER'IMAGE(P_FIRST) & 
-- "  P_LAST = " & INTEGER'IMAGE(P_LAST) & "  DIFF_J = " & INTEGER'IMAGE(DIFF_J) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
               P_FIRST := 1;
               P_LAST  := 0;
            
            
            elsif ((PA(J).D_K in ADDONS..YYY) or (PA(J).IR.QUAL.POFS in XONS))  and then
               (FIX_ON)     then               --  another FIX
--TEXT_IO.PUT_LINE("SWEEP  Another FIX/TRICK  J = " & INTEGER'IMAGE(J));
               null;
            
            
            elsif ((PA(J).D_K in ADDONS..YYY)  or
                      (PA(J).IR.QUAL.POFS = X))  and then  --  Kills TRICKS stuff
               (not PW_ON)     then
            --TEXT_IO.PUT_LINE("Killing Tricks stuff  J = " & INTEGER'IMAGE(J));
               PA(P_LAST-DIFF_J+1..PA_LAST-DIFF_J) := PA(P_LAST+1..PA_LAST);
               PA_LAST := PA_LAST - DIFF_J;
            --PA_LAST := PA_LAST - 1;
               P_LAST := P_LAST - 1;
            
            
            else
--TEXT_IO.PUT_LINE("SWEEP  else  J = " & INTEGER'IMAGE(J) & "  P_LAST = " & INTEGER'IMAGE(P_LAST));
--for I in 1..PA_LAST  loop
--PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--end loop;            
               PW_ON := TRUE;
               FIX_ON := FALSE;
               if P_LAST <= 0  then
                  P_LAST := J;
               end if;
               if J = 1  then
 --TEXT_IO.PUT_LINE("SWEEP  J = 1     P_LAST = " & INTEGER'IMAGE(P_LAST));
                  ORDER_PARSE_ARRAY(PA(1..P_LAST), DIFF_J); 
                  PA(P_LAST-DIFF_J+1..PA_LAST-DIFF_J) := PA(P_LAST+1..PA_LAST);
                  PA_LAST := PA_LAST - DIFF_J;
 --TEXT_IO.PUT_LINE("SWEEP  J = 1 end    PA_LAST = " & INTEGER'IMAGE(PA_LAST) & "  DIFF_J = " & INTEGER'IMAGE(DIFF_J));
               end if;
            
            
            end if;                                      --  check PART
         
         
            J := J - 1;
         
         end loop;                          --  loop sweep over PA
      
      end SWEEPING;
   
--   TEXT_IO.PUT_LINE("PA after SWEEPING  in LIST_STEMS - before COMPRESS_LOOP   PA_LAST = " 
--   & INTEGER'IMAGE(PA_LAST));
--   for I in 1..PA_LAST  loop
--   PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--   end loop;
   
      OPR := PA(1);
   --  Last chance to weed out duplicates
      J := 2;
   COMPRESS_LOOP:
      loop
         exit when J > PA_LAST;
         PR := PA(J);
         if PR /= OPR  then
            SUPRESS_KEY_CHECK:
            declare
               function "<=" (A, B : PARSE_RECORD) return BOOLEAN is
               begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                  if A.IR.QUAL = B.IR.QUAL  and
                  A.MNPC    = B.MNPC     then
                     return TRUE;
                  else
                     return FALSE;
                  end if;
               end "<=";
               function "<" (A, B : PARSE_RECORD) return BOOLEAN is
               begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                  if A.IR.QUAL = B.IR.QUAL  and
                  A.MNPC   /= B.MNPC     then
                     return TRUE;
                  else
                     return FALSE;
                  end if;
               end "<";
            begin
               if ((PR.D_K /= XXX) and (PR.D_K /= YYY) and  (PR.D_K /= PPP)) then
                  if PR <= OPR  then       --  Get rid of duplicates, if ORDER is OK
                     PA(J.. PA_LAST-1) := PA(J+1..PA_LAST);  --  Shift PA down 1
                     PA_LAST := PA_LAST - 1;        --  because found key duplicate
                  end if;
               else
                  J := J + 1;
               end if;
            end SUPRESS_KEY_CHECK;
         else
            J := J + 1;
         
         end if;
         OPR := PR;
      end loop COMPRESS_LOOP;
   
   
   
      for I in 1..PA_LAST  loop
      --  Set to 0 the VAR for N            --  DON'T
      --  if PA(I).IR.QUAL.POFS = N  then
      --    PA(I).IR.QUAL.N.DECL.VAR := 0;
      --  end if;
      --  Destroy the artificial VAR for PRON 1 X
         if PA(I).IR.QUAL.POFS = PRON  and then
         PA(I).IR.QUAL.PRON.DECL.WHICH =1  then
            PA(I).IR.QUAL.PRON.DECL.VAR := 0;
         end if;
         if PA(I).IR.QUAL.POFS = V   then
            if PA(I).IR.QUAL.V.CON = (3, 4)  then
            --  Fix V 3 4 to be 4th conjugation
               PA(I).IR.QUAL.V.CON := (4, 1);
            --    else
            --    --  Set to 0 other VAR for V
            --      PA(I).IR.QUAL.V.CON.VAR := 0;
            end if;
         end if;
      end loop;
   
   
   
--      TEXT_IO.PUT_LINE("PA after COMPRESS  almost leaving LIST_STEMS    PA_LAST = "  & INTEGER'IMAGE(PA_LAST));
--      for I in 1..PA_LAST  loop
--         PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--      end loop;
   
--TEXT_IO.PUT("}");
   
   end LIST_SWEEP;
