   with TEXT_IO;
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with CONFIG;  use CONFIG;
   with UNIQUES_PACKAGE; use UNIQUES_PACKAGE;
   with ADDONS_PACKAGE; use ADDONS_PACKAGE;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with PREFACE;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with LINE_STUFF; use LINE_STUFF;
   with ENGLISH_SUPPORT_PACKAGE; use ENGLISH_SUPPORT_PACKAGE;
 package body WORD_PACKAGE is

      INFLECTIONS_SECTIONS_FILE : LEL_SECTION_IO.FILE_TYPE;

       procedure PAUSE(OUTPUT : TEXT_IO.FILE_TYPE) is
            use CONFIG;
            PAUSE_LINE : STRING(1..300);
            PAUSE_LAST : INTEGER := 0;
         begin
         if WORDS_MDEV(PAUSE_IN_SCREEN_OUTPUT)  then
          if METHOD = INTERACTIVE  then
           if TEXT_IO.NAME(OUTPUT) =
                  TEXT_IO.NAME(TEXT_IO.STANDARD_OUTPUT)  then
                     TEXT_IO.PUT_LINE(TEXT_IO.STANDARD_OUTPUT,
                 "                          MORE - hit RETURN/ENTER to continue");
                     TEXT_IO.GET_LINE(TEXT_IO.STANDARD_INPUT, PAUSE_LINE, PAUSE_LAST);
                  end if;
               elsif METHOD = COMMAND_LINE_INPUT  then
                  TEXT_IO.PUT_LINE(TEXT_IO.STANDARD_OUTPUT,
                                   "                          MORE - hit RETURN/ENTER to continue");
                  TEXT_IO.GET_LINE(TEXT_IO.STANDARD_INPUT, PAUSE_LINE, PAUSE_LAST);
               elsif METHOD = COMMAND_LINE_FILES  then
                  null;                       --  Do not PAUSE
               end if;
            end if;
          exception
           when others  =>
             TEXT_IO.PUT_LINE("Unexpected exception in PAUSE");
         end PAUSE;

      function MIN(A, B : INTEGER) return INTEGER is
      begin
         if A <= B  then
            return A; end if;
         return B;
      end MIN;


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
         for I in 1..S'LENGTH  loop   --  Not TRIMed, so same length
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


      procedure RUN_UNIQUES(S : in STRING; UNIQUE_FOUND : out BOOLEAN;
                            PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is
         SL : constant STRING        --  BAD NAME!!!!!!!!!!!!!!!!!!
         := LOWER_CASE(TRIM(S));
         ST : constant STEM_TYPE := HEAD(SL, MAX_STEM_SIZE);
         UNQL : UNIQUE_LIST;   --  Unique list for a letter
      begin
         UNIQUE_FOUND := FALSE;
         if SL(SL'FIRST) = 'v'  then
            UNQL := UNQ('u');   --  Unique list for a letter
         elsif SL(SL'FIRST) = 'j'  then
            UNQL := UNQ('i');   --  Unique list for a letter
         else
            UNQL := UNQ(SL(SL'FIRST));   --  Unique list for a letter
         end if;

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE("Called UNIQUES with =>" & SL & "|");

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE("UNQL ");

         while UNQL /= null  loop
         --  If there is a match, add to PA
         --TEXT_IO.PUT_LINE("UNIQUE =>" & UNQL.PR.STEM);
         --if ST = LOWER_CASE(UNQL.PR.STEM)  then
            if EQU(ST, LOWER_CASE(UNQL.STEM)) then
               PA_LAST := PA_LAST + 1;
               PA(PA_LAST) := (UNQL.STEM, 
                              (UNQL.QUAL, 
                               0, 
                               NULL_ENDING_RECORD,
                               X,
                               X), 
                               UNIQUE,
                               UNQL.MNPC);

            --TEXT_IO.PUT_LINE("UNIQUE    HIT     *********" & INTEGER'IMAGE(PA_LAST));
               UNIQUE_FOUND := TRUE;
            end if;
            UNQL := UNQL.SUCC;
         end loop;

       end RUN_UNIQUES;


      procedure RUN_INFLECTIONS(S : in STRING; SL : in out SAL;
                                RESTRICTION : DICT_RESTRICTION := REGULAR) is
      --  Trys all possible inflections against the input word in S
      --  and constructs a STEM_LIST of those that survive SL
         use LEL_SECTION_IO;
         use INFLECTION_RECORD_IO;
         WORD : constant STRING := LOWER_CASE(TRIM(S));
         LAST_OF_WORD : constant CHARACTER := WORD(WORD'LAST);
         LENGTH_OF_WORD   : constant INTEGER := WORD'LENGTH;
         STEM_LENGTH  : INTEGER := 0;
         PR   : PARSE_RECORD;
         M : INTEGER := 1;

      begin
      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE("Called RUN_INFLECTIONS with =>" & WORD & "|");
         if WORD'LENGTH = 0  then
            SL(M) := NULL_PARSE_RECORD;
            return;
         end if;

         SA := NOT_A_STEM_ARRAY;

      --  Add all of these to list of possible ending records
      --  since the blank ending agrees with everything
      --  PACK/PRON have no blank endings
         if ((RESTRICTION /= PACK_ONLY) and (RESTRICTION /= QU_PRON_ONLY))  and then
            (WORD'LENGTH <= MAX_STEM_SIZE)  then
            for I in BELF(0, ' ')..BELL(0, ' ')  loop
               PR := (WORD & NULL_STEM_TYPE(LENGTH_OF_WORD+1..STEM_TYPE'LENGTH),
                        BEL(I), DEFAULT_DICTIONARY_KIND, NULL_MNPC);
               SL(M) := PR;
               M := M + 1;

            end loop;
            SA(LENGTH_OF_WORD) := PR.STEM;  --  Is always a possibility (null ending)

         end if;

      --  Here we read in the INFLECTIONS_SECTION that is applicable
         if RESTRICTION = REGULAR  then
            case LAST_OF_WORD is
               when 'a' | 'c' | 'd' | 'e' | 'i'  =>
                  READ(INFLECTIONS_SECTIONS_FILE, LEL, 1);
               when 'm' | 'n' | 'o' | 'r'  =>
                  READ(INFLECTIONS_SECTIONS_FILE, LEL, 2);
               when 's'  =>
                  READ(INFLECTIONS_SECTIONS_FILE, LEL, 3);
               when 't' | 'u'  =>
                  READ(INFLECTIONS_SECTIONS_FILE, LEL, 4);
               when others  =>
               --PUT_LINE("Only blank inflections are found");
                  return;
            end case;
         elsif RESTRICTION = PACK_ONLY  or RESTRICTION = QU_PRON_ONLY  then
            READ(INFLECTIONS_SECTIONS_FILE, LEL, 4);
         end if;


      --  Now do the non-blank endings      --  Only go to LENGTH_OF_WORD
         for Z in reverse 1..MIN(MAX_ENDING_SIZE, LENGTH_OF_WORD)  loop

         --  Check if Z agrees with a PDL SIZE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         --  Maybe make PDL on size, if it has to be a list, or order by size if array
            if LELL(Z, LAST_OF_WORD) > 0  then   --  Any likely inflections at all

               for I in LELF(Z, LAST_OF_WORD)..LELL(Z, LAST_OF_WORD) loop
                if EQU(LOWER_CASE(LEL(I).ENDING.SUF(1..Z)),
                         LOWER_CASE(WORD(WORD'LAST-Z+1..WORD'LAST)))  then
                  --  Add to list of possible ending records
                  --STEM_LENGTH := WORD'LENGTH - LEL(I).ENDING.SIZE; 
                     STEM_LENGTH := WORD'LENGTH - Z;
                  --PUT(STEM_LENGTH);
                  --TEXT_IO.PUT_LINE("#######################################################");

                     if STEM_LENGTH <= MAX_STEM_SIZE  then  --  Reject too long words
                     --  Check if LEL IR agrees with PDL IR  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        PR := (WORD(WORD'FIRST..STEM_LENGTH) &
                                 NULL_STEM_TYPE(STEM_LENGTH+1..MAX_STEM_SIZE),
                                 LEL(I), DEFAULT_DICTIONARY_KIND, NULL_MNPC);
                        SL(M) := PR;
                        M := M + 1;

                        SA(STEM_LENGTH) := PR.STEM;    --  Gets set dozens of times
                     --  Could order the endings by length (suffix sort) so length changes slowly

                     --PUT_LINE("LENGTH = " & INTEGER'IMAGE(STEM_LENGTH) 
                     --& "   SA =>" & PR.STEM & "|");

                     end if;
                  end if;
               end loop;

            end if;
         end loop;
      --TEXT_IO.PUT_LINE("RUN_INF FOUND STEMS = " & INTEGER'IMAGE(M-1));
      --exception
      --when others =>
      --TEXT_IO.PUT_LINE("exception RUN_INF FOUND STEMS = " & INTEGER'IMAGE(M-1));
      --raise;
      end RUN_INFLECTIONS;


      procedure TRY_TO_LOAD_DICTIONARY(D_K : DICTIONARY_KIND) is
      begin
      --    PUT_LINE("Trying to load " & DICTIONARY_KIND'IMAGE(D_K) &
      --             " dictionary from STEMFILE ");

         STEM_IO.OPEN(STEM_FILE(D_K), STEM_IO.IN_FILE,
                      ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                                              DICTIONARY_KIND'IMAGE(D_K)));
         DICT_IO.OPEN(DICT_FILE(D_K), DICT_IO.IN_FILE,
                      ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                              DICTIONARY_KIND'IMAGE(D_K)));
         LOAD_INDICES_FROM_INDX_FILE(ADD_FILE_NAME_EXTENSION(INDX_FILE_NAME,
                                                             DICTIONARY_KIND'IMAGE(D_K)), D_K);
         DICTIONARY_AVAILABLE(D_K) := TRUE;
      --    PUT_LINE("Successfully loaded " & DICTIONARY_KIND'IMAGE(D_K) &
      --             " dictionary from STEMFILE ");

         exception
            when others  =>
            --PUT_LINE("Failed to load " & DICTIONARY_KIND'IMAGE(D_K) &
            --             " dictionary from STEMFILE ");
               DICTIONARY_AVAILABLE(D_K) := FALSE;
      end TRY_TO_LOAD_DICTIONARY;



      procedure DICTIONARY_SEARCH(SSA : STEM_ARRAY_TYPE;
                                  PREFIX : PREFIX_ITEM;
                                  SUFFIX : SUFFIX_ITEM;
                                  D_K : DICTIONARY_KIND;
                                  RESTRICTION : DICT_RESTRICTION := REGULAR) is
                     --  Prepares a PDL list of possible dictionary hits   
      --  Search a dictionary (D_K) looking for all stems that match
      --  any of the stems that are physically possible with Latin inflections 
         use STEM_IO;

      --type NAT_32 is range 0..2**31-1;   --###############
         J, J1, J2, JJ : STEM_IO.COUNT := 0;

         INDEX_ON : constant STRING := SSA(SSA'LAST);
         INDEX_FIRST, INDEX_LAST : STEM_IO.COUNT := 0;
         DS : DICTIONARY_STEM;
         FIRST_TRY, SECOND_TRY : BOOLEAN := TRUE;


         function FIRST_TWO(W : STRING) return STRING is
         --  'v' could be represented by 'u', like the new Oxford Latin Dictionary
         --  Fixes the first two letters of a word/stem which can be done right
            S : constant STRING := LOWER_CASE(W);
            SS : STRING(W'RANGE) := W;
            
            function UI(C : CHARACTER) return CHARACTER  is
            begin
               if (C = 'v')   then
                  return 'u';
               elsif (C = 'V')  then
                  return 'U';
               elsif (C = 'j')  then
                  return 'i';
               elsif (C = 'J')  then
                  return 'I';
               else
                  return C;
               end if;
            end UI;

         begin

            if S'LENGTH = 1  then
               SS(S'FIRST) := UI(W(S'FIRST));
            else
               SS(S'FIRST)   := UI(W(S'FIRST));
               SS(S'FIRST+1) := UI(W(S'FIRST+1));
            end if;

            return SS;
         end FIRST_TWO;


         procedure LOAD_PDL is
         begin
            case RESTRICTION is
               when REGULAR    =>
                  if not (DS.PART.POFS = PACK  or
                             (DS.PART.POFS = PRON  and then
                                 (DS.PART.PRON.DECL.WHICH = 1)))  then
                     PDL_INDEX := PDL_INDEX + 1;
                     PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
                  end if;

               when PACK_ONLY  =>
                  if DS.PART.POFS = PACK  then
                     PDL_INDEX := PDL_INDEX + 1;
                     PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
                  end if;

               when QU_PRON_ONLY  =>
                  if DS.PART.POFS = PRON  and then
                     (DS.PART.PRON.DECL.WHICH = 1)  then
                     PDL_INDEX := PDL_INDEX + 1;
                     PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
                  end if;

               when others =>
                  PDL_INDEX := PDL_INDEX + 1;
                  PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
            end case;

         end LOAD_PDL;


      begin
      --  Now go through the dictionary list DL for the first letters
      --  and make a reduced dictionary list PDL
      --TEXT_IO.PUT_LINE("Entering DICTIONARY_SEARCH PDL_INDEX = " & INTEGER'IMAGE(PDL_INDEX));


         if D_K = LOCAL  then
            INDEX_FIRST := FIRST_INDEX((FIRST_TWO(INDEX_ON)(1), 'a'), D_K);
            INDEX_LAST  := LAST_INDEX((FIRST_TWO(INDEX_ON)(1), 'a'), D_K);
         else
            INDEX_FIRST := FIRST_INDEX(FIRST_TWO(INDEX_ON), D_K);
            INDEX_LAST  := LAST_INDEX(FIRST_TWO(INDEX_ON), D_K);
         end if;
      
         if INDEX_FIRST > 0  and then INDEX_FIRST <= INDEX_LAST then


            J1 := STEM_IO.COUNT(INDEX_FIRST);    --######################
            J2 := STEM_IO.COUNT(INDEX_LAST);

         STEM_ARRAY_LOOP:
            for K in SSA'RANGE  loop
               if TRIM(SSA(K))'LENGTH > 1  then
               --  This may be checking for 0 and 1 letter SSAs which are done elsewhere
               --TEXT_IO.PUT(INTEGER'IMAGE(K) & "  SSA(K) =>" );
               --TEXT_IO.PUT_LINE(SSA(K));

                  if D_K = LOCAL  then    --  Special processing for unordered DICT.LOC
                     for J in J1..J2  loop       --  Sweep exaustively through the scope
                        SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(J));
                        READ(STEM_FILE(D_K), DS);

                        if EQU(LOWER_CASE(DS.STEM), SSA(K))  then
                        --TEXT_IO.PUT_LINE("HIT LOC =   " & DS.STEM & " - " & SSA(K));
                           LOAD_PDL;
                        end if;
                     end loop;

                  else                     --  Regular dictionaries

                     FIRST_TRY := TRUE;

                     SECOND_TRY := TRUE;

                     J := (J1 + J2) / 2;

                  BINARY_SEARCH:
                     loop

                        if (J1 = J2-1) or (J1 = J2) then
                           if FIRST_TRY  then
                           --TEXT_IO.PUT_LINE("FIRST_TRY");
                              J := J1;
                              FIRST_TRY := FALSE;
                           elsif SECOND_TRY  then
                           --TEXT_IO.PUT_LINE("SECOND_TRY");
                              J := J2;
                              SECOND_TRY := FALSE;
                           else
                           --TEXT_IO.PUT_LINE("THIRD_TRY   exit BINARY_SEARCH");
                              JJ := J;
                              exit BINARY_SEARCH;
                           end if;
                        end if;

                        SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(J));
                        READ(STEM_FILE(D_K), DS);
                     
                        if  LTU(LOWER_CASE(DS.STEM), SSA(K))  then
                           J1 := J;
                           J := (J1 + J2) / 2;
                        elsif  GTU(LOWER_CASE(DS.STEM), SSA(K))  then
                           J2 := J;
                           J := (J1 + J2) / 2;
                        else
                           for I in reverse J1..J  loop
                              SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(I));
                              READ(STEM_FILE(D_K), DS);
                           
                              if EQU(LOWER_CASE(DS.STEM), SSA(K))  then
                                 JJ := I;
--TEXT_IO.PUT_LINE("PDL STEM   " & DS.STEM & "  " & INTEGER'IMAGE(INTEGER(DS.MNPC)));

                                 LOAD_PDL;

                              else
                                 exit;
                              end if;
                           end loop;

                           for I in J+1..J2  loop
                              SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(I));
                              READ(STEM_FILE(D_K), DS);
                           
                              if EQU(LOWER_CASE(DS.STEM), SSA(K))  then
                                JJ := I;
--TEXT_IO.PUT_LINE("PDL STEM   " & DS.STEM & "  " & INTEGER'IMAGE(INTEGER(DS.MNPC)));

                                 LOAD_PDL;

                              else
                                 exit BINARY_SEARCH;
                              end if;
                           end loop;
                           exit BINARY_SEARCH;

                        end if;
                     end loop BINARY_SEARCH;
                     J1 := JJ;
                     J2 := STEM_IO.COUNT(INDEX_LAST);

                  end if;               --  On LOCAL check
               end if;               --  On LENGTH > 1 
            end loop STEM_ARRAY_LOOP;
            end if;
--TEXT_IO.PUT_LINE("Leaving DICTIONARY_SEARCH PDL_INDEX = " & INTEGER'IMAGE(PDL_INDEX));
      --    exception
      --      when others  =>
      --TEXT_IO.PUT_LINE("exception DICTIONARY_SEARCH PDL_INDEX = " & INTEGER'IMAGE(PDL_INDEX));
      --        raise;       
      end DICTIONARY_SEARCH;





      procedure SEARCH_DICTIONARIES(SSA : in STEM_ARRAY_TYPE;
                                    PREFIX : PREFIX_ITEM; SUFFIX : SUFFIX_ITEM;
                                    RESTRICTION : DICT_RESTRICTION := REGULAR) is
         use STEM_IO;
         FC : CHARACTER := ' ';
        begin
      --PUT_LINE("Entering SEARCH_DICTIONARIES"); 
         PDL := (others => NULL_PRUNED_DICTIONARY_ITEM);
         PDL_INDEX := 0;
      --PUT_LINE("Search for blank stems"); 
      --  BDL is always used, so it is loaded initially and not called from disk
      --  Check all stems of the dictionary entry against the reduced stems

      --  Determine if there is a pure blank "  " stem 
         if LEN(SSA(SSA'FIRST)) = 0    then   --  a size would help?
         --PUT("HIT on blank stem   I = ");PUT('1');
         --PUT("  STEM = ");PUT_LINE(BDL(1).STEM);
         --PDL := new PRUNED_DICTIONARY_ITEM'(BDL(1), GENERAL, PDL);
            PDL_INDEX := PDL_INDEX + 1;
            PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(BDL(1), GENERAL);
         end if;
      --  Now there is only one blank stem (2 of to_be), but need not always be so


      --  Determine if there is a blank stem  (SC = ' ')
      --  Prepare for the posibility that one stem is short but there are others
         FC := ' ';
         if SSA(SSA'FIRST)(1) = ' ' then
            if SSA'LENGTH > 1  and then SSA(SSA'FIRST+1)(2) = ' '  then
               FC := SSA(SSA'FIRST+1)(1);
            end if;
         elsif SSA(SSA'FIRST)(2) = ' '  then
            FC := SSA(SSA'FIRST)(1);
         end if;

      --  If there is a single letter stem  (FC /= ' ') then
         if FC /= ' '  then
            for I in 2..BDL_LAST  loop
            --  Check all stems of the dictionary entry against the reduced stems
            --if LOWER_CASE(BDL(I).STEM(1)) = FC  then
               if EQU(LOWER_CASE(BDL(I).STEM(1)),  FC)  then
               --PUT("HIT on 1 letter stem   I = ");PUT(I);PUT("  STEM = ");PUT_LINE(BDL(I).STEM);
                  PDL_INDEX := PDL_INDEX + 1;
                  PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(BDL(I), GENERAL);
               --  D_K set to GENERAL, but should not SPE have a chance? !!!!!!!!!
               end if;
            end loop;
         end if;

         if SSA'LENGTH = 0  then
         --        PUT_LINE("Empty stem array, don't bother searching");
            return;
         --      elsif LEN(SSA(SSA'LAST)) <= 1  then
         --        PUT_LINE("No two letter stems, have done searching");
         --      else
         --        PUT_LINE("Searching Dictionaries");
         end if;

         for D_K in DICTIONARY_KIND  loop
            if DICTIONARY_AVAILABLE(D_K)  then
               if not IS_OPEN(STEM_FILE(D_K))  then
                  OPEN(STEM_FILE(D_K), STEM_IO.IN_FILE,
                        ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                                                DICTIONARY_KIND'IMAGE(D_K)));
               end if;
               DICTIONARY_SEARCH(SSA, PREFIX, SUFFIX, D_K, RESTRICTION);
               CLOSE(STEM_FILE(D_K));  --??????
            end if;
         end loop;

      --TEXT_IO.PUT_LINE("Leaving SEARCH_DICTIONARY PDL_INDEX = " & INTEGER'IMAGE(PDL_INDEX));

                                 end SEARCH_DICTIONARIES;
                                 
                                 
 procedure CHANGE_LANGUAGE(C : CHARACTER) is
 begin  if UPPER_CASE(C) = 'L'  then
    LANGUAGE := LATIN_TO_ENGLISH;
    PREFACE.PUT_LINE("Language changed to " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
  elsif UPPER_CASE(C) = 'E'  then  
    if ENGLISH_DICTIONARY_AVAILABLE(GENERAL)  then
      LANGUAGE:= ENGLISH_TO_LATIN;
      PREFACE.PUT_LINE("Language changed to " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
      PREFACE.PUT_LINE("Input a single English word (+ part of speech - N, ADJ, V, PREP, ...)");
    else
      PREFACE.PUT_LINE("No English dictionary available");
    end if;
  else
    PREFACE.PUT_LINE("Bad LANGAUGE input - no change, remains " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
  end if;
exception 
  when others  =>
    PREFACE.PUT_LINE("Bad LANGAUGE input - no change, remains " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
end CHANGE_LANGUAGE;
    
  


      procedure WORD(RAW_WORD : in STRING;
                     PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is

         INPUT_WORD : constant STRING := LOWER_CASE(RAW_WORD);
         PA_SAVE : INTEGER := PA_LAST;

         UNIQUE_FOUND : BOOLEAN := FALSE;
         
         SS, SSS : SAL := (others => NULL_PARSE_RECORD);


         procedure ORDER_STEMS(SX : in out SAL) is
            use INFLECTION_RECORD_IO;
            use DICT_IO;
            HITS : INTEGER := 0;
            SL : SAL := SX;
            SL_LAST : INTEGER := 0;
            SM : PARSE_RECORD;
         begin
            if SX(1) = NULL_PARSE_RECORD  then
               return; end if;
         --PUT_LINE("ORDERing_STEMS");

            for I in SL'RANGE  loop
               exit when SL(I) = NULL_PARSE_RECORD;
               SL_LAST := SL_LAST + 1;
            end loop;
         --PUT_LINE("In ORDER  SL_LAST = " & INTEGER'IMAGE(SL_LAST));

         --  Bubble sort since this list should usually be very small (1-5)
         HIT_LOOP:
            loop
               HITS := 0;

               SWITCH:
               begin
               --  Need to remove duplicates in ARRAY_STEMS
               --  This sort is very sloppy
               --  One problem is that it can mix up some of the order of PREFIX, XXX, LOC
               --  I ought to do this for every set of results from different approaches
               --  not just in one fell swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               INNER_LOOP:
                  for I in 1..SL_LAST-1  loop
                     if SL(I+1) /= NULL_PARSE_RECORD  then
                        if (SL(I+1).MNPC < SL(I).MNPC)  or else
                           (SL(I+1).MNPC = SL(I).MNPC   and then
                            SL(I+1).IR.ENDING.SIZE < SL(I).IR.ENDING.SIZE)  or else
                           (SL(I+1).MNPC = SL(I).MNPC   and then
                            SL(I+1).IR.ENDING.SIZE = SL(I).IR.ENDING.SIZE  and then
                            SL(I+1).IR.QUAL < SL(I).IR.QUAL)  or else
                           (SL(I+1).MNPC = SL(I).MNPC   and then
                            SL(I+1).IR.ENDING.SIZE = SL(I).IR.ENDING.SIZE  and then
                            SL(I+1).IR.QUAL = SL(I).IR.QUAL   and then
                            SL(I+1).D_K  < SL(I).D_K)   then
                        --PUT(SL(I+1).IR.QUAL); PUT("   <    "); PUT(SL(I).IR.QUAL); NEW_LINE;
                           SM := SL(I);
                           SL(I) := SL(I+1);                                                                            SL(I+1) := SM;
                           SL(I+1) := SM;
                           HITS := HITS + 1;
                        --else
                        --PUT(SL(I+1).IR.QUAL); PUT("   >=   "); PUT(SL(I).IR.QUAL); NEW_LINE;
                        end if;
                     else
                        exit INNER_LOOP;
                     end if;
                  end loop INNER_LOOP;

               end SWITCH;
            --NEW_LINE;
            --PUT_LINE("In ORDER  HITS    = " & INTEGER'IMAGE(HITS   ));
            --NEW_LINE;
            --for I in 1..SL_LAST  loop
            --PUT(SL(I)); NEW_LINE;
            --end loop;
            --PUT_LINE("--------------------------------------------------------");

               exit when HITS = 0;
            end loop HIT_LOOP;
            SX := SL;
         end ORDER_STEMS;


         procedure ARRAY_STEMS(SX : in SAL;
                               PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is
            SL : SAL := SX;
            OPR : PARSE_RECORD := NULL_PARSE_RECORD;
         begin

            if SL(1) = NULL_PARSE_RECORD  then
               return;
            else

               OPR := NULL_PARSE_RECORD;
               for I in SL'RANGE  loop
                  if SL(I) /= NULL_PARSE_RECORD  then
                  --PUT('*'); PUT(SL(I)); NEW_LINE;

                     SUPRESS_KEY_CHECK:
                     declare

                        function "<=" (A, B : PARSE_RECORD) return BOOLEAN is
                           use DICT_IO;
                        begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                           if A.IR.QUAL = B.IR.QUAL        and then
                           A.MNPC = B.MNPC  then
                              return TRUE;
                           else
                              return FALSE;
                           end if;
                        end "<=";

                     begin
                        if SL(I) <= OPR  then       --  Get rid of duplicates, if ORDER is OK
                        --PUT('-'); PUT(SL(I)); NEW_LINE;
                           null;
                        else
                           PA_LAST := PA_LAST + 1;
                        --PUT('+'); PUT(SL(I)); NEW_LINE;
                        --PUT("SAL  PR /= OPR and PA_LAST incremented to "); PUT(PA_LAST); NEW_LINE;
                           PA(PA_LAST) := SL(I);
                           OPR := SL(I);
                        end if;
                     end SUPRESS_KEY_CHECK;

                  else
                     exit;
                  end if;
               end loop;

            end if;

--TEXT_IO.PUT_LINE("At the end of ARRAY_STEMS");
         --for I in 1..PA_LAST  loop
         --PUT(PA(I).STEM); PUT("  "); PUT(PA(I).IR); NEW_LINE;
         --PUT(PA(I).D_K); PUT("> "); PUT(PA(I).AAMNPC.MNPC); NEW_LINE;
         --end loop;

         end ARRAY_STEMS;


         procedure REDUCE_STEM_LIST(SL : in SAL; SXX : in out SAL;
                                    --  Need in out if want to print it at the end
                                    --procedure REDUCE_STEM_LIST(SL : in SAL; SXX : out SAL; 
                                    PREFIX : in PREFIX_ITEM := NULL_PREFIX_ITEM;
                                    SUFFIX : in SUFFIX_ITEM := NULL_SUFFIX_ITEM) is
            MNPC_PART : MNPC_TYPE := NULL_MNPC;
            PDL_PART : PART_ENTRY;
            COM : COMPARISON_TYPE := X;
            NUM_SORT : NUMERAL_SORT_TYPE := X;
            LS : INTEGER := 0;
            M : INTEGER := 0;


            PDL_KEY : STEM_KEY_TYPE;
            PDL_P   : PART_OF_SPEECH_TYPE;
            SL_KEY  : STEM_KEY_TYPE;
            SL_P    : PART_OF_SPEECH_TYPE;

            function "<=" (LEFT, RIGHT : PART_OF_SPEECH_TYPE) return BOOLEAN is
            begin
               if RIGHT = LEFT  or else
                  (LEFT = PACK and RIGHT = PRON)  or else
               RIGHT = X    then
                  return TRUE;
               else
                  return FALSE;
               end if;
            end "<=";

            function "<=" (LEFT, RIGHT : GENDER_TYPE)   return BOOLEAN is
            begin
               if (RIGHT = LEFT   or else
                      (RIGHT = C  and LEFT /= N)  or else
                      (RIGHT = X))  then
                  return TRUE;
               else
                  return FALSE;
               end if;
            end "<=";

            function "<=" (LEFT, RIGHT : STEM_KEY_TYPE)   return BOOLEAN is
            begin
               if (RIGHT = LEFT   or else
                      (RIGHT = 0))  then
                  return TRUE;
               else
                  return FALSE;
               end if;
            end "<=";

         begin
            SXX := (others => NULL_PARSE_RECORD);  --  Essentially initializing
         --for J in 1..PDL_INDEX  loop
         --NUMBER_IN_PDL := NUMBER_IN_PDL + 1;
         --TEXT_IO.PUT(INTEGER'IMAGE(NUMBER_IN_PDL)); 
         --TEXT_IO.PUT("    PDL    "); TEXT_IO.PUT(PDL(J).DS.STEM); 
         --PART_ENTRY_IO.PUT(PDL(J).DS.PART);
         --TEXT_IO.PUT(INTEGER'IMAGE(PDL(J).DS.KEY)); TEXT_IO.NEW_LINE;
         --end loop;
         --TEXT_IO.PUT_LINE("****************************");

         --if WORDS_MODE(WRITE_STATISTICS_FILE)  then
         --declare
         --  PSTAT : PRUNED_DICTIONARY_LIST := PDL; 
         --begin
         --  TEXT_IO.PUT(STATS, "Number of PDL DICT hits");
         --  TEXT_IO.SET_COL(STATS, 30);
         --  INTEGER_IO.PUT(STATS, NUMBER_IN_PDL); 
         --  TEXT_IO.SET_COL(STATS, 40);
         --  TEXT_IO.PUT(STATS, INPUT_WORD);
         --  TEXT_IO.NEW_LINE(STATS);
         --end;
         --end if;
         -------------------------------------------------------------

         --  For the reduced dictionary list PDL
            M := 0;
         ON_PDL:
            for J in 1..PDL_INDEX  loop



               PDL_PART := PDL(J).DS.PART;
               PDL_KEY := PDL(J).DS.KEY;
               MNPC_PART := PDL(J).DS.MNPC;


            --  Is there any point in going through the process for this PDL
               PDL_P  := PDL(J).DS.PART.POFS;  --  Used only for FIX logic below

            --  If there is no SUFFIX then carry on
               if (SUFFIX = NULL_SUFFIX_ITEM)  then  --  No suffix working, fall through
               --PUT_LINE("No SUFFIX in REDUCE - Fall through to PREFIX check "); 
                  null;
               elsif
                  (PDL_P = N    and then PDL_PART.N.DECL = (9, 8)) or  --  No suffix for
                  (PDL_P = ADJ  and then PDL_PART.ADJ.DECL = (9, 8)) then  --  abbreviations
               --   Can be no suffix on abbreviation");
                  goto END_OF_PDL_LOOP;
               else                      --  There is SUFFIX, see if it agrees with PDL
                  if PDL_P <= SUFFIX.ENTR.ROOT  and then     --  Does SUFFIX agree in ROOT
                     ((PDL_KEY <= SUFFIX.ENTR.ROOT_KEY)  or else
                         ((PDL_KEY = 0) and then
                             ((PDL_P = N) or (PDL_P = ADJ) or (PDL_P = V)) and then
                             ((SUFFIX.ENTR.ROOT_KEY = 1) or (SUFFIX.ENTR.ROOT_KEY = 2))))  then
                  --PUT_LINE("HIT HIT HIT HIT HIT HIT HIT HIT HIT     SUFFIX SUFFIX    in REDUCE");
                     case SUFFIX.ENTR.TARGET.POFS is      --  Transform PDL_PART to TARGET
                        when N =>
                           PDL_PART := (N, SUFFIX.ENTR.TARGET.N);
                        when PRON =>
                           PDL_PART := (PRON, SUFFIX.ENTR.TARGET.PRON);
                        when ADJ =>
                           PDL_PART := (ADJ, SUFFIX.ENTR.TARGET.ADJ);
                        when NUM =>
                           PDL_PART := (NUM, SUFFIX.ENTR.TARGET.NUM);
                        when ADV =>
                           PDL_PART := (ADV, SUFFIX.ENTR.TARGET.ADV);
                        when V =>
                           PDL_PART := (V, SUFFIX.ENTR.TARGET.V);
                        when others  =>
                           null;                  --  No others so far, except X = all
                     end case;
                     PDL_KEY := SUFFIX.ENTR.TARGET_KEY;
                     PDL_P  := PDL_PART.POFS;  --  Used only for FIX logic below
                  --PUT("    Changed to    "); PUT(PDL_PART); PUT(PDL_KEY); NEW_LINE;

                  else
                  --PUT_LINE("In REDUCE_STEM_LIST   There is no legal suffix");
                  --            exit;
                     goto END_OF_PDL_LOOP;
                  end if;
               end if;


               if (PREFIX = NULL_PREFIX_ITEM)  then      --  No PREFIX, drop through   
               --PUT_LINE("No PREFIX in REDUCE - Fall through to MATCHing "); 
                  null;
               elsif
                  (PDL_P = N    and then PDL_PART.N.DECL = (9, 8)) or  --  No prefix for
                  (PDL_P = ADJ  and then PDL_PART.ADJ.DECL = (9, 8)) or --  abbreviations
                  (PDL_P = INTERJ  or PDL_P = CONJ) then  --  or INTERJ or CONJ
               --PUT_LINE("In REDUCE_STEM_LIST   no prefix on abbreviationi, interj, conj");
                  goto END_OF_PDL_LOOP;
               else                                           --  Check if PREFIX agrees
               --PUT("PREFIX in REDUCE  "); 
               --PUT(PDL_P); PUT(" <= "); PUT(PREFIX.ENTR.ROOT); PUT("       OR"); NEW_LINE;
               --PUT("                  "); PUT(PDL_PART); PUT(" <= "); 
               --PUT(PREFIX.ENTR.ROOT); NEW_LINE;
                  if (PDL_P = PREFIX.ENTR.ROOT)  or    --  = ROOT 
                     (PDL_PART.POFS = PREFIX.ENTR.ROOT)  then  --  or part mod by suf
                  --PUT_LINE("PREFIX in REDUCE  PART HIT"); 
                     null;
                  elsif (PREFIX.ENTR.ROOT = X)  then  --   or ROOT = X
                     if PDL_P = N or PDL_P = PRON or
                     PDL_P = ADJ or PDL_P = ADV or PDL_P = V  then
                                          --  Dont prefix PREP, CONJ, ...
                     -- PUT_LINE("PREFIX in REDUCE  X    HIT"); 
                        null;
                     end if;
                  else
                     goto END_OF_PDL_LOOP;
                  --PUT_LINE("In REDUCE_STEM_LIST   There is no legal prefix");
                  --              exit;
                  end if;
               end if;

            --  SUFFIX and PREFIX either agree or don't exist (agrees with everything)
            --PUT("ON_PDL:+   "); PUT(PDL(J).DS.STEM); PUT(PDL(J).DS.PART);
            --PUT(PDL(J).DS.KEY); NEW_LINE;
            --PUT_LINE(ADD_PREFIX(PDL(J).DS.STEM, PREFIX) & "|");
            --PUT_LINE(ADD_SUFFIX(ADD_PREFIX(PDL(J).DS.STEM, PREFIX), SUFFIX) & "|");


               LS := LEN(ADD_SUFFIX(ADD_PREFIX(PDL(J).DS.STEM, PREFIX), SUFFIX));
            --PUT("LS = LEN of "); PUT(ADD_SUFFIX(ADD_PREFIX(PDL(J).DS.STEM, PREFIX), SUFFIX
            --PUT("    = "); PUT(LS); NEW_LINE;

            --TEXT_IO.PUT_LINE("Entering ON_SL loop");

            ON_SL:
               for I in SL'RANGE loop
                  exit when SL(I) = NULL_PARSE_RECORD;

               --TEXT_IO.PUT("SL(I)  "); PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
               --TEXT_IO.PUT("    PDL    "); TEXT_IO.PUT(PDL(J).DS.STEM); 
               --PART_ENTRY_IO.PUT(PDL(J).DS.PART);
               --TEXT_IO.PUT(INTEGER'IMAGE(PDL(J).DS.KEY)); TEXT_IO.NEW_LINE;
               --TEXT_IO.PUT("LS  "); TEXT_IO.PUT(INTEGER'IMAGE(LS)); 
               --TEXT_IO.PUT("  LEN SL(I).STEM   "); TEXT_IO.PUT_LINE(INTEGER'IMAGE(LEN(SL(I).STEM))); 

                  if LS  = LEN(SL(I).STEM)  then




                  --  Scan through the whole unreduced stem list
                  --  Single out those stems that match (pruned) dictionary entries
                  --^^^^^^^^^^^^^^^^^should be able to do this better with new arrangement


                     SL_KEY := SL(I).IR.KEY;
                     SL_P := SL(I).IR.QUAL.POFS;


                     if (
                           ((PDL_KEY <= SL(I).IR.KEY) )  or else
                           ((PDL_KEY = 0)  and then
                               (((PDL_P = N) or (PDL_P = ADJ) or (PDL_P = V)) and then
                                   ((SL(I).IR.KEY = 1) or (SL(I).IR.KEY = 2)) ))
                        )  and then   --  and KEY
                        ( PDL_PART.POFS  = EFF_PART(SL(I).IR.QUAL.POFS) )  then

                     --TEXT_IO.PUT_LINE("#######################   PDL - SL  MATCH   ############");

                        if
                           (PDL_PART.POFS = N                                and then
                            PDL_PART.N.DECL <= SL(I).IR.QUAL.N.DECL            and then
                            PDL_PART.N.GENDER <= SL(I).IR.QUAL.N.GENDER)             then
                        --TEXT_IO.PUT_LINE(" HIT  N     ");
                        --  Need to transfer the gender of the noun dictionary item
                           M := M + 1;
                           SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                        IR => (
                                              QUAL => (
                                                      POFS => N,
                                                      N => (
                                                           PDL_PART.N.DECL,
                                                           SL(I).IR.QUAL.N.CS,
                                                           SL(I).IR.QUAL.N.NUMBER,
                                                           PDL_PART.N.GENDER  )  ),
                                              KEY => SL(I).IR.KEY,
                                              ENDING => SL(I).IR.ENDING,
                                              AGE => SL(I).IR.AGE,
                                              FREQ => SL(I).IR.FREQ),
                                        D_K => PDL(J).D_K,
                                        MNPC => MNPC_PART);

                        elsif
                           (PDL_PART.POFS = PRON                             and then
                            PDL_PART.PRON.DECL <= SL(I).IR.QUAL.PRON.DECL)          then
                        --PUT(" HIT  PRON  ");
                        --  Need to transfer the kind of the pronoun dictionary item
                           M := M + 1;
                           SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                        IR => (
                                              QUAL => (
                                                      POFS => PRON,
                                                      PRON => (
                                                              PDL_PART.PRON.DECL,
                                                              SL(I).IR.QUAL.PRON.CS,
                                                              SL(I).IR.QUAL.PRON.NUMBER,
                                                              SL(I).IR.QUAL.PRON.GENDER  )  ),
                                              KEY => SL(I).IR.KEY,
                                              ENDING => SL(I).IR.ENDING,
                                              AGE => SL(I).IR.AGE,
                                              FREQ => SL(I).IR.FREQ),
                                        D_K => PDL(J).D_K,
                                        MNPC => MNPC_PART);
                                                        
                        elsif (PDL_PART.POFS = ADJ)                          and then
                           (PDL_PART.ADJ.DECL <= SL(I).IR.QUAL.ADJ.DECL)     and then
                           ((SL(I).IR.QUAL.ADJ.CO   <= PDL_PART.ADJ.CO  ) or
                               ((SL(I).IR.QUAL.ADJ.CO = X)  or (PDL_PART.ADJ.CO = X)))    then
                        --  Note the reversal on comparisom
 --PUT(" HIT  ADJ   ");
                        --  Need to transfer the gender of the dictionary item
                        --  Need to transfer the CO of the ADJ dictionary item
                           if PDL_PART.ADJ.CO in POS..SUPER  then
                           --  If the dictionary entry has a unique CO, use it
                              COM := PDL_PART.ADJ.CO;
                           else
                           --  Otherwise, the entry is X, generate a CO from KEY
                              COM := ADJ_COMP_FROM_KEY(PDL_KEY);
                           end if;
                           M := M + 1;
                           SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                        IR => (
                                              QUAL => (
                                                      POFS => ADJ,
                                                      ADJ => (
                                                             PDL_PART.ADJ.DECL,
                                                             SL(I).IR.QUAL.ADJ.CS,
                                                             SL(I).IR.QUAL.ADJ.NUMBER,
                                                             SL(I).IR.QUAL.ADJ.GENDER,
                                                             COM )  ),
                                              KEY => SL(I).IR.KEY,
                                              ENDING => SL(I).IR.ENDING,
                                              AGE => SL(I).IR.AGE,
                                              FREQ => SL(I).IR.FREQ),
                                        D_K => PDL(J).D_K,
                                        MNPC => MNPC_PART);

                        elsif (PDL_PART.POFS = NUM)                          and then
                           (PDL_PART.NUM.DECL <= SL(I).IR.QUAL.NUM.DECL)     and then
                           (PDL_KEY         = SL(I).IR.KEY)                   then
                        --PUT(" HIT  NUM    "); 
                           if PDL_PART.NUM.SORT = X  then
                           --  If the entry is X, generate a CO from KEY
                              NUM_SORT:= NUM_SORT_FROM_KEY(PDL_KEY);
                           else
                           --  Otherwise, the dictionary entry has a unique CO, use it
                              NUM_SORT := PDL_PART.NUM.SORT;
                           end if;
                           M := M + 1;
                           SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                        IR => (
                                              QUAL => (
                                                      POFS => NUM,
                                                      NUM => (
                                                             PDL_PART.NUM.DECL,
                                                             SL(I).IR.QUAL.NUM.CS,
                                                             SL(I).IR.QUAL.NUM.NUMBER,
                                                             SL(I).IR.QUAL.NUM.GENDER,
                                                             NUM_SORT)  ),
                                              KEY => SL(I).IR.KEY,
                                              ENDING => SL(I).IR.ENDING,
                                              AGE => SL(I).IR.AGE,
                                              FREQ => SL(I).IR.FREQ),
                                        D_K => PDL(J).D_K,
                                        MNPC => MNPC_PART);


                        elsif (PDL_PART.POFS = ADV)                          and then
                           ((PDL_PART.ADV.CO   <= SL(I).IR.QUAL.ADV.CO  ) or
                               ((SL(I).IR.QUAL.ADV.CO = X)  or (PDL_PART.ADV.CO = X)))    then
                        --PUT(" HIT  ADV   ");
                        --  Need to transfer the CO of the ADV dictionary item
                           if PDL_PART.ADV.CO in POS..SUPER  then
                           --  If the dictionary entry has a unique CO, use it
                              COM := PDL_PART.ADV.CO;
                           else
                           --  The entry is X and we need to generate a COMP from the KEY
                              COM := ADV_COMP_FROM_KEY(PDL_KEY);
                           end if;
                           M := M + 1;
                           SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                        IR => (
                                              QUAL => (
                                                      POFS => ADV,
                                                      ADV => (
                                                             CO => COM)  ),
                                              KEY => SL(I).IR.KEY,
                                              ENDING => SL(I).IR.ENDING,
                                              AGE => SL(I).IR.AGE,
                                              FREQ => SL(I).IR.FREQ),
                                        D_K => PDL(J).D_K,
                                        MNPC => MNPC_PART);

                        elsif (PDL_PART.POFS = V)                         then
                        --TEXT_IO.PUT_LINE("V found, now check CON");
                           if SL(I).IR.QUAL.POFS = V     and then
                              (PDL_PART.V.CON <= SL(I).IR.QUAL.V.CON) then
                           --TEXT_IO.PUT(" HIT  V     ");
                              M := M + 1;
                              SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                           IR => (
                                                 QUAL => (
                                                         POFS => V,
                                                         V => (
                                                              PDL_PART.V.CON,
                                                              SL(I).IR.QUAL.V.TENSE_VOICE_MOOD,
                                                              SL(I).IR.QUAL.V.PERSON,
                                                              SL(I).IR.QUAL.V.NUMBER )  ),
                                                 KEY => SL(I).IR.KEY,
                                                 ENDING => SL(I).IR.ENDING,
                                                 AGE => SL(I).IR.AGE,
                                                 FREQ => SL(I).IR.FREQ),
                                           D_K => PDL(J).D_K,
                                           MNPC => MNPC_PART);

                           elsif SL(I).IR.QUAL.POFS = VPAR   and then
                              (PDL_PART.V.CON <= SL(I).IR.QUAL.VPAR.CON)   then
                           --PUT(" HIT  VPAR  ");
                              M := M + 1;
                              SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                           IR => (
                                                 QUAL => (
                                                         POFS => VPAR,
                                                         VPAR => (
                                                                 PDL_PART.V.CON,
                                                                 SL(I).IR.QUAL.VPAR.CS,
                                                                 SL(I).IR.QUAL.VPAR.NUMBER,
                                                                 SL(I).IR.QUAL.VPAR.GENDER,
                                                                 SL(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD )  ),
                                                 KEY => SL(I).IR.KEY,
                                                 ENDING => SL(I).IR.ENDING,
                                                 AGE => SL(I).IR.AGE,
                                                 FREQ => SL(I).IR.FREQ),
                                           D_K => PDL(J).D_K,
                                           MNPC => MNPC_PART);


                           elsif SL(I).IR.QUAL.POFS = SUPINE   and then
                              (PDL_PART.V.CON <= SL(I).IR.QUAL.SUPINE.CON)   then
                           --PUT(" HIT  SUPINE");
                              M := M + 1;
                              SXX(M) := (STEM => SUBTRACT_PREFIX(SL(I).STEM, PREFIX),
                                           IR => (
                                                 QUAL => (
                                                         POFS => SUPINE,
                                                         SUPINE => (
                                                                   PDL_PART.V.CON,
                                                                   SL(I).IR.QUAL.SUPINE.CS,
                                                                   SL(I).IR.QUAL.SUPINE.NUMBER,
                                                                   SL(I).IR.QUAL.SUPINE.GENDER)  ),
                                                 KEY => SL(I).IR.KEY,
                                                 ENDING => SL(I).IR.ENDING,
                                                 AGE => SL(I).IR.AGE,
                                                 FREQ => SL(I).IR.FREQ),
                                           D_K => PDL(J).D_K,
                                           MNPC => MNPC_PART);


                           end if;

                        elsif PDL_PART.POFS = PREP and then
                        PDL_PART.PREP.OBJ = SL(I).IR.QUAL.PREP.OBJ           then
                        --PUT(" HIT  PREP  ");
                           M := M + 1;
                           SXX(M) := (SUBTRACT_PREFIX(SL(I).STEM, PREFIX), SL(I).IR,
                                        PDL(J).D_K, MNPC_PART);

                        elsif PDL_PART.POFS = CONJ                              then
                        --PUT(" HIT  CONJ  ");
                           M := M + 1;
                           SXX(M) := (SUBTRACT_PREFIX(SL(I).STEM, PREFIX), SL(I).IR,
                                        PDL(J).D_K, MNPC_PART);

                        elsif PDL_PART.POFS = INTERJ                            then
                        --PUT(" HIT  INTERJ ");
                           M := M + 1;
                           SXX(M) := (SUBTRACT_PREFIX(SL(I).STEM, PREFIX), SL(I).IR,
                                        PDL(J).D_K, MNPC_PART);


                        end if;

                     --TEXT_IO.NEW_LINE; PUT(SL(I).IR.QUAL); TEXT_IO.PUT("  --  "); 
                     --TEXT_IO.PUT(PDL(J).DS.STEM); PUT(PDL_PART); TEXT_IO.NEW_LINE;

                     end if;
                  end if;
               <<END_OF_SL_LOOP>> null;
               end loop ON_SL;
            --TEXT_IO.PUT("In RED_ST_L   after loop ON_SL  M = "); 
            --TEXT_IO.PUT(INTEGER'IMAGE(M)); TEXT_IO.NEW_LINE;


            <<END_OF_PDL_LOOP>> null;
            end loop ON_PDL;
         --for I in 1..M  loop
         --TEXT_IO.PUT(INTEGER'IMAGE(I)); TEXT_IO.PUT("  "); 
         --PARSE_RECORD_IO.PUT(SXX(I)); TEXT_IO.NEW_LINE;
         --end loop;
         end REDUCE_STEM_LIST;



         procedure APPLY_PREFIX(SA : in STEM_ARRAY_TYPE; SUFFIX : in SUFFIX_ITEM;
                                SX : in SAL; SXX : in out SAL;
                                PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is
         --  Worry about the stem changing re-cipio from capio
         --  Correspondence of parts, need EFF for VPAR
         --  The prefixes should be ordered with the longest/most likely first
            SSA : STEM_ARRAY;
            L : INTEGER :=  0;
            --use TEXT_IO;
            --use INFLECTIONS_PACKAGE.INTEGER_IO;
            
         begin
         --PUT_LINE("Entering APPLY_PREFIX");
            SXX := (others => NULL_PARSE_RECORD);    --  !!!!!!!!!!!!!!!!!!!!!!!
            
           if WORDS_MDEV(USE_PREFIXES)  then
              

         
--PUT(NUMBER_OF_PREFIXES); PUT(INTEGER(SA'LENGTH)); PUT(SA'LAST); NEW_LINE;
            for I in 1..NUMBER_OF_PREFIXES  loop       --  Loop through PREFIXES
               L :=  0;
               for J in SA'RANGE  loop                  --  Loop through stem array
--PUT("J = "); PUT(J); PUT("   SA(J) = "); PUT(SA(J)); NEW_LINE;
                  if (SA(J)(1) = PREFIXES(I).FIX(1))  then  --  Cuts down a little -- do better
                  if SUBTRACT_PREFIX(SA(J), PREFIXES(I)) /=
                  HEAD(SA(J), MAX_STEM_SIZE)  then
--PUT_LINE("Hit on prefix  " & PREFIXES(I).FIX);
                  --PUT("I = "); PUT(I); PUT("  "); PUT(PREFIXES(I).FIX); PUT("  "); 
                  --PUT("J = "); PUT(J); PUT("  "); PUT(SA(J)); NEW_LINE;
                     L := L + 1;            --  We have a hit, make new stem array item
                     SSA(L) := HEAD(SUBTRACT_PREFIX(SA(J), PREFIXES(I)),
                                    MAX_STEM_SIZE);  --  And that has prefix subtracted to match dict
                  --PUT("L = "); PUT(L); PUT("   "); PUT_LINE(SUBTRACT_PREFIX(SA(J), PREFIXES(I)));
                  end if;                               --  with prefix subtracted stems
                  end if;
               end loop;

               if L > 0  then                        --  There has been a prefix hit
                  SEARCH_DICTIONARIES(SSA(1..L),      --  So run new dictionary search
                                      PREFIXES(I), SUFFIX);

                  if  PDL_INDEX /= 0     then                  --  Dict search was successful
                  --PUT_LINE("IN APPLY_PREFIX -  PDL_INDEX not 0     after prefix  " & PREFIXES(I).FIX);

                  --PUT_LINE("REDUCE_STEM_LIST being called from APPLY_PREFIX  ----  SUFFIX = "
                  --& SUFFIX.FIX);
                     REDUCE_STEM_LIST(SX, SXX, PREFIXES(I), SUFFIX);

                     if SXX(1) /= NULL_PARSE_RECORD  then   --  There is reduced stem result
                        PA_LAST := PA_LAST + 1;        --  So add prefix line to parse array
                        PA(PA_LAST).IR :=
                              ((PREFIX, NULL_PREFIX_RECORD), 0, NULL_ENDING_RECORD, X, X);
                        PA(PA_LAST).STEM := HEAD(PREFIXES(I).FIX, MAX_STEM_SIZE);
                        PA(PA_LAST).MNPC := DICT_IO.COUNT(PREFIXES(I).MNPC);
                        PA(PA_LAST).D_K  := ADDONS;
                        exit;      --  Because we accept only one prefix                  
                     end if;

                  end if;
               end if;
             end loop;      --  Loop on I for PREFIXES
           end if;  --  On USE_PREFIXES
         end APPLY_PREFIX;


         procedure APPLY_SUFFIX(SA : in STEM_ARRAY_TYPE;
                                SX : in SAL; SXX : in out SAL;
                                PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is
            SSA : STEM_ARRAY;
            L : INTEGER :=  0;
            SUFFIX_HIT : INTEGER := 0;
--            use TEXT_IO;
--            use INFLECTIONS_PACKAGE.INTEGER_IO;
            
         begin
         --PUT_LINE("Entering APPLY_SUFFIX");
         --PUT(NUMBER_OF_SUFFIXES); PUT(INTEGER(SA'LENGTH)); PUT(SA'LAST); NEW_LINE;
            for I in 1..NUMBER_OF_SUFFIXES  loop       --  Loop through SUFFIXES 
               L :=  0;                                 --  Take as many as fit

               for J in SA'RANGE  loop                  --  Loop through stem array
                  if SUBTRACT_SUFFIX(SA(J), SUFFIXES(I)) /=
                  HEAD(SA(J), MAX_STEM_SIZE)  then
                  --PUT("Hit on suffix  " & SUFFIXES(I).FIX & "    " & SUFFIXES(I).CONNECT & "  ");
                  --PUT(SUFFIXES(I).ENTR); NEW_LINE;
                  --PUT("I = "); PUT(I); PUT("  "); PUT(SUFFIXES(I).FIX); PUT("  "); 
                  --PUT("J = "); PUT(J); PUT("  "); PUT(SA(J)); NEW_LINE;
                     L := L + 1;            --  We have a hit, make new stem array item
                     SSA(L) := HEAD(SUBTRACT_SUFFIX(SA(J), SUFFIXES(I)),
                                    MAX_STEM_SIZE);  --  And that has prefix subtracted to match dict
                  --PUT("L = "); PUT(L); PUT("   "); PUT_LINE(SUBTRACT_SUFFIX(SA(J), SUFFIXES(I)));
                  end if;
               end loop;    --  Loop on J through SA

               if L > 0  then                        --  There has been a suffix hit
                  SEARCH_DICTIONARIES(SSA(1..L),
                                      NULL_PREFIX_ITEM, SUFFIXES(I));     --  So run new dictionary search
               --  For suffixes we allow as many as match 
               
                  if  PDL_INDEX /= 0     then                  --  Dict search was successful
                  --PUT_LINE("IN APPLY_SUFFIX -  PDL_INDEX not 0     after suffix  " & SUFFIXES(I).FIX);

                  --PUT_LINE("REDUCE_STEM_LIST called from APPLY_SUFFIX");
                     SUFFIX_HIT := I;

                     REDUCE_STEM_LIST(SX, SXX, NULL_PREFIX_ITEM, SUFFIXES(I));

                     if SXX(1) /= NULL_PARSE_RECORD  then    --  There is reduced stem result
                        PA_LAST := PA_LAST + 1;        --  So add suffix line to parse array
                     --PUT_LINE("REDUCE_STEM_LIST is not null so add suffix to parse array");
                        PA(PA_LAST).IR :=
                              ((SUFFIX, NULL_SUFFIX_RECORD), 0, NULL_ENDING_RECORD, X, X);
                        PA(PA_LAST).STEM := HEAD(
                                                SUFFIXES(SUFFIX_HIT).FIX, MAX_STEM_SIZE);
                     --  Maybe it would better if suffix.fix was of stem size
                        PA(PA_LAST).MNPC := DICT_IO.COUNT(SUFFIXES(SUFFIX_HIT).MNPC);
                     --PUT("SUFFIX MNPC  "); PUT(SUFFIXES(SUFFIX_HIT).MNPC); NEW_LINE;
                        PA(PA_LAST).D_K  := ADDONS;
                     ---
                        for I in SXX'RANGE  loop
                           exit when SXX(I) = NULL_PARSE_RECORD;
                           PA_LAST := PA_LAST + 1;
                           PA(PA_LAST) := SXX(I);
                        end loop;
                     ---
                     end if;


                  else   --  there is suffix (L /= 0) but no dictionary hit
                     SUFFIX_HIT := I;
                  --PUT_LINE("   --  there is suffix (L /= 0) but no dictionary hit");
                  --PUT("L = "); PUT(L); PUT("    "); 
                  --PUT("SUFFIX_HIT = "); PUT(SUFFIXES(I).FIX); NEW_LINE;
                     APPLY_PREFIX(SSA(1..L), SUFFIXES(I), SX, SXX, PA, PA_LAST);
                  --PUT_LINE("PREFIXES applied from APPLY_SUFFIXES");
                     if SXX(1) /= NULL_PARSE_RECORD  then    --  There is reduced stem result
                        PA_LAST := PA_LAST + 1;        --  So add suffix line to parse array
                     --PUT_LINE("REDUCE_STEM_LIST is not null so add suffix to parse array");
                        PA(PA_LAST).IR :=
                              ((SUFFIX, NULL_SUFFIX_RECORD), 0, NULL_ENDING_RECORD, X, X);
                        PA(PA_LAST).STEM := HEAD(
                                                SUFFIXES(SUFFIX_HIT).FIX, MAX_STEM_SIZE);
                        PA(PA_LAST).MNPC := DICT_IO.COUNT(SUFFIXES(SUFFIX_HIT).MNPC);
                     --PUT("SUFFIX MNPC  "); PUT(SUFFIXES(SUFFIX_HIT).MNPC); NEW_LINE;
                        PA(PA_LAST).D_K  := ADDONS;

                        for I in SXX'RANGE  loop    --  Set this set of results
                           exit when SXX(I) = NULL_PARSE_RECORD;
                           PA_LAST := PA_LAST + 1;
                           PA(PA_LAST) := SXX(I);
                        end loop;

                     end if;

                  end if;
               end if;                               --  with suffix subtracted stems
            end loop;      --  Loop on I for SUFFIXES

         end APPLY_SUFFIX;


         procedure PRUNE_STEMS(INPUT_WORD : STRING; SX : in SAL; SXX : in out SAL) is
            J : INTEGER := 0;
         --SXX : SAL;

         begin
--TEXT_IO.PUT_LINE("Entering PRUNE_STEMS    INPUT_WORD = " & INPUT_WORD );
--TEXT_IO.PUT_LINE("In PRUNE   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
            if SX(1) = NULL_PARSE_RECORD  then
               return; end if;

         -----------------------------------------------------------------

            GENERATE_REDUCED_STEM_ARRAY:
            begin
            --PUT_LINE("List of stems by size");
            --NEW_LINE;
               J := 1;
               for Z in 0..MIN(MAX_STEM_SIZE, LEN(INPUT_WORD))  loop
                  if SA(Z) /= NOT_A_STEM  then
                  --PUT(Z); PUT(J); PUT("  "); PUT_LINE(SA(Z));
                     SSA(J) := SA(Z);                                               
                     SSA_MAX := J;
                     J := J + 1;
                  end if;
               end loop;
            --PUT_LINE("SSA_MAX = " & INTEGER'IMAGE(SSA_MAX));
            --NEW_LINE(2);
            end GENERATE_REDUCED_STEM_ARRAY;



--TEXT_IO.PUT_LINE("PRUNE_STEMS   checking (not)    DO_ONLY_FIXES   = " & BOOLEAN'IMAGE(WORDS_MDEV(DO_ONLY_FIXES)));
            if not WORDS_MDEV(DO_ONLY_FIXES)  then   --  Just bypass main dictionary search
--TEXT_IO.PUT_LINE("Calling SEARCH_DICTIONARIES from PRUNE_STEMS   ---  General case");

               SEARCH_DICTIONARIES(SSA(1..SSA_MAX), NULL_PREFIX_ITEM, NULL_SUFFIX_ITEM);
--TEXT_IO.PUT_LINE("Finished SEARCH_DICTIONARIES from PRUNE_STEMS   ---  General case");
            end if;
--TEXT_IO.PUT_LINE("PRUNE_STEMS   passing over because of NOT DO_ONLY_FIXES");
           
 
--              ---------------------------------------------------------------
--TEXT_IO.PUT_LINE("PRUNE_STEMS   below  NOT DO_ONLY_FIXES  PA_LAST = " 
--& INTEGER'IMAGE(PA_LAST));

            if (((PA_LAST = 0)  and            --  No Uniques or Syncope
                (PDL_INDEX = 0))  --)   and then    --  No dictionary match
                  or WORDS_MDEV(DO_FIXES_ANYWAY))  and then 
                WORDS_MODE(DO_FIXES)  then
                
              ----So try prefixes and suffixes, Generate a new SAA array, search again
--TEXT_IO.PUT_LINE(" PDL_INDEX = 0     after straight search   ------  So APPLY_SUFFIX  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

               if SXX(1) = NULL_PARSE_RECORD  then        --  We could not find a match with suffix
                  APPLY_PREFIX(SSA(1..SSA_MAX), NULL_SUFFIX_ITEM, SX, SXX, PA, PA_LAST);
               end if;
            --------------
               if SXX(1) = NULL_PARSE_RECORD  then        --  We could not find a match with suffix
                  APPLY_SUFFIX(SSA(1..SSA_MAX), SX, SXX, PA, PA_LAST);
                  if SXX(1) = NULL_PARSE_RECORD  then        --  We could not find a match with suffix
                    ----So try prefixes, Generate a new SAA array, search again
--TEXT_IO.PUT_LINE(" PDL_INDEX = 0     after suffix search  -----  So APPLY_PREFIX by itself  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                    ----Need to use the new SSA, modified to include suffixes
                     APPLY_PREFIX(SSA(1..SSA_MAX), NULL_SUFFIX_ITEM, SX, SXX, PA, PA_LAST);
--TEXT_IO.PUT_LINE("PREFIXES applied  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                  --------------
                  end if;       --  Suffix failed
               end if;       --  Suffix failed
            else
--TEXT_IO.PUT_LINE(" PDL_INDEX not 0     after straight search   ------  So REDUCE_STEMS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
               REDUCE_STEM_LIST(SX, SXX, NULL_PREFIX_ITEM, NULL_SUFFIX_ITEM);
               if PA_LAST = 0  and then  SXX(1) = NULL_PARSE_RECORD  then
--TEXT_IO.PUT_LINE("Although  PDL_INDEX not 0     after straight search , SXX fails  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
               --------------
                  if WORDS_MODE(DO_FIXES)  then
                     APPLY_SUFFIX(SSA(1..SSA_MAX), SX, SXX, PA, PA_LAST);
--TEXT_IO.PUT_LINE("SUFFIXES applied  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     if SXX(1) = NULL_PARSE_RECORD  then        --  We could not find a match with suffix
                       ----So try prefixes, Generate a new SAA array, search again
--TEXT_IO.PUT_LINE(" PDL_INDEX = 0     after suffix search  -----  So APPLY_PREFIX by itself  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                       ----Need to use the new SSA, modified to include suffixes
                        APPLY_PREFIX(SSA(1..SSA_MAX), NULL_SUFFIX_ITEM,
                                     SX, SXX, PA, PA_LAST);
--TEXT_IO.PUT_LINE("PREFIXES applied  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     --------------
                     end if;   --  Suffix failed
                  end if;     --  If DO_FIXES then do
               end if;       --  First search passed but SXX null
            end if;         --  First search failed

--TEXT_IO.PUT_LINE("End of PRUNE_STEMS   PA_LAST = " & INTEGER'IMAGE(PA_LAST));


         end PRUNE_STEMS;


         procedure PROCESS_PACKONS(INPUT_WORD : STRING; KEY : STEM_KEY_TYPE := 0) is

            STEM_LENGTH  : INTEGER := 0;
            PR   : PARSE_RECORD;
            M : INTEGER := 1;
            DE : DICTIONARY_ENTRY;
            MEAN : MEANING_TYPE;
            PACKON_FIRST_HIT : BOOLEAN := FALSE;
            SL, SL_NULLS : SAL := (others => NULL_PARSE_RECORD);

            function "<=" (LEFT, RIGHT : PRONOUN_KIND_TYPE)   return BOOLEAN is
            begin
               if (RIGHT = LEFT   or else
                   RIGHT = X)  then
                  return TRUE;
               elsif
                  (RIGHT = ADJECT and    --  Just for PACK
                   LEFT  = INDEF)   then
                  return TRUE;
               else
                  return FALSE;
               end if;
            end "<=";

         begin

         OVER_PACKONS:
            for K in PACKONS'RANGE  loop    -- Do whole set, more than one may apply
            --TEXT_IO.PUT_LINE("OVER_PACKONS   K = "& INTEGER'IMAGE(K) & "   PACKON = " & PACKONS(K).TACK);
            --  PACKON if the TACKON ENTRY is PRON
               FOR_EACH_PACKON:
               declare
                  XWORD : constant STRING := SUBTRACT_TACKON(INPUT_WORD, PACKONS(K));
                  WORD : STRING(1..XWORD'LENGTH) := XWORD;
                  PACKON_LENGTH : constant INTEGER := TRIM(PACKONS(K).TACK)'LENGTH;
                  LAST_OF_WORD : CHARACTER := WORD(WORD'LAST);
                  LENGTH_OF_WORD   : constant INTEGER := WORD'LENGTH;
               begin
               --PUT_LINE("FOR_EACH_PACKON   WORD = |"& WORD & "|");
               --PUT_LINE("FOR_EACH_PACKON   PACKON_LENGTH = "& INTEGER'IMAGE(PACKON_LENGTH));
               --PUT_LINE("FOR_EACH_PACKON   LENGTH_OF_WORD = "& INTEGER'IMAGE(LENGTH_OF_WORD));
                  SL := SL_NULLS;      --  Initialize SL to nulls
                  if WORD  /= INPUT_WORD  then
                  --PUT_LINE("PROCESS_PACKONS Hit on PACKON    " & PACKONS(K).TACK);
                     PACKON_FIRST_HIT := TRUE;

                     if PACKONS(K).TACK(1..3) = "dam" and  LAST_OF_WORD = 'n'  then
                        WORD(WORD'LAST) := 'm';   --  Takes care of the m - > n shift with dam
                        LAST_OF_WORD := 'm';
                     --PUT_LINE("PACKON = dam   and LAST_OF_WORD = n    => " & WORD);
                     end if;

                  --  No blank endings in these pronouns
                     LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE, LEL, 4);

                     M := 0;
                  ON_INFLECTS:
                     for Z in reverse 1..MIN(6, LENGTH_OF_WORD)  loop   --  optimum for qu-pronouns
                     --PUT("ON_INFLECTS  Z = "); PUT(Z); PUT("  "); PUT(WORD(1..Z)); NEW_LINE;
                        if PELL(Z, LAST_OF_WORD) > 0  then   --  Any possible inflections at all
                           for I in PELF(Z, LAST_OF_WORD)..PELL(Z, LAST_OF_WORD) loop
                           --PUT("+");PUT(LEL(I)); PUT(WORD'LAST); PUT(WORD(WORD'LAST-Z+1..WORD'LAST)); 
                           --PUT("  "); PUT((LEL(I).ENDING.SUF(1..Z))); NEW_LINE;

                              if (Z <= LENGTH_OF_WORD)  and then
                                 ((EQU(LEL(I).ENDING.SUF(1..Z),
                                        WORD(WORD'LAST-Z+1..WORD'LAST)))  and
                                     (LEL(I).QUAL.PRON.DECL <= PACKONS(K).ENTR.BASE.PACK.DECL))  then
                              --  Have found an ending that is a possible match
                              --  And INFLECT agrees with PACKON.BASE
                              --PUT_LINE("INFLECTS HIT ------------------------------------------------------");

                              --  Add to list of possible ending records
                                 STEM_LENGTH := WORD'LENGTH - Z;
                                 PR := (HEAD(WORD(WORD'FIRST..STEM_LENGTH), MAX_STEM_SIZE),
                                          LEL(I), DEFAULT_DICTIONARY_KIND, NULL_MNPC);
                                 M := M + 1;
                                 SL(M) := PR;
                                 SSA(1) := HEAD(WORD(WORD'FIRST.. WORD'FIRST+STEM_LENGTH-1),
                                                MAX_STEM_SIZE);
                              --PUT_LINE("STEM_LENGTH = " & INTEGER'IMAGE(STEM_LENGTH));
                              --PUT_LINE("SSA(1) in PACKONS from real  INFLECTS ->" & SSA(1) & '|');
                              --  may get set several times
                              end if;
                           end loop;
                        end if;
                     end loop ON_INFLECTS;



                  --  Only one stem will emerge
                     PDL_INDEX := 0;
                     SEARCH_DICTIONARIES(SSA(1..1), NULL_PREFIX_ITEM, NULL_SUFFIX_ITEM,
                                         PACK_ONLY);
                  --  Now have a PDL, scan for agreement

                  PDL_LOOP:
                     for J in 1..PDL_INDEX  loop  --  Go through all dictionary hits to see 
                     --PUT_LINE("PACKON  PDL_INDEX  "); PUT(PDL(J).DS.STEM); PUT(PDL(J).DS.PART); NEW_LINE;
                     --  M used here wher I is used in REDUCE, maybe make consistent
                        M := 1;
                     SL_LOOP:
                        while SL(M) /= NULL_PARSE_RECORD  loop  --  Over all inflection hits
                          --  if this stem is possible  
                        --  call up the meaning to check for "(w/-"
                           DICT_IO.SET_INDEX(DICT_FILE(PDL(J).D_K), PDL(J).DS.MNPC);
                           DICT_IO.READ(DICT_FILE(PDL(J).D_K), DE);
                           MEAN := DE.MEAN;

                           if (TRIM(MEAN)(1..4) = "(w/-"  and then  --  Does attached PACKON agree
                               TRIM(MEAN)(5..4+PACKON_LENGTH) = TRIM(PACKONS(K).TACK))   then
                           --PUT_LINE("Mean = PACK Hit Hit ");
                           --PUT("MEAN|" & MEAN(1..4) & '|');
                           --PUT_LINE("PACK|" & TRIM(PACKONS(K).TACK) & '|');

                           --PUT("DECL    PDL_INDEX "); PUT(PDL(J).DS.PART.PACK.DECL);  
                           --PUT(" <= ?  SL   ");
                           --PUT(SL(M).IR.QUAL.PRON.DECL); 
                           --PUT(" <= ?  PACKON  ");
                           --PUT(PACKONS(K).ENTR.BASE.PACK.DECL); NEW_LINE;
                              if (PDL(J).DS.PART.PACK.DECL = SL(M).IR.QUAL.PRON.DECL)   then  --  or 

                              --PUT_LINE("DECL    Hit Hit Hit Hit Hit Hit Hit Hit ");
                              --PUT("KINDS  PACKON   ");
                              --PUT(PACKONS(K).ENTR.BASE.PACK.KIND); 
                              --PUT(" <= ?   PDL_KIND    ");
                              --PUT(PDL(J).DS.PART.PACK.KIND); 
                              --NEW_LINE;
                                 --if (PACKONS(K).ENTR.BASE.PACK.KIND   <=
                                 --    PDL(J).DS.PART.PACK.KIND)   then
                                 --  Then we have a hit and make a PA
                                 --PUT_LINE("KIND    Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit Hit ");

                                    if PACKON_FIRST_HIT then
                                       PA_LAST := PA_LAST + 1;
                                       PA(PA_LAST) := (PACKONS(K).TACK,
                                                            ((TACKON, NULL_TACKON_RECORD), 0,
                                                              NULL_ENDING_RECORD, X, X),
                                                         ADDONS,
                                                            DICT_IO.COUNT((PACKONS(K).MNPC)));
                                       PACKON_FIRST_HIT := FALSE;

                                    end if;                          
                                    PA_LAST := PA_LAST + 1;
                                 --PUT_LINE("PACKON  PDL HIT    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                                 --PUT(PDL(J).DS.STEM); PUT(PDL(J).DS.PART); NEW_LINE;
                                 --PUT_LINE(MEAN);
                                 --PUT(PA(PA_LAST)); NEW_LINE;
                                    PA(PA_LAST) := (STEM => SL(M).STEM,
                                                      IR => (
                                                            QUAL => (
                                                                    POFS => PRON,
                                                                    PRON => (
                                                                            PDL(J).DS.PART.PACK.DECL,
                                                                            SL(M).IR.QUAL.PRON.CS,
                                                                            SL(M).IR.QUAL.PRON.NUMBER,
                                                                            SL(M).IR.QUAL.PRON.GENDER )),
                                                            KEY => SL(M).IR.KEY,
                                                            ENDING => SL(M).IR.ENDING,
                                                            AGE => SL(M).IR.AGE,
                                                            FREQ => SL(M).IR.FREQ),
                                                      D_K => PDL(J).D_K,
                                                      MNPC => PDL(J).DS.MNPC);
                                 --end if;
                              end if;
                           end if;
                           M := M + 1;

                        end loop SL_LOOP;

                     end loop PDL_LOOP;

                  end if;
               end FOR_EACH_PACKON;
               PACKON_FIRST_HIT := FALSE;

            end loop OVER_PACKONS;

         end PROCESS_PACKONS;


         procedure PROCESS_QU_PRONOUNS(INPUT_WORD : STRING; QKEY : STEM_KEY_TYPE := 0) is

            WORD : constant STRING := LOWER_CASE(TRIM(INPUT_WORD));
            LAST_OF_WORD : constant CHARACTER := WORD(WORD'LAST);
            LENGTH_OF_WORD   : constant INTEGER := WORD'LENGTH;
            STEM_LENGTH  : INTEGER := 0;
            M : INTEGER := 0;
            PR   : PARSE_RECORD;
            SL : SAL := (others => NULL_PARSE_RECORD);

         begin
--TEXT_IO.PUT_LINE("PROCESS_QU_PRONOUNS   " & INPUT_WORD);

         --  No blank endings in these pronouns
            LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE, LEL, 4);

         --  M used here while I is used in REDUCE, maybe make consistent
            M := 0;
         ON_INFLECTS:
            for Z in reverse 1..MIN(4, LENGTH_OF_WORD)  loop     --  optimized for qu-pronouns
            --PUT("ON_INFLECTS  "); PUT(Z); PUT("  "); PUT(LAST_OF_WORD); NEW_LINE;
               if PELL(Z, LAST_OF_WORD) > 0  then   --  Any possible inflections at all
                  for I in PELF(Z, LAST_OF_WORD)..PELL(Z, LAST_OF_WORD) loop
                  --PUT(LEL(I)); PUT(WORD'LAST); PUT(WORD'LAST-Z+1); NEW_LINE;
                     if (Z <= LENGTH_OF_WORD)  and then
                        LEL(I).KEY = QKEY  and then
                        EQU(LEL(I).ENDING.SUF(1..Z),
                          WORD(WORD'LAST-Z+1..WORD'LAST))    then
                     --  Have found an ending that is a possible match
                     --PUT_LINE("INFLECTS HIT --------------------------------------------");

                     --  Add to list of possible ending records
                        STEM_LENGTH := WORD'LENGTH - Z;
                        PR := (HEAD(WORD(WORD'FIRST..STEM_LENGTH), MAX_STEM_SIZE),
                                 LEL(I), DEFAULT_DICTIONARY_KIND, NULL_MNPC);
                        M := M + 1;
                        SL(M) := PR;
                     --PUT("M = "); PUT(M); PUT("    "); PUT(SL(M)); NEW_LINE;
                        SSA(1) := HEAD(WORD(WORD'FIRST.. WORD'FIRST+STEM_LENGTH-1),
                                       MAX_STEM_SIZE);
                     --  may get set several times
                     end if;
                  end loop;
               end if;
            end loop ON_INFLECTS;



         --  Only one stem will emerge
            PDL_INDEX := 0;
            SEARCH_DICTIONARIES(SSA(1..1), NULL_PREFIX_ITEM, NULL_SUFFIX_ITEM,
                                QU_PRON_ONLY);
         --  Now have a PDL, scan for agreement

         PDL_LOOP:
            for J in 1..PDL_INDEX  loop  --  Go through all dictionary hits to see 
               M := 1;
            SL_LOOP:
               while SL(M) /= NULL_PARSE_RECORD  loop  --  Over all inflection hits
               --PUT("SL_LOOP  M = "); PUT(M); PUT("  SL => "); PUT(SL(M)); NEW_LINE; NEW_LINE;

               --PUT("DECL    PDL "); PUT(PDL(J).DS.PART.PRON.DECL);  
               --PUT(" <= ?  SL   ");
               --PUT(SL(M).IR.QUAL.PRON.DECL); 
               --NEW_LINE;
                  if (PDL(J).DS.PART.PRON.DECL = SL(M).IR.QUAL.PRON.DECL)   then
                  --PUT_LINE("DECL    Hit Hit Hit Hit Hit Hit Hit Hit ");
                     PA_LAST := PA_LAST + 1;
                  --PUT_LINE("QU_PRON  PDL HIT    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                  --PUT(PDL(J).DS.STEM); PUT(PDL(J).DS.PART); NEW_LINE;
                  --PUT(PA(PA_LAST)); NEW_LINE;
                     PA(PA_LAST) := (STEM => SL(M).STEM,
                                       IR => (
                                             QUAL => (
                                                     POFS => PRON,
                                                     PRON => (
                                                             PDL(J).DS.PART.PRON.DECL,
                                                             SL(M).IR.QUAL.PRON.CS,
                                                             SL(M).IR.QUAL.PRON.NUMBER,
                                                             SL(M).IR.QUAL.PRON.GENDER )),
                                             KEY => SL(M).IR.KEY,
                                             ENDING => SL(M).IR.ENDING,
                                             AGE => SL(M).IR.AGE,
                                             FREQ => SL(M).IR.FREQ),
                                       D_K => PDL(J).D_K,
                                       MNPC => PDL(J).DS.MNPC);
                  end if;
                  M := M + 1;

               end loop SL_LOOP;
            -- PDL:= PDL.SUCC;
            end loop PDL_LOOP;

         end PROCESS_QU_PRONOUNS;



         procedure TRY_TACKONS(INPUT_WORD : STRING) is
            TACKON_HIT : BOOLEAN := FALSE;
            TACKON_ON  : BOOLEAN := FALSE;
            TACKON_LENGTH : constant INTEGER := 0; 
            J : INTEGER := 0;
            DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
            MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
            ENTERING_PA_LAST : INTEGER := PA_LAST;
            START_OF_LOOP : INTEGER := 5;    --  4 enclitics     --  Hard number  !!!!!!!!!!!!!!!
            END_OF_LOOP : INTEGER := NUMBER_OF_TACKONS;
         begin
--TEXT_IO.PUT_LINE("TRYing TACKONS   ***************  INPUT_WORD = " & INPUT_WORD);  

         LOOP_OVER_TACKONS:
            for I in START_OF_LOOP..END_OF_LOOP  loop
            --PUT_LINE("TACKON #" & INTEGER'IMAGE(I) & "    "  & 
            --SUBTRACT_TACKON(INPUT_WORD, TACKONS(I)) & "  +  " & TACKONS(I).TACK);                  

               REMOVE_A_TACKON:
               declare
                  LESS : constant STRING :=
                     SUBTRACT_TACKON(INPUT_WORD, TACKONS(I));
               begin
--TEXT_IO.PUT_LINE("LESS = " & LESS);
                  if LESS  /= INPUT_WORD  then       --  LESS is less

                  --==========================================================
                  --RUN_UNIQUES(INPUT_WORD, UNIQUE_FOUND, PA, PA_LAST);
                  --RUN_INFLECTIONS(LESS, SS);
                  --PRUNE_STEMS(LESS, SS, SSS);
                  --if SSS(1) /= NULL_PARSE_RECORD   then
                  --ORDER_STEMS(SSS);
                  --ARRAY_STEMS(SSS, PA, PA_LAST);
                  --SSS(1) := NULL_PARSE_RECORD;
                  --end if;
                  --==========================================================
                     WORD(LESS, PA, PA_LAST);


--                  TEXT_IO.PUT("In TRY_TACKONS  Left WORD    ");
--                  TEXT_IO.PUT("PA_LAST = "); TEXT_IO.PUT(INTEGER'IMAGE(PA_LAST)); TEXT_IO.PUT("  "); 
--                  TEXT_IO.PUT(TACKONS(I).TACK);
--                  TEXT_IO.NEW_LINE;

                  -----------------------------------------


                     if PA_LAST > ENTERING_PA_LAST  then      --  we have a possible word
 --TEXT_IO.PUT("I = " & INTEGER'IMAGE(I) & "  " & TACKONS(I).TACK & "  TACKONS(I).ENTR.BASE.PART = ");  
 --PART_OF_SPEECH_TYPE_IO.PUT(TACKONS(I).ENTR.BASE.PART); TEXT_IO.NEW_LINE;

                        if TACKONS(I).ENTR.BASE.POFS = X  then          --  on PART (= X?)
                        --PUT("TACKON X found "); PUT( TACKONS(I).TACK); NEW_LINE;
                        --PUT("PA_LAST = "); PUT(PA_LAST); PUT("  "); 
                        --PUT("TACKON MNPC  "); PUT( TACKONS(I).MNPC); NEW_LINE;
                           TACKON_HIT := TRUE;
                        --PUT("TACKON_HIT  = "); PUT(TACKON_HIT); NEW_LINE;  
                           TACKON_ON  := FALSE;

                        else                                            

                           J := PA_LAST;

                           while J >= ENTERING_PA_LAST+1  loop        --  Sweep backwards over PA
                           --  Sweeping up inapplicable fixes, 
                           --  although we only have TACKONs for X or PRON or ADJ - so far
                           --  and there are no fixes for PRON - so far
--TEXT_IO.PUT("J = " & INTEGER'IMAGE(J) & "  PA(J).IR.QUAL = ");  
--QUALITY_RECORD_IO.PUT(PA(J).IR.QUAL); 
--TEXT_IO.NEW_LINE;

                              if ((PA(J).IR.QUAL.POFS = PREFIX) and then (TACKON_ON))  then
                                 null;          --  check PART
                                 TACKON_ON  := FALSE;
                              elsif ((PA(J).IR.QUAL.POFS = SUFFIX) and then (TACKON_ON))  then 
                                --  check PART
                               
                                 null;
                                 TACKON_ON  := FALSE;


                              elsif PA(J).IR.QUAL.POFS = TACKONS(I).ENTR.BASE.POFS  then
                                DICT_IO.SET_INDEX(DICT_FILE(PA(J).D_K), PA(J).MNPC);
                                DICT_IO.READ(DICT_FILE(PA(J).D_K), DE);
                                MEAN := DE.MEAN;
 
--TEXT_IO.PUT("J = " & INTEGER'IMAGE(J) & "  PA(J).IR.QUAL = ");  
--QUALITY_RECORD_IO.PUT(PA(J).IR.QUAL); 
--TEXT_IO.NEW_LINE;
                             --  check PART                              
                                case TACKONS(I).ENTR.BASE.POFS is
                                     when N       =>
                                       if (PA(J).IR.QUAL.N.DECL <=
                                           TACKONS(I).ENTR.BASE.N.DECL)  then
                                         --  Ignore GEN and KIND
                                         TACKON_HIT := TRUE;
                                         TACKON_ON  := TRUE;
                                       end if;
                                      
                                     when PRON    =>              --  Only one we have other than X
                                    --PUT("TACK/PA DECL "); PUT(PA(J).IR.QUAL.PRON.DECL); PUT("  -  "); 
                                    --PUT(TACKONS(I).ENTR.BASE.PRON.DECL); NEW_LINE;
                                    --PUT("TACK/PA KIND "); PUT(PA(J).IR.QUAL.PRON.KIND); PUT("  -  "); 
                                    --PUT(TACKONS(I).ENTR.BASE.PRON.KIND); NEW_LINE;
                                       if PA(J).IR.QUAL.PRON.DECL <=
                                       TACKONS(I).ENTR.BASE.PRON.DECL  --and then
                                       --PA(J).IR.QUAL.PRON.KIND <=
                                       --TACKONS(I).ENTR.BASE.PRON.KIND  
                                           then
                                       --PUT("TACKON PRON found HIT "); PUT( TACKONS(I).TACK); NEW_LINE;
                                          TACKON_HIT := TRUE;
                                          TACKON_ON  := TRUE;
                                       else
                                          PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
                                          PA_LAST := PA_LAST - 1;
                                          
                                       end if;     
                                       
                                     when ADJ     =>           
                                        --  Forego all checks, even on DECL of ADJ
                                        --  -cumque is the only one I have now
                                      --  if  .......
                                          TACKON_HIT := TRUE;
                                          TACKON_ON  := TRUE;
                                      --  else
                                      --    PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
                                      --    PA_LAST := PA_LAST - 1;
                                      --  end if;     
                                      
                                 --when ADV     =>
                                 --when V       =>
                                    when others  =>
                                       PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
                                       PA_LAST := PA_LAST - 1;

                                 end case;

                              else                                          --  check PART
                                 PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
                                 PA_LAST := PA_LAST - 1;
                              --PUT("J failed  J & PA_LAST = "); PUT(J); PUT("  "); PUT(PA_LAST); NEW_LINE;
                              end if;                                      --  check PART
                              J := J - 1;
                           end loop;                          --  loop sweep over PA

                        end if;                                      --  on PART (= X?)
                     --PUT_LINE("End if on PART = X ?");


                     -----------------------------------------
                        if TACKON_HIT  then
                        --PUT("Where it counts TACKON_HIT  = "); PUT(TACKON_HIT); NEW_LINE;  
                        --  Put on TACKON
                      
                     
                     
                    
                     
                          PA_LAST := PA_LAST + 1;
                           PA(ENTERING_PA_LAST+2..PA_LAST) :=
                              PA(ENTERING_PA_LAST+1..PA_LAST-1);
                           PA(ENTERING_PA_LAST+1) := (TACKONS(I).TACK,
                                                         ((TACKON, NULL_TACKON_RECORD), 0,
                                                          NULL_ENDING_RECORD, X, X),
                                                      ADDONS,
                                                         DICT_IO.COUNT((TACKONS(I).MNPC)));
                        --PUT("PA_LAST = "); PUT(PA_LAST); PUT("  "); 
                        --PUT("I = "); PUT(I); PUT("  TACKONS(I).TACK = "); PUT(TACKONS(I).TACK);
                        --PUT_LINE("TACKON added");
                        --PUT_LINE("Now list the PA array after adding the found TACKON");
                        --for K in 1..PA_LAST  loop
                        --PUT("K = "); PUT(K); PUT("  PA(K)  "); PUT(PA(K).D_K); PUT(PA(K).IR); NEW_LINE;
                        --end loop;
                           return;                 --  Be happy with one ???????
                        else
                           null;
                        --PUT("No TACKON_HIT, so no punitive TACKON   PA_LAST is  "); 
                        --PUT(PA_LAST); NEW_LINE;
                        end if;   --  TACKON_HIT

                     end if;                             --  we have a possible word
                  -----------------------------------------
                  end if;                                     --  LESS is less
               end REMOVE_A_TACKON;
            end loop LOOP_OVER_TACKONS;
         --PUT_LINE("LEAVING TACKONS   *******************************************  ");  
         end TRY_TACKONS;


      begin                           --  WORD
--TEXT_IO.PUT_LINE("Starting WORD  INPUT = " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
         if TRIM(INPUT_WORD) = ""  then
            return;
         end if;


         RUN_UNIQUES(INPUT_WORD, UNIQUE_FOUND, PA, PA_LAST);     

         
      --if INPUT_WORD(INPUT_WORD'FIRST) in 'a'..'z'  then
      --      CONSTRUCT_STEMS(INPUT_WORD, 
      --      SEARCH_DICTIONARIES();
      --      TRY_STEMS_AGAINST_INFLECTIONS;
      --    end if;

      --if INPUT_WORD(INPUT_WORD'FIRST) in 'a'..'z'  then
--TEXT_IO.PUT_LINE("After UNIQUES  INPUT = " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

         
         QU:
         declare
            PA_QSTART : INTEGER := PA_LAST;
            PA_START : INTEGER := PA_LAST;
            SAVED_MODE_ARRAY : MODE_ARRAY := WORDS_MODE;
            QKEY : STEM_KEY_TYPE := 0;


         begin       --  QU
            TICKONS(NUMBER_OF_TICKONS+1) := NULL_PREFIX_ITEM;
            WORDS_MODE  := (others => FALSE);

            for I in 1..NUMBER_OF_TICKONS+1  loop
               declare
                  Q_WORD : constant STRING :=  TRIM(SUBTRACT_TICKON(INPUT_WORD, TICKONS(I)));
               begin
                  PA_LAST := PA_QSTART;
                  PA(PA_LAST+1) := NULL_PARSE_RECORD;
                  if (I = NUMBER_OF_TICKONS + 1)   or else  --  The prefix is a TICKON
                     (Q_WORD /= INPUT_WORD) then            --  and it matches the start of INPUT_WORD

                     if I <= NUMBER_OF_TICKONS  then        --  Add to PA if 
--TEXT_IO.PUT_LINE("ADDING TICKON PA    " & TICKONS(I).FIX);
                        PA_LAST := PA_LAST + 1;        --  So add prefix line to parse array
                        PA(PA_LAST).STEM := HEAD(TICKONS(I).FIX, MAX_STEM_SIZE);
                        PA(PA_LAST).IR := ((PREFIX, NULL_PREFIX_RECORD), 0, NULL_ENDING_RECORD, X, X);
                        PA(PA_LAST).D_K  := ADDONS;
                        PA(PA_LAST).MNPC := DICT_IO.COUNT(TICKONS(I).MNPC);
                     end if;
                     


                     if Q_WORD'LENGTH >= 3   and then   --  qui is shortest QU_PRON
                        ((Q_WORD(Q_WORD'FIRST..Q_WORD'FIRST+1) = "qu")  or
                         (Q_WORD(Q_WORD'FIRST..Q_WORD'FIRST+1) = "cu"))  then
                       if Q_WORD(Q_WORD'FIRST..Q_WORD'FIRST+1) = "qu"  then
                         QKEY := 1;
                         PROCESS_QU_PRONOUNS(Q_WORD, QKEY);
                       elsif Q_WORD(Q_WORD'FIRST..Q_WORD'FIRST+1) = "cu"  then
                         QKEY := 2;
                         PROCESS_QU_PRONOUNS(Q_WORD, QKEY);
                       end if;
                       if PA_LAST <= PA_QSTART + 1  and then
                         QKEY > 0                    then    --  If did not find a PACKON
                         if Q_WORD(Q_WORD'FIRST..Q_WORD'FIRST+1) = "qu"  then
                           PROCESS_PACKONS(Q_WORD, QKEY);
                         elsif Q_WORD(Q_WORD'FIRST..Q_WORD'FIRST+1) = "cu"  then
                           PROCESS_PACKONS(Q_WORD, QKEY);
                         end if;
                       else
                         exit;
                       end if;
                       if PA_LAST > PA_QSTART + 1  then
                         exit; 
                       end if;


                     elsif INPUT_WORD'LENGTH >= 6  then   --  aliqui as aliQU_PRON
                       if INPUT_WORD(INPUT_WORD'FIRST..INPUT_WORD'FIRST+4) = "aliqu"  then
                         PROCESS_QU_PRONOUNS(INPUT_WORD, 1);
                       elsif INPUT_WORD(INPUT_WORD'FIRST..INPUT_WORD'FIRST+4) = "alicu"  then
                         PROCESS_QU_PRONOUNS(INPUT_WORD, 2);
  
                       end if;
                     
                     end if;
                        
                      

                  if PA_LAST = PA_START + 1  then    --  Nothing found
                        PA_LAST := PA_START;             --  Reset PA_LAST
                     else
                        exit;
                     end if;
                     

                  end if;
               end;
            end loop;

            WORDS_MODE := SAVED_MODE_ARRAY;



            exception
               when others =>
                  WORDS_MODE := SAVED_MODE_ARRAY;
         end QU;

      --==========================================================
         RUN_INFLECTIONS(INPUT_WORD, SS);
         PRUNE_STEMS(INPUT_WORD, SS, SSS);
--TEXT_IO.PUT_LINE("After PRUNE  INPUT = " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
         if SSS(1) /= NULL_PARSE_RECORD   then
            ORDER_STEMS(SSS);
            ARRAY_STEMS(SSS, PA, PA_LAST);
            SSS(1) := NULL_PARSE_RECORD;
         end if;
--TEXT_IO.PUT_LINE("After ARRAY  INPUT = " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
      --==========================================================




         if PA_LAST = PA_SAVE  then
            TRY_TACKONS(INPUT_WORD);
         end if;


--TEXT_IO.PUT_LINE("Out WORD  INPUT = " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

         --TEXT_IO.SET_OUTPUT(TEXT_IO.STANDARD_OUTPUT);

         exception
            when STORAGE_ERROR =>
               TEXT_IO.PUT_LINE(TEXT_IO.STANDARD_OUTPUT,
                       "STORAGE_ERROR exception in WORD while processing =>"
                       & RAW_WORD);
               PA_LAST := PA_SAVE;
               if WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
                  TEXT_IO.PUT(UNKNOWNS, RAW_WORD);
                  TEXT_IO.SET_COL(UNKNOWNS, 21);
                  TEXT_IO.PUT_LINE(UNKNOWNS, "========   STORAGE_ERROR  ");
               end if;
            when others =>
               if WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
                  TEXT_IO.PUT(UNKNOWNS, RAW_WORD);
                  TEXT_IO.SET_COL(UNKNOWNS, 21);
                  TEXT_IO.PUT_LINE(UNKNOWNS, "========   ERROR  ");
               end if;
               PA_LAST := PA_SAVE;
      end WORD;


      procedure INITIALIZE_WORD_PACKAGE is
      begin                                  --  Initializing WORD_PACKAGE


         ESTABLISH_INFLECTIONS_SECTION;

         LEL_SECTION_IO.OPEN(INFLECTIONS_SECTIONS_FILE, LEL_SECTION_IO.IN_FILE,
                             INFLECTIONS_SECTIONS_NAME);


         TRY_TO_LOAD_DICTIONARY(GENERAL);

         TRY_TO_LOAD_DICTIONARY(SPECIAL);


         LOAD_LOCAL:
         begin
         --  First check if there is a LOC dictionary
            CHECK_FOR_LOCAL_DICTIONARY:
            declare
               DUMMY : TEXT_IO.FILE_TYPE;
            begin
               TEXT_IO.OPEN(DUMMY, TEXT_IO.IN_FILE,
                            ADD_FILE_NAME_EXTENSION(DICTIONARY_FILE_NAME,
                                                    "LOCAL"));
            --  Failure to OPEN will raise an exception, to be handled below
               TEXT_IO.CLOSE(DUMMY);
            end CHECK_FOR_LOCAL_DICTIONARY;
         --  If the above does not exception out, we can load LOC
            PREFACE.PUT("LOCAL ");
            DICT_LOC := NULL_DICTIONARY;
            LOAD_DICTIONARY(DICT_LOC,
                            ADD_FILE_NAME_EXTENSION(DICTIONARY_FILE_NAME, "LOCAL"));
         --  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
            LOAD_STEM_FILE(LOCAL);
            DICTIONARY_AVAILABLE(LOCAL) := TRUE;
            exception
               when others  =>
                  DICTIONARY_AVAILABLE(LOCAL) := FALSE;
         end LOAD_LOCAL;
     
         LOAD_UNIQUES(UNQ, UNIQUES_FULL_NAME);

         LOAD_ADDONS(ADDONS_FULL_NAME);
--TEXT_IO.PUT_LINE("Loaded ADDONS");
         LOAD_BDL_FROM_DISK;
--TEXT_IO.PUT_LINE("BDL loaded");
         if not (DICTIONARY_AVAILABLE(GENERAL)  or
                 DICTIONARY_AVAILABLE(SPECIAL)  or
                 DICTIONARY_AVAILABLE(LOCAL))  then
            PREFACE.PUT_LINE("There are no main dictionaries - program will not do much");
            PREFACE.PUT_LINE("Check that there are dictionary files in this subdirectory");
            PREFACE.PUT_LINE("Except DICT.LOC that means DICTFILE, INDXFILE, STEMFILE");
         end if;

--TEXT_IO.PUT_LINE("Ready to load English");

         TRY_TO_LOAD_ENGLISH_WORDS:
         begin 
           ENGLISH_DICTIONARY_AVAILABLE(GENERAL) := FALSE;
           EWDS_DIRECT_IO.OPEN(EWDS_FILE, EWDS_DIRECT_IO.IN_FILE, "EWDSFILE.GEN");
  
           ENGLISH_DICTIONARY_AVAILABLE(GENERAL) := TRUE;
     
         exception
            when others  =>
               PREFACE.PUT_LINE("No English available");
               ENGLISH_DICTIONARY_AVAILABLE(GENERAL) := FALSE;
         end TRY_TO_LOAD_ENGLISH_WORDS;
         
         
      
         --put_line("WORD_PACKAGE INITIALIZED");
      end INITIALIZE_WORD_PACKAGE;

   end WORD_PACKAGE;
