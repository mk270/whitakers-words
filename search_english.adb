  with TEXT_IO; use TEXT_IO;
  with Strings_Package; use Strings_Package;  
  with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
  with CONFIG;
  with WORD_PARAMETERS; use WORD_PARAMETERS;
  with Inflections_Package; use Inflections_Package;
  with Dictionary_Package; use Dictionary_Package;
  with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
  with WORD_PACKAGE; use WORD_PACKAGE;
  with ENGLISH_SUPPORT_PACKAGE; use ENGLISH_SUPPORT_PACKAGE;
  with DICTIONARY_FORM;
    
 procedure SEARCH_ENGLISH(INPUT_ENGLISH_WORD : STRING; POFS : PART_OF_SPEECH_TYPE := X) is
     use EWDS_DIRECT_IO;
     INPUT_WORD : EWORD := LOWER_CASE(HEAD(INPUT_ENGLISH_WORD, EWORD_SIZE));
     INPUT_POFS : PART_OF_SPEECH_TYPE := POFS; 
       
     OUTPUT_ARRAY : EWDS_ARRAY(1..500) := (others => NULL_EWDS_RECORD);
     NUMBER_OF_HITS : INTEGER := 0;
     J1, J2, J, JJ : EWDS_DIRECT_IO.COUNT := 0;
     
     D_K : DICTIONARY_KIND := GENERAL;    --  For the moment
     
     EWDS : EWDS_RECORD := NULL_EWDS_RECORD;
     
     FIRST_TRY, SECOND_TRY : BOOLEAN := TRUE;
 
     procedure LOAD_OUTPUT_ARRAY(EWDS : in EWDS_RECORD) is
     begin
 --PUT("LOAD a  " & PART_OF_SPEECH_TYPE'IMAGE(INPUT_POFS));
 --PUT("LOAD b  " & PART_OF_SPEECH_TYPE'IMAGE(INPUT_POFS));
       if EWDS.POFS <= INPUT_POFS  then
         NUMBER_OF_HITS := NUMBER_OF_HITS + 1;
         OUTPUT_ARRAY(NUMBER_OF_HITS) := EWDS;
--  PUT("$  " & INTEGER'IMAGE(NUMBER_OF_HITS));
--         EWDS_RECORD_IO.PUT(OUTPUT_ARRAY(NUMBER_OF_HITS));
--         TEXT_IO.NEW_LINE;
       end if;
     end LOAD_OUTPUT_ARRAY;
      
     --procedure TRIM_OUTPUT_ARRAY is
       procedure SORT_OUTPUT_ARRAY is
         HITS : INTEGER := 0;
       begin
         
    --  Bubble sort     
    HIT_LOOP:
    loop
      HITS := 0;

      SWITCH:
      declare
        DW, EW : EWDS_RECORD := NULL_EWDS_RECORD;
   
      begin
          INNER_LOOP:    --  Order by RANK, FREQ, SEMI
          for I in 1..NUMBER_OF_HITS-1  loop
             if OUTPUT_ARRAY(I+1).RANK  >  OUTPUT_ARRAY(I).RANK     or else  

               (OUTPUT_ARRAY(I+1).RANK  =  OUTPUT_ARRAY(I).RANK     and then
                OUTPUT_ARRAY(I+1).FREQ  <  OUTPUT_ARRAY(I).FREQ)  or else  
                  
               (OUTPUT_ARRAY(I+1).RANK  =  OUTPUT_ARRAY(I).RANK     and then
                OUTPUT_ARRAY(I+1).FREQ  =  OUTPUT_ARRAY(I).FREQ   and then
                OUTPUT_ARRAY(I+1).SEMI  <  OUTPUT_ARRAY(I).SEMI)          then
                  
               DW := OUTPUT_ARRAY(I);
               OUTPUT_ARRAY(I) := OUTPUT_ARRAY(I+1);
               OUTPUT_ARRAY(I+1) := DW;
               HITS := HITS + 1;
     --PUT_LINE("HITS    " & INTEGER'IMAGE(HITS));
            end if;
         end loop INNER_LOOP;

        end SWITCH;
        exit when HITS = 0;
      end loop HIT_LOOP;
         
     end SORT_OUTPUT_ARRAY;
       
--     begin
--       SORT_OUTPUT_ARRAY;
--      end TRIM_OUTPUT_ARRAY;
       
     procedure DUMP_OUTPUT_ARRAY(OUTPUT : in TEXT_IO.FILE_TYPE) is
       DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
       NUMBER_TO_SHOW : INTEGER := NUMBER_OF_HITS;
       ONE_SCREEN : INTEGER := 6;
     begin
  --TEXT_IO.PUT_LINE("DUMP_OUTPUT");             
      if NUMBER_OF_HITS = 0  then
         TEXT_IO.PUT_LINE(OUTPUT, "No Match");             
       else
--PUT_LINE("Unsorted EWDS");
--for I in 1..NUMBER_TO_SHOW  loop
--  PUT(INTEGER'IMAGE(I)); PUT("*"); EWDS_RECORD_IO.PUT(OUTPUT_ARRAY(I)); NEW_LINE;
--end loop;
       
SORT_OUTPUT_ARRAY;

--TEXT_IO.PUT_LINE("DUMP_OUTPUT SORTED");             
       
         TRIMMED := FALSE;
         if WORDS_MODE(TRIM_OUTPUT)  then
           if NUMBER_OF_HITS > ONE_SCREEN  then
             NUMBER_TO_SHOW := ONE_SCREEN;    
             TRIMMED := TRUE;
           else 
             NUMBER_TO_SHOW := NUMBER_OF_HITS;
           end if;
         end if;   
         
         for I in 1..NUMBER_TO_SHOW  loop
           TEXT_IO.NEW_LINE(OUTPUT);
              
         DO_PAUSE:
         begin
--PUT(INTEGER'IMAGE(INTEGER(TEXT_IO.LINE(OUTPUT))) & " ");
--PUT(INTEGER'IMAGE(INTEGER(SCROLL_LINE_NUMBER)) & " ");
--PUT(INTEGER'IMAGE(INTEGER(CONFIG.OUTPUT_SCREEN_SIZE)) & " ");
           if (INTEGER(TEXT_IO.LINE(OUTPUT)) >
                   SCROLL_LINE_NUMBER + CONFIG.OUTPUT_SCREEN_SIZE)  then
              PAUSE(OUTPUT);
              SCROLL_LINE_NUMBER := INTEGER(TEXT_IO.LINE(OUTPUT));
            end if;
          end DO_PAUSE;                                                                                     

         
        
--         EWDS_RECORD_IO.PUT(OUTPUT_ARRAY(I));
--         TEXT_IO.NEW_LINE;
DICT_IO.READ(DICT_FILE(GENERAL), DE, DICT_IO.COUNT(OUTPUT_ARRAY(I).N));

--TEXT_IO.PUT_LINE("DUMP_OUTPUT READ");             
--  DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;

            PUT(OUTPUT, DICTIONARY_FORM(DE));
            TEXT_IO.PUT(OUTPUT, "   ");
            --PART_ENTRY_IO.PUT(OUTPUT, DE.PART);
--TEXT_IO.PUT_LINE("DUMP_OUTPUT PART");             
            if DE.PART.POFS = N  then
               TEXT_IO.PUT(OUTPUT, "  ");  DECN_RECORD_IO.PUT(OUTPUT, DE.PART.N.DECL);
               TEXT_IO.PUT(OUTPUT, "  " & GENDER_TYPE'IMAGE(DE.PART.N.GENDER) & "  ");
             end if;
             if (DE.PART.POFS = V)   then
               TEXT_IO.PUT(OUTPUT, "  ");  DECN_RECORD_IO.PUT(OUTPUT, DE.PART.V.CON);
             end if;
             if (DE.PART.POFS = V)  and then  (DE.PART.V.KIND in GEN..PERFDEF)  then
               TEXT_IO.PUT(OUTPUT, "  " & VERB_KIND_TYPE'IMAGE(DE.PART.V.KIND) & "  ");
             end if;
      
 --TEXT_IO.PUT_LINE("DUMP_OUTPUT CODE");             

 
       if WORDS_MDEV(SHOW_DICTIONARY_CODES)    then
                 TEXT_IO.PUT(OUTPUT, " [");
                 AGE_TYPE_IO.PUT(OUTPUT, DE.TRAN.AGE);
                 AREA_TYPE_IO.PUT(OUTPUT, DE.TRAN.AREA);
                 GEO_TYPE_IO.PUT(OUTPUT, DE.TRAN.GEO);
                 FREQUENCY_TYPE_IO.PUT(OUTPUT, DE.TRAN.FREQ);
                 SOURCE_TYPE_IO.PUT(OUTPUT, DE.TRAN.SOURCE);
                 TEXT_IO.PUT(OUTPUT, "]  ");
             end if;
               
            if WORDS_MDEV(SHOW_DICTIONARY) then
                 TEXT_IO.PUT(OUTPUT, EXT(D_K) & ">");
              end if;
--TEXT_IO.PUT_LINE("DUMP_OUTPUT SHOW");             
            
            if WORDS_MDEV(SHOW_DICTIONARY_LINE)  then
                   TEXT_IO.PUT(OUTPUT, "(" 
                         & TRIM(INTEGER'IMAGE(OUTPUT_ARRAY(I).N)) & ")");
               end if;
                  
                 TEXT_IO.NEW_LINE(OUTPUT);
                 
--TEXT_IO.PUT_LINE("DUMP_OUTPUT MEAN");      
       
           TEXT_IO.PUT(OUTPUT, TRIM(DE.MEAN));
           TEXT_IO.NEW_LINE(OUTPUT);
         
      
               
       end loop;
 --TEXT_IO.PUT_LINE("DUMP_OUTPUT TRIMMED"); 
               
      if TRIMMED  then
         PUT_LINE(OUTPUT, "*");
       end if;
       
       end if;    --  On HITS = 0
       
     exception
           when others =>
             null;   --  If N not in DICT_FILE
     end DUMP_OUTPUT_ARRAY;
    
   begin
            
            J1 := 1;
            J2 := SIZE(EWDS_FILE);
     
                     FIRST_TRY := TRUE;

                     SECOND_TRY := TRUE;

                     J := (J1 + J2) / 2;

                  BINARY_SEARCH:
                     loop
--   TEXT_IO.PUT_LINE("J = " & INTEGER'IMAGE(INTEGER(J)));
   
                        if (J1 = J2-1) or (J1 = J2) then
                           if FIRST_TRY  then
--   TEXT_IO.PUT_LINE("FIRST_TRY");
                              J := J1;
                              FIRST_TRY := FALSE;
                           elsif SECOND_TRY  then
--   TEXT_IO.PUT_LINE("SECOND_TRY");
                              J := J2;
                              SECOND_TRY := FALSE;
                           else
--   TEXT_IO.PUT_LINE("THIRD_TRY   exit BINARY_SEARCH");
                              JJ := J;
                              exit BINARY_SEARCH;
                           end if;
                        end if;
                                            
                           --  Should D_K
                        SET_INDEX(EWDS_FILE, EWDS_DIRECT_IO.COUNT(J));
                        READ(EWDS_FILE, EWDS);
--   EWDS_RECORD_IO.PUT(EWDS);
--   TEXT_IO.NEW_LINE;
--   PUT_LINE(LOWER_CASE(EWDS.W));
--   PUT_LINE(INPUT_WORD);
--   TEXT_IO.PUT_LINE("J = " & INTEGER'IMAGE(INTEGER(J)) &
--                  "   J1 = " & INTEGER'IMAGE(INTEGER(J1)) &
--                 "   J2 = " & INTEGER'IMAGE(INTEGER(J2)));
--                  
                        if  "<"(LOWER_CASE(EWDS.W), INPUT_WORD)  then  --  Not LTU, not u=v
                           J1 := J;
                           J := (J1 + J2) / 2;
                        elsif  ">"(LOWER_CASE(EWDS.W), INPUT_WORD)  then
                           J2 := J;
                           J := (J1 + J2) / 2;
                        else
                          for I in reverse J1..J  loop
                             SET_INDEX(EWDS_FILE, EWDS_DIRECT_IO.COUNT(I));
                             READ(EWDS_FILE, EWDS);    --  Reads and advances index!!
                        
                              if "="(LOWER_CASE(EWDS.W), INPUT_WORD)  then
                                 JJ := I;
--      PUT(INTEGER'IMAGE(INTEGER(I))); PUT("-"); EWDS_RECORD_IO.PUT(EWDS); NEW_LINE;
   LOAD_OUTPUT_ARRAY(EWDS);

                              else
                                 exit;
                              end if;
                           end loop;

                           for I in J+1..J2  loop
                        SET_INDEX(EWDS_FILE, EWDS_DIRECT_IO.COUNT(I));
                        READ(EWDS_FILE, EWDS);
                        
                              if "="(LOWER_CASE(EWDS.W), INPUT_WORD)  then
                                JJ := I;

--        PUT(INTEGER'IMAGE(INTEGER(I))); PUT("+"); EWDS_RECORD_IO.PUT(EWDS);  NEW_LINE;
   LOAD_OUTPUT_ARRAY(EWDS);

                              else
                                 exit BINARY_SEARCH;
                              end if;
                           end loop;
                           exit BINARY_SEARCH;

                        end if;
                     end loop BINARY_SEARCH;
                          
          
                    
          if  WORDS_MODE(WRITE_OUTPUT_TO_FILE)      then
            DUMP_OUTPUT_ARRAY(OUTPUT);
          else
            DUMP_OUTPUT_ARRAY(CURRENT_OUTPUT);
          end if;

         
         -- DUMP_OUTPUT_ARRAY(;
           
--      TEXT_IO.PUT_LINE("Leaving SEARCH NUMBER_OF_HITS = " & 
--        INTEGER'IMAGE(NUMBER_OF_HITS));
          exception
            when others  =>
      TEXT_IO.PUT_LINE("exception SEARCH NUMBER_OF_HITS = " & 
        INTEGER'IMAGE(NUMBER_OF_HITS));
              raise;       
      end SEARCH_ENGLISH;





