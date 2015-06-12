   with TEXT_IO; 
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure WAKEDICT is 
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use TEXT_IO;
      use STEM_KEY_TYPE_IO;
      use DICTIONARY_ENTRY_IO;
      use PART_ENTRY_IO;
      use KIND_ENTRY_IO;
      use TRANSLATION_RECORD_IO;
      use AGE_TYPE_IO;
      use AREA_TYPE_IO;
      use GEO_TYPE_IO;
      use FREQUENCY_TYPE_IO;
      use SOURCE_TYPE_IO;
      use DICT_IO;
   
      PORTING : constant BOOLEAN := FALSE;
   
      BE_VE : VERB_ENTRY := (CON => (5, 1), KIND => TO_BE);
   
      D_K : DICTIONARY_KIND := XXX;       --  ######################
   
   
      START_STEM_1  : constant := 1;
      START_STEM_2  : constant := START_STEM_1 + MAX_STEM_SIZE + 1;
      START_STEM_3  : constant := START_STEM_2 + MAX_STEM_SIZE + 1;
      START_STEM_4  : constant := START_STEM_3 + MAX_STEM_SIZE + 1;
      START_PART    : constant := START_STEM_4 + MAX_STEM_SIZE + 1;
      START_TRAN    : constant INTEGER := 
         START_PART + 
         INTEGER(PART_ENTRY_IO.DEFAULT_WIDTH + 1);
      FINISH_LINE   : constant INTEGER := 
         START_TRAN +
         TRANSLATION_RECORD_IO.DEFAULT_WIDTH - 1;
   
   
   
   
   
      DICTFILE : DICT_IO.FILE_TYPE;
      INPUT, STEMLIST : TEXT_IO.FILE_TYPE;
      DE : DICTIONARY_ENTRY;
   
      S, LINE, BLANK_LINE : STRING(1..400) := (others => ' ');
      L, LL, LAST : INTEGER := 0;
      J : DICT_IO.COUNT := 0;
      MEAN_TO_BE : constant MEANING_TYPE := 
         HEAD("be; exist; (also used to form verb perfect passive tenses)" &
              " with NOM PERF PPL", MAX_MEANING_SIZE);
   
   begin
      PUT_LINE(
              "Takes a DICTLINE.D_K and produces a STEMLIST.D_K and DICTFILE.D_K");
      PUT_LINE("This version inserts ESSE when D_K = GEN");
      PUT("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
      GET_LINE(LINE, LAST);
      if LAST > 0  then
         if TRIM(LINE(1..LAST))(1) = 'G'  or else
         TRIM(LINE(1..LAST))(1) = 'g'     then
            D_K := GENERAL;
         elsif TRIM(LINE(1..LAST))(1) = 'S'  or else
         TRIM(LINE(1..LAST))(1) = 's'     then
            D_K := SPECIAL;
         else
            PUT_LINE("No such dictionary");
            raise TEXT_IO.DATA_ERROR;
         end if; 
      end if;
   
   
      OPEN(INPUT, IN_FILE, ADD_FILE_NAME_EXTENSION(DICT_LINE_NAME, 
                                                DICTIONARY_KIND'IMAGE(D_K))); 
   
      if not PORTING  then
      
         CREATE(STEMLIST, OUT_FILE, ADD_FILE_NAME_EXTENSION(STEM_LIST_NAME, 
                                                    DICTIONARY_KIND'IMAGE(D_K)));
      end if;
   
      CREATE(DICTFILE, OUT_FILE, ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, 
                                                 DICTIONARY_KIND'IMAGE(D_K)));
   
--      if D_K = GENERAL  then
--         PUT_LINE("WAKEDICT reads DICTLINE.d_k and produces DICTFILE.d_k");
--         PUT_LINE("WAKEDICT also produces STEMLIST.d_k");
--         PUT_LINE("This version inserts ESSE when d_k = GEN");
--      
--         J := J + 1;
--      
--      --  First construct ESSE
--         DE.STEMS(1) := "s                 ";
--         DE.STEMS(2) := "                  ";
--         DE.STEMS(3) := "fu                ";
--         DE.STEMS(4) := "fut               ";
--      --DE.PART := (PART => V,  CON => (5, 10));
--      --DE.PART := (V, ((5, 1)));
--         DE.PART := (V, BE_VE);
--         DE.KIND := (V, TO_BE);
--         DE.TRAN := (X, X, X, A, X);
--         DE.MEAN := MEAN_TO_BE;
--      
--      
--         if not PORTING  then
--         --  Load ESSE
--            for I in STEM_KEY_TYPE range 1..4  loop
--               PUT(STEMLIST, DE.STEMS(I)); PUT(STEMLIST, ' ');
--               PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
--               SET_COL(STEMLIST, 45);
--               PUT(STEMLIST, I, 2); PUT(STEMLIST, ' ');
--            --      PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
--            --      PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
--            --      PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
--            --      PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
--            --      PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
--               SET_COL(STEMLIST, 50);
--               INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
--            end loop;
--         end if;
--      
--         WRITE(DICTFILE, DE, J);        --  J = 1
--      end if;
--   
   
    --  Now do the rest 
   OVER_LINES:
      while not END_OF_FILE(INPUT) loop
         S := BLANK_LINE;
         GET_LINE(INPUT, S, LAST);
         if TRIM(S(1..LAST)) /= ""  then
            L := 0;
         
            FORM_DE:
            begin
            
               DE.STEMS(1) := S(START_STEM_1..MAX_STEM_SIZE);
            --NEW_LINE; PUT(DE.STEMS(1));
               DE.STEMS(2) := S(START_STEM_2..START_STEM_2+MAX_STEM_SIZE-1);
               DE.STEMS(3) := S(START_STEM_3..START_STEM_3+MAX_STEM_SIZE-1);
               DE.STEMS(4) := S(START_STEM_4..START_STEM_4+MAX_STEM_SIZE-1);
            --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('@'); 
               GET(S(START_PART..LAST), DE.PART, L);
            --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('&'); PUT(S(L+1..LAST)); PUT('3'); 
              -- GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
               GET(S(L+1..LAST), DE.TRAN.AGE, L);
               GET(S(L+1..LAST), DE.TRAN.AREA, L);
               GET(S(L+1..LAST), DE.TRAN.GEO, L);
               GET(S(L+1..LAST), DE.TRAN.FREQ, L);
               GET(S(L+1..LAST), DE.TRAN.SOURCE, L);
               DE.MEAN := HEAD(S(L+2..LAST), MAX_MEANING_SIZE);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING, not ENUM
            
               exception
                  when others =>
                     NEW_LINE;
                     PUT_LINE("Exception");
                     PUT_LINE(S(1..LAST));
                     INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
                     PUT(DE); NEW_LINE;
            end FORM_DE;
         
            J := J + 1;
            WRITE(DICTFILE, DE, J);
         
         
            if not PORTING  then
            
               if DE.PART.POFS = N    and then
               DE.STEMS(1) = DE.STEMS(2)     and then
               DE.STEMS(1) /= ZZZ_STEM       then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 0, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               --        if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
               --          PUT(STEMLIST, DE.STEMS(3)); PUT(STEMLIST, ' ');
               --          PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
               --          SET_COL(STEMLIST, 45);
               --          INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
               ----          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               ----          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               ----          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               ----          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               ----          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
               --          INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               --        end if;
               --        if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
               --          PUT(STEMLIST, DE.STEMS(4)); PUT(STEMLIST, ' ');
               --          PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
               --          SET_COL(STEMLIST, 45);
               --          INTEGER_IO.PUT(STEMLIST, 4, 2); PUT(STEMLIST, ' ');
               --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
               --          INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               --        end if;
               elsif DE.PART.POFS = ADJ  and then
               DE.STEMS(1) = DE.STEMS(2)     and then
               DE.STEMS(1) /= ZZZ_STEM       then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 0, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                  if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
                     PUT(STEMLIST, DE.STEMS(3)); PUT(STEMLIST, ' ');
                     PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 45);
                     INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 50);
                     INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                  end if;
                  if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
                     PUT(STEMLIST, DE.STEMS(4)); PUT(STEMLIST, ' ');
                     PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 45);
                     INTEGER_IO.PUT(STEMLIST, 4, 2); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 50);
                     INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                  end if;
               elsif DE.PART.POFS = ADJ  and then
               --  POS taken care of by position
               DE.PART.ADJ.CO = COMP   then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = ADJ  and then
               DE.PART.ADJ.CO = SUPER  then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 4, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = ADV  and then
               --  POS taken care of by position
               DE.PART.ADV.CO = COMP   then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 2, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = ADV  and then
               DE.PART.ADV.CO = SUPER  then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = V    and then
               DE.STEMS(1) = DE.STEMS(2)     and then
               DE.STEMS(1) /= ZZZ_STEM       then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 0, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                  if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
                     PUT(STEMLIST, DE.STEMS(3)); PUT(STEMLIST, ' ');
                     PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 45);
                     INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 50);
                     INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                  end if;
                  if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
                     PUT(STEMLIST, DE.STEMS(4)); PUT(STEMLIST, ' ');
                     PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 45);
                     INTEGER_IO.PUT(STEMLIST, 4, 2); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
                  --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                     SET_COL(STEMLIST, 50);
                     INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                  end if;
               elsif DE.PART.POFS = NUM  and then
               DE.PART.NUM.SORT = CARD   then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 1, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = NUM  and then
               DE.PART.NUM.SORT = ORD    then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 2, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = NUM  and then
               DE.PART.NUM.SORT = DIST   then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               elsif DE.PART.POFS = NUM  and then
               DE.PART.NUM.SORT = ADVERB  then
                  PUT(STEMLIST, DE.STEMS(1)); PUT(STEMLIST, ' ');
                  PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 45);
                  INTEGER_IO.PUT(STEMLIST, 4, 2); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
               --        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                  SET_COL(STEMLIST, 50);
                  INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
               else
                  for I in STEM_KEY_TYPE range 1..4  loop
                     if DE.STEMS(I) /= ZZZ_STEM  and 
                     DE.STEMS(I) /= NULL_STEM_TYPE  then
                        PUT(STEMLIST, DE.STEMS(I)); PUT(STEMLIST, ' ');
                        PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
                        SET_COL(STEMLIST, 45);
                        PUT(STEMLIST, I, 2); PUT(STEMLIST, ' ');
                     --            PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
                     --            PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
                     --            PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
                     --            PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
                     --            PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
                        SET_COL(STEMLIST, 50);
                        INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
                     end if;
                  end loop;
               end if;
            end if;   --  PORTING
         end if;
      end loop OVER_LINES;
   
      
      
      
      
      
      
      
      
          if D_K = GENERAL  then
         
      
         J := J + 1;
      
      --  First construct ESSE
         DE.STEMS(1) := "s                 ";
         DE.STEMS(2) := "                  ";
         DE.STEMS(3) := "fu                ";
         DE.STEMS(4) := "fut               ";
      --DE.PART := (PART => V,  CON => (5, 10));
      --DE.PART := (V, ((5, 1)));
         DE.PART := (V, BE_VE);
         --DE.KIND := (V, TO_BE);
         DE.TRAN := (X, X, X, A, X);
         DE.MEAN := MEAN_TO_BE;
      
      
         if not PORTING  then
         --  Load ESSE
            for I in STEM_KEY_TYPE range 1..4  loop
               PUT(STEMLIST, DE.STEMS(I)); PUT(STEMLIST, ' ');
               PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
               SET_COL(STEMLIST, 45);
               PUT(STEMLIST, I, 2); PUT(STEMLIST, ' ');
            --      PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
            --      PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
            --      PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
            --      PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
            --      PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
               SET_COL(STEMLIST, 50);
               INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
            end loop;
         end if;
      
         WRITE(DICTFILE, DE, J);       
      end if;
     
      
      
      
      if not PORTING  then
         CLOSE(STEMLIST);
      end if;
   
      exception
         when TEXT_IO.DATA_ERROR  =>
            null;
         when others =>
            PUT_LINE(S(1..LAST));
            INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
            CLOSE(STEMLIST);
   
   end WAKEDICT;
