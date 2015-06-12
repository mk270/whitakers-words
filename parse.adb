with TEXT_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with PREFACE;
with WORD_PACKAGE; use WORD_PACKAGE;
with LIST_PACKAGE; use LIST_PACKAGE;
with TRICKS_PACKAGE; use TRICKS_PACKAGE;
with CONFIG; use CONFIG;
with PREFACE;
with PUT_STAT;
with ENGLISH_SUPPORT_PACKAGE; use ENGLISH_SUPPORT_PACKAGE;
with SEARCH_ENGLISH;
pragma Elaborate(WORD_PARAMETERS);
procedure PARSE(COMMAND_LINE : STRING := "") is
  use INFLECTIONS_PACKAGE.INTEGER_IO;
  use INFLECTION_RECORD_IO;
  use TEXT_IO;

  STORAGE_ERROR_COUNT : INTEGER := 0;

  J, K, L : INTEGER := 0;
  LINE, BLANK_LINE : STRING(1..2500) := (others => ' ');
  --INPUT : TEXT_IO.FILE_TYPE;

  
  PA : PARSE_ARRAY(1..100) := (others => NULL_PARSE_RECORD);
  SYNCOPE_MAX : constant := 20;
  NO_SYNCOPE : BOOLEAN := FALSE;
  TRICKS_MAX : constant := 40;
  SYPA : PARSE_ARRAY(1..SYNCOPE_MAX) := (others => NULL_PARSE_RECORD);
  TRPA : PARSE_ARRAY(1..TRICKS_MAX) := (others => NULL_PARSE_RECORD);
  PA_LAST, SYPA_LAST, TRPA_LAST : INTEGER := 0;


  procedure PARSE_LINE(INPUT_LINE : STRING) is
    L : INTEGER := TRIM(INPUT_LINE)'LAST;
    --LINE : STRING(1..2500) := (others => ' ');
    W : STRING(1..L) := (others => ' ');      
  begin
    WORD_NUMBER := 0;
    LINE(1..L) := TRIM(INPUT_LINE);
 

  --  Someday I ought to be interested in punctuation and numbers, but not now
  ELIMINATE_NOT_LETTERS:
    begin
    for I in 1..L  loop
      if ((LINE(I) in 'A'..'Z')  or
          (LINE(I) = '-')           or     --  For the comment 
          (LINE(I) = '.')           or     --  Catch period later
          (LINE(I) in 'a'..'z'))  then
        null;
      else
        LINE(I) := ' ';
      end if;
    end loop;
    end ELIMINATE_NOT_LETTERS;



    J := 1;
    K := 0;
    OVER_LINE:
    while J <= L  loop


      
      --  Skip over leading and intervening blanks, looking for comments
      --  Punctuation, numbers, and special characters were cleared above
      for I in K+1..L  loop
        exit when LINE(J) in 'A'..'Z';
        exit when LINE(J) in 'a'..'z';
        if I < L  and then
           LINE(I..I+1) = "--"   then
          exit OVER_LINE;      --  the rest of the line is comment
        end if;
        J := I + 1;
      end loop;

      exit when J > L;             --  Kludge

      FOLLOWS_PERIOD := FALSE;
      if FOLLOWED_BY_PERIOD  then
        FOLLOWED_BY_PERIOD := FALSE;
        FOLLOWS_PERIOD := TRUE;
      end if;



      CAPITALIZED := FALSE;
      ALL_CAPS := FALSE;


      --  Extract the word
      for I in J..L  loop

--  Although I have removed punctuation above, it may not always be so
        if LINE(I) = '.'  then
          FOLLOWED_BY_PERIOD := TRUE;
          exit;
        end if;
--         exit when (LINE(I) = ' ' or LINE(I) = ',' or LINE(I) = '-'
--                or LINE(I) = ';' or LINE(I) = ':'
--                or LINE(I) = '(' or LINE(I) = '[' or LINE(I) = '{' or LINE(I) = '<'
--                or LINE(I) = ')' or LINE(I) = ']' or LINE(I) = '}' or LINE(I) = '>'
--                or (CHARACTER'POS(LINE(I)) < 32)  or (CHARACTER'POS(LINE(I)) > 127) );
          exit when ((LINE(I) not in 'A'..'Z') and (LINE(I) not in 'a'..'z'));
        W(I) := LINE(I);
        K := I;

      end loop;



          if W(J) in 'A'..'Z'  and then
             K - J >= 1  and then
             W(J+1) in 'a'..'z'  then
        CAPITALIZED := TRUE;
      end if;

      ALL_CAPS := TRUE;
      for I in J..K  loop
        if W(I) = LOWER_CASE(W(I))  then
          ALL_CAPS := FALSE;
          exit;
        end if;
      end loop;

      for I in J..K-1  loop               --  Kludge for QVAE
        if W(I) = 'Q'  and then W(I+1) = 'V'  then
          W(I+1) := 'U';
        end if;
      end loop;

      
if LANGUAGE = ENGLISH_TO_LATIN  then
   
PARSE_LINE_ENGLISH_TO_LATIN:  
--  Since we do only one English word per line
declare
  INPUT_WORD : constant STRING := W(J..K);
  POFS : PART_OF_SPEECH_TYPE := X;
begin

--  Extract from the rest of the line
--  Should do AUX here !!!!!!!!!!!!!!!!!!!!!!!!
EXTRACT_POFS:
begin
  PART_OF_SPEECH_TYPE_IO.GET(LINE(K+1..L), POFS, L);
--TEXT_IO.PUT_LINE("In EXTRACT   " & LINE(K+1..L));
exception
  when others =>
    POFS := X;
end EXTRACT_POFS;
--PART_OF_SPEECH_TYPE_IO.PUT(POFS); 
--TEXT_IO.NEW_LINE;

SEARCH_ENGLISH(INPUT_WORD, POFS);

exit OVER_LINE;

end PARSE_LINE_ENGLISH_TO_LATIN;

      
      
              
elsif LANGUAGE = LATIN_TO_ENGLISH  then
  
PARSE_WORD_LATIN_TO_ENGLISH:  
declare
INPUT_WORD : constant STRING := W(J..K);
ENTERING_PA_LAST : INTEGER := 0;
ENTERING_TRPA_LAST    : INTEGER := 0;
HAVE_DONE_ENCLITIC : BOOLEAN := FALSE;
  

procedure PASS(INPUT_WORD : STRING);

      
procedure ENCLITIC is
  SAVE_DO_FIXES  : BOOLEAN := WORDS_MODE(DO_FIXES);
  SAVE_DO_ONLY_FIXES  : BOOLEAN := WORDS_MDEV(DO_ONLY_FIXES);
  ENCLITIC_LIMIT : INTEGER := 4;
  TRY : constant STRING := LOWER_CASE(INPUT_WORD);
begin
--TEXT_IO.PUT_LINE("Entering ENCLITIC  HAVE DONE = " & BOOLEAN'IMAGE(HAVE_DONE_ENCLITIC));
    --if WORDS_MODE(TRIM_OUTPUT)  and (PA_LAST > 0)  then    return;   end if;
    if HAVE_DONE_ENCLITIC  then    return;   end if;
     
    ENTERING_PA_LAST := PA_LAST;
    if PA_LAST > 0 then ENCLITIC_LIMIT := 1; end if;
    LOOP_OVER_ENCLITIC_TACKONS:
    for I in 1..ENCLITIC_LIMIT  loop   --  If have parse, only do que of que, ne, ve, (est) 

      REMOVE_A_TACKON:
      declare
        LESS : constant STRING :=
               SUBTRACT_TACKON(TRY, TACKONS(I));
               --SUBTRACT_TACKON(INPUT_WORD, TACKONS(I));
        SAVE_PA_LAST  : INTEGER := 0;
    begin
--TEXT_IO.PUT_LINE("In ENCLITIC     LESS/TACKON  = " & LESS & "/" & TACKONS(I).TACK); 
       if LESS  /= TRY  then       --  LESS is less
          --WORDS_MODE(DO_FIXES) := FALSE;
          WORD_PACKAGE.WORD(LESS, PA, PA_LAST);
--TEXT_IO.PUT_LINE("In ENCLITICS after WORD NO_FIXES  LESS = " & LESS & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));


if PA_LAST = 0  then

          SAVE_PA_LAST := PA_LAST;
          TRY_SLURY(LESS, PA, PA_LAST, LINE_NUMBER, WORD_NUMBER);
          if SAVE_PA_LAST /= 0   then     
            if (PA_LAST - 1) - SAVE_PA_LAST = SAVE_PA_LAST  then
              PA_LAST := SAVE_PA_LAST;
            end if;
          end if;
          
 end if;         
          
          --  Do not SYNCOPE if there is a verb TO_BE or compound already there
         --  I do this here and below, it might be combined but it workd now
        for I in 1..PA_LAST  loop
 --PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
          if PA(I).IR.QUAL.POFS = V and then
              PA(I).IR.QUAL.V.CON = (5, 1)  then 
             NO_SYNCOPE := TRUE;
           end if;
         end loop;

         
         --TEXT_IO.PUT_LINE("In ENCLITICS after SLURY  LESS = " & LESS & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
          SYPA_LAST := 0;
if WORDS_MDEV(DO_SYNCOPE)  and not NO_SYNCOPE  then
          SYNCOPE(LESS, SYPA, SYPA_LAST);  --  Want SYNCOPE second to make cleaner LIST
--TEXT_IO.PUT_LINE("In ENCLITIC after SYNCOPE  LESS = " & LESS & "   SYPA_LAST = " & INTEGER'IMAGE(SYPA_LAST));
          PA_LAST := PA_LAST + SYPA_LAST;   --  Make syncope another array to avoid PA_LAST = 0 problems
          PA(1..PA_LAST) := PA(1..PA_LAST-SYPA_LAST) & SYPA(1..SYPA_LAST);  --  Add SYPA to PA
          SYPA(1..SYNCOPE_MAX) := (1..SYNCOPE_MAX => NULL_PARSE_RECORD);   --  Clean up so it does not repeat
          SYPA_LAST := 0;
 end if;
             NO_SYNCOPE := FALSE;
         --  Restore FIXES
          --WORDS_MODE(DO_FIXES) := SAVE_DO_FIXES;
          
          WORDS_MDEV(DO_ONLY_FIXES) := TRUE;
          WORD(INPUT_WORD, PA, PA_LAST);
--TEXT_IO.PUT_LINE("In ENCLITICS after ONLY_FIXES  LESS = " & LESS & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
          WORDS_MDEV(DO_ONLY_FIXES) := SAVE_DO_ONLY_FIXES;
          
     
          if PA_LAST > ENTERING_PA_LAST  then      --  have a possible word
              PA_LAST := PA_LAST + 1;
              PA(ENTERING_PA_LAST+2..PA_LAST) :=
                       PA(ENTERING_PA_LAST+1..PA_LAST-1);
              PA(ENTERING_PA_LAST+1) := (TACKONS(I).TACK,
                      ((TACKON, NULL_TACKON_RECORD), 0, NULL_ENDING_RECORD, X, X),
                        ADDONS, DICT_IO.COUNT(TACKONS(I).MNPC));
                        
            HAVE_DONE_ENCLITIC := TRUE;
          end if;
          exit LOOP_OVER_ENCLITIC_TACKONS;
        end if;
      end REMOVE_A_TACKON;
    end loop LOOP_OVER_ENCLITIC_TACKONS;
  end ENCLITIC;    
  
    
  procedure TRICKS_ENCLITIC is
    TRY : constant STRING := LOWER_CASE(INPUT_WORD);
  begin
--TEXT_IO.PUT_LINE("Entering TRICKS_ENCLITIC    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
    --if WORDS_MODE(TRIM_OUTPUT)  and (PA_LAST > 0)  then    return;   end if;
    if HAVE_DONE_ENCLITIC  then    return;   end if;
    
    ENTERING_TRPA_LAST := TRPA_LAST;
    LOOP_OVER_ENCLITIC_TACKONS:
    for I in 1..4  loop   --  que, ne, ve, (est) 

      REMOVE_A_TACKON:
      declare
        LESS : constant STRING :=
               --SUBTRACT_TACKON(LOWER_CASE(INPUT_WORD), TACKONS(I));
               SUBTRACT_TACKON(TRY, TACKONS(I));
      begin
--TEXT_IO.PUT_LINE("In TRICKS_ENCLITIC     LESS/TACKON  = " & LESS & "/" & TACKONS(I).TACK); 
       if LESS  /= TRY  then       --  LESS is less
          --PASS(LESS);
          TRY_TRICKS(LESS, TRPA, TRPA_LAST, LINE_NUMBER, WORD_NUMBER);
 --TEXT_IO.PUT_LINE("In TRICKS_ENCLITICS after TRY_TRICKS  LESS = " & LESS & "   TRPA_LAST = " & INTEGER'IMAGE(TRPA_LAST));
         if TRPA_LAST > ENTERING_TRPA_LAST  then      --  have a possible word
              TRPA_LAST := TRPA_LAST + 1;
              TRPA(ENTERING_TRPA_LAST+2..trPA_LAST) :=
                       TRPA(ENTERING_TRPA_LAST+1..TRPA_LAST-1);
              TRPA(ENTERING_TRPA_LAST+1) := (TACKONS(I).TACK,
                      ((TACKON, NULL_TACKON_RECORD), 0, NULL_ENDING_RECORD, X, X),
                        ADDONS, DICT_IO.COUNT(TACKONS(I).MNPC));
          end if;
          exit LOOP_OVER_ENCLITIC_TACKONS;
        end if;
      end REMOVE_A_TACKON;
    end loop LOOP_OVER_ENCLITIC_TACKONS;
  end TRICKS_ENCLITIC;

procedure PASS(INPUT_WORD : STRING) is
--  This is the core logic of the program, everything else is details
  SAVE_PA_LAST  : INTEGER := 0;
  SAVE_DO_FIXES  : BOOLEAN := WORDS_MODE(DO_FIXES);
  SAVE_DO_ONLY_FIXES  : BOOLEAN := WORDS_MDEV(DO_ONLY_FIXES);
  SAVE_DO_TRICKS : BOOLEAN := WORDS_MODE(DO_TRICKS);
begin
--TEXT_IO.PUT_LINE("Entering PASS with >" & INPUT_WORD);
  --  Do straight WORDS without FIXES/TRICKS, is the word in the dictionary
  WORDS_MODE(DO_FIXES) := FALSE;
  ROMAN_NUMERALS(INPUT_WORD, PA, PA_LAST);  
  WORD(INPUT_WORD, PA, PA_LAST);
  
--TEXT_IO.PUT_LINE("SLURY-   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--for JK in 1..PA_LAST  loop
-- f PARSE_RECORD_IO.PUT(PA(JK)); TEXT_IO.NEW_LINE;
--end loop;


  if PA_LAST = 0  then
    TRY_SLURY(INPUT_WORD, PA, PA_LAST, LINE_NUMBER, WORD_NUMBER);
  end if;
 
  
          --  Do not SYNCOPE if there is a verb TO_BE or compound already there
         for I in 1..PA_LAST  loop
 --PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
          if PA(I).IR.QUAL.POFS = V and then
              PA(I).IR.QUAL.V.CON = (5, 1)  then 
             NO_SYNCOPE := TRUE;
           end if;
         end loop;
 
   
-- --  WITH THE DICTIONARY BETTER, LET US FORGET THIS - a and c DONE, e and i STILL BUT NOT MANY
--  SAVE_PA_LAST := PA_LAST;
--  --  BIG PROBLEM HERE
--  --  If I do SLURY everytime, then each case where there is an aps- and abs- in dictionary 
--  --  will show up twice, straight and SLURY, in the ourout - For either input
--  --  But if I only do SLURY if there is no hit, then some incomplete pairs will not
--  --  fully express (illuxit has two entries, inluxit has only one of them) (inritas)
--  --  So I will do SLURY and if it produces only 2 more PR (XXX and GEN), kill it, otherwise use it only
--  --  Still have a problem if there are other intervening results, not slurried.
--  --  Or if there is syncope
--  TRY_SLURY(INPUT_WORD, PA, PA_LAST, LINE_NUMBER, WORD_NUMBER);
----TEXT_IO.PUT_LINE("SLURY+   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--  if SAVE_PA_LAST /= 0   then     
--    if (PA_LAST - 2) = SAVE_PA_LAST  then
--      PA_LAST := SAVE_PA_LAST;
--      XXX_MEANING := NULL_MEANING_TYPE;
----TEXT_IO.PUT_LINE("SLURY!   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--    end if;
--  end if;
----TEXT_IO.PUT_LINE("1  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
          
  --  Pure SYNCOPE
     SYPA_LAST := 0;
if WORDS_MDEV(DO_SYNCOPE)  and not NO_SYNCOPE  then
     SYNCOPE(INPUT_WORD, SYPA, SYPA_LAST);  
     PA_LAST := PA_LAST + SYPA_LAST;   --  Make syncope another array to avoid PA-LAST = 0 problems
     PA(1..PA_LAST) := PA(1..PA_LAST-SYPA_LAST) & SYPA(1..SYPA_LAST);  --  Add SYPA to PA
     SYPA(1..SYNCOPE_MAX) := (1..SYNCOPE_MAX => NULL_PARSE_RECORD);   --  Clean up so it does not repeat
     SYPA_LAST := 0;
end if;
             NO_SYNCOPE := FALSE;

--TEXT_IO.PUT_LINE("2  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
     
     --  There may be a vaild simple parse, if so it is most probable
     --  But I have to allow for the possibility that -que is answer, not colloque V 
        ENCLITIC;
        
     --  Restore FIXES
     WORDS_MODE(DO_FIXES) := SAVE_DO_FIXES;
--TEXT_IO.PUT_LINE("3  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
    
     --  Now with only fixes
     if PA_LAST = 0  and then
       WORDS_MODE(DO_FIXES)  then
       WORDS_MDEV(DO_ONLY_FIXES) := TRUE;
 --TEXT_IO.PUT_LINE("3a PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
      WORD(INPUT_WORD, PA, PA_LAST);
 --TEXT_IO.PUT_LINE("3b PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
      SYPA_LAST := 0;
 if WORDS_MDEV(DO_SYNCOPE)  and not NO_SYNCOPE  then
      SYNCOPE(INPUT_WORD, SYPA, SYPA_LAST);  
 --TEXT_IO.PUT_LINE("3c PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
      PA_LAST := PA_LAST + SYPA_LAST;   --  Make syncope another array to avoid PA-LAST = 0 problems
       PA(1..PA_LAST) := PA(1..PA_LAST-SYPA_LAST) & SYPA(1..SYPA_LAST);  --  Add SYPA to PA
       SYPA(1..SYNCOPE_MAX) := (1..SYNCOPE_MAX => NULL_PARSE_RECORD);   --  Clean up so it does not repeat
       SYPA_LAST := 0;
end if;
             NO_SYNCOPE := FALSE;

--TEXT_IO.PUT_LINE("4  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
       ENCLITIC;
   
--TEXT_IO.PUT_LINE("5  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
       WORDS_MDEV(DO_ONLY_FIXES) := SAVE_DO_ONLY_FIXES;
     end if;
--TEXT_IO.PUT_LINE("6  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--  ROMAN_NUMERALS(INPUT_WORD, PA, PA_LAST);  
     
     --  If Pure WORDS and ENCLITICS found something OK, otherwise proceed
--    if PA_LAST = 0  or        --  If no go, try syncope, fixes
--      (not WORDS_MODE(TRIM_OUTPUT)) or     
--       WORDS_MDEV(DO_FIXES_ANYWAY) then     
--   
--     
--     --  If SYNCOPE does it, then OK, otherwise proceed
--     --  Do not try FIXES (aud+i+i) on audii since SYNCOPE worked
--     --  Now try FIXES
--     if PA_LAST = 0  or (not WORDS_MODE(TRIM_OUTPUT)) or     
--       WORDS_MDEV(DO_FIXES_ANYWAY)  then     
--      --TRY_SLURY(INPUT_WORD, PA, PA_LAST, LINE_NUMBER, WORD_NUMBER);
--       if PA_LAST = 0  then 
--       WORD(INPUT_WORD, PA, PA_LAST);
--         SYPA_LAST := 0;
--         --  SYNCOPE after TRICK
--         SYNCOPE(INPUT_WORD, SYPA, SYPA_LAST);  --  Want SYNCOPE second to make cleaner LIST
--       end if;
--     end if;
--     PA_LAST := PA_LAST + SYPA_LAST;   --  Make syncope another array to avoid PA_LAST = 0 problems
--     PA(1..PA_LAST) := PA(1..PA_LAST-SYPA_LAST) & SYPA(1..SYPA_LAST);  --  Add SYPA to PA
--     SYPA(1..SYNCOPE_MAX) := (1..SYNCOPE_MAX => NULL_PARSE_RECORD);   --  Clean up so it does not repeat
--     SYPA_LAST := 0;
--
--  
-- end if;   --  on A_LAST = 0
      
end PASS;

begin   --  PARSE
  XXX_MEANING := NULL_MEANING_TYPE;

  PASS_BLOCK:
  begin
    PA_LAST := 0;
    WORD_NUMBER := WORD_NUMBER + 1;

    PASS(INPUT_WORD);

  end PASS_BLOCK;
  
--TEXT_IO.PUT_LINE("After PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

     --if (PA_LAST = 0) or DO_TRICKS_ANYWAY  then    --  WORD failed, try to modify the word
      if (PA_LAST = 0)  and then         
           not (WORDS_MODE(IGNORE_UNKNOWN_NAMES)  and CAPITALIZED)  then
      --  WORD failed, try to modify the word
--TEXT_IO.PUT_LINE("WORDS fail me");
        if WORDS_MODE(DO_TRICKS)  then
--TEXT_IO.PUT_LINE("DO_TRICKS      PA_LAST    TRPA_LAST  " & INTEGER'IMAGE(PA_LAST) & "   " & INTEGER'IMAGE(TRPA_LAST));
          WORDS_MODE(DO_TRICKS) := FALSE;  --  Turn it off so wont be circular
          TRY_TRICKS(INPUT_WORD, TRPA, TRPA_LAST, LINE_NUMBER, WORD_NUMBER);
--TEXT_IO.PUT_LINE("DONE_TRICKS    PA_LAST    TRPA_LAST  " & INTEGER'IMAGE(PA_LAST) & "   " & INTEGER'IMAGE(TRPA_LAST));
          if TRPA_LAST = 0  then
            TRICKS_ENCLITIC;
          end if;
          WORDS_MODE(DO_TRICKS) := TRUE;   --  Turn it back on
        end if;
        
      PA_LAST := PA_LAST + TRPA_LAST;   --  Make TRICKS another array to avoid PA-LAST = 0 problems
      PA(1..PA_LAST) := PA(1..PA_LAST-TRPA_LAST) & TRPA(1..TRPA_LAST);  --  Add SYPA to PA
      TRPA(1..TRICKS_MAX) := (1..TRICKS_MAX => NULL_PARSE_RECORD);   --  Clean up so it does not repeat
      TRPA_LAST := 0;

      end if;
      
      
 --TEXT_IO.PUT_LINE("After TRICKS " & INTEGER'IMAGE(PA_LAST));     
  

--======================================================================

--  At this point we have done what we can with individual words
--  Now see if there is something we can do with word combinations
--  For this we have to look ahead


      if PA_LAST > 0   then    --  But PA may be killed by ALLOW in LIST_STEMS
if WORDS_MODE(DO_COMPOUNDS)  and
   not (CONFIGURATION = ONLY_MEANINGS)  then
COMPOUNDS_WITH_SUM:
declare
  NW : STRING(1..2500) := (others => ' ');
  NK : INTEGER := 0;

  COMPOUND_TENSE : INFLECTIONS_PACKAGE.TENSE_TYPE := X;
  COMPOUND_TVM   : INFLECTIONS_PACKAGE.TENSE_VOICE_MOOD_RECORD;
  PPL_ON : BOOLEAN := FALSE;

  SUM_INFO : VERB_RECORD := ((5, 1),
                             (X, ACTIVE, X),
                              0,
                              X);

--  ESSE_INFO : VERB_RECORD := ((5, 1),
--                              (PRES, ACTIVE, INF),
--                               0,
--                               X);

  PPL_INFO : VPAR_RECORD := ((0, 0),
                              X,
                              X,
                              X,
                             (X, X, X));

  SUPINE_INFO : SUPINE_RECORD := ((0, 0),
                                   X,
                                   X,
                                   X);

  procedure LOOK_AHEAD is
    J : INTEGER := 0;
  begin
    for I in K+2..L  loop
    --  Although I have removed punctuation above, it may not always be so
      exit when (LINE(I) = ' ' or LINE(I) = ',' or LINE(I) = '-'
              or LINE(I) = ';' or LINE(I) = ':' or LINE(I) = '.'
              or LINE(I) = '(' or LINE(I) = '[' or LINE(I) = '{' or LINE(I) = '<'
              or LINE(I) = ')' or LINE(I) = ']' or LINE(I) = '}' or LINE(I) = '>');
      J := J + 1;
      NW(J) := LINE(I);
      NK := I;
    end loop;
  end LOOK_AHEAD;

  function NEXT_WORD return STRING is
  begin
    return TRIM(NW);
  end NEXT_WORD;

  function IS_SUM(T : STRING) return BOOLEAN is
    SA : constant array (MOOD_TYPE range IND..SUB,
                         TENSE_TYPE range PRES..FUTP,
                         NUMBER_TYPE range S..P,
                         PERSON_TYPE range 1..3)
                                                    of STRING(1..9) :=
(
 (         --  IND
(("sum      ", "es       ", "est      "), ("sumus    ", "estis    ", "sunt     ")),
(("eram     ", "eras     ", "erat     "), ("eramus   ", "eratis   ", "erant    ")),
(("ero      ", "eris     ", "erit     "), ("erimus   ", "eritis   ", "erunt    ")),
(("fui      ", "fuisti   ", "fuit     "), ("fuimus   ", "fuistis  ", "fuerunt  ")),
(("fueram   ", "fueras   ", "fuerat   "), ("fueramus ", "fueratis ", "fuerant  ")),
(("fuero    ", "fueris   ", "fuerit   "), ("fuerimus ", "fueritis ", "fuerunt  "))
 ),
 (         --  SUB
(("sim      ", "sis      ", "sit      "), ("simus    ", "sitis    ", "sint     ")),
(("essem    ", "esses    ", "esset    "), ("essemus  ", "essetis  ", "essent   ")),
(("zzz      ", "zzz      ", "zzz      "), ("zzz      ", "zzz      ", "zzz      ")),
(("fuerim   ", "fueris   ", "fuerit   "), ("fuerimus ", "fueritis ", "fuerint  ")),
(("fuissem  ", "fuisses  ", "fuisset  "), ("fuissemus", "fuissetis", "fuissent ")),
(("zzz      ", "zzz      ", "zzz      "), ("zzz      ", "zzz      ", "zzz      "))
 )
);

  begin
    if T = ""  then
      return FALSE;
    elsif T(T'FIRST) /= 's'  and
          T(T'FIRST) /= 'e'  and
          T(T'FIRST) /= 'f'      then
    return FALSE;
    end if;
    for L in MOOD_TYPE range IND..SUB  loop
      for K in TENSE_TYPE range PRES..FUTP  loop
        for J in NUMBER_TYPE range S..P  loop
          for I in PERSON_TYPE range 1..3  loop
            if TRIM(T) = TRIM(SA(L, K, J, I))  then
              SUM_INFO := ((5, 1), (K, ACTIVE, L), I, J);
              return TRUE;     --  Only one of the forms can agree
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
    return FALSE;
  end IS_SUM;

  function IS_ESSE(T : STRING) return BOOLEAN is
  begin
    return TRIM(T) = "esse";
  end IS_ESSE;

  function IS_FUISSE(T : STRING) return BOOLEAN is
  begin
    return TRIM(T) = "fuisse";
  end IS_FUISSE;

  function IS_IRI(T : STRING) return BOOLEAN is
  begin
    return TRIM(T) = "iri";
  end IS_IRI;


begin

  --  Look ahead for sum                                           
LOOK_AHEAD;
if IS_SUM(NEXT_WORD)  then                 --  On NEXT_WORD = sum, esse, iri

    for I in 1..PA_LAST  loop    --  Check for PPL
      if PA(I).IR.QUAL.POFS = VPAR and then
         PA(I).IR.QUAL.VPAR.CS = NOM  and then
         PA(I).IR.QUAL.VPAR.NUMBER = SUM_INFO.NUMBER  and then
      ( (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)) or
        (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  ACTIVE,  PPL)) or
        (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  PASSIVE, PPL)) )  then

        --  There is at least one hit, fix PA, and advance J over the sum
        K := NK;

      end if;
    end loop;

    if K = NK  then      --  There was a PPL hit
      CLEAR_PAS_NOM_PPL:
      declare
        J : INTEGER := PA_LAST;
      begin
        while J >= 1  loop        --  Sweep backwards to kill empty suffixes
          if ((PA(J).IR.QUAL.POFS = PREFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.POFS = SUFFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.POFS = TACKON) and then (PPL_ON))  then
            null;



          elsif PA(J).IR.QUAL.POFS = VPAR and then
             PA(J).IR.QUAL.VPAR.CS = NOM  and then
             PA(J).IR.QUAL.VPAR.NUMBER = SUM_INFO.NUMBER  then

            if PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)  then
            PPL_ON := TRUE;

             case SUM_INFO.TENSE_VOICE_MOOD.TENSE is  --  Allows PERF for sum
                when PRES | PERF  =>  COMPOUND_TENSE := PERF;
                when IMPF | PLUP  =>  COMPOUND_TENSE := PLUP;
                when FUT          =>  COMPOUND_TENSE := FUTP;
                when others       =>  COMPOUND_TENSE := X;
              end case;
           COMPOUND_TVM := (COMPOUND_TENSE, PASSIVE, SUM_INFO.TENSE_VOICE_MOOD.MOOD);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD);
      PPP_MEANING :=
          HEAD("PERF PASSIVE PPL + verb TO_BE => PASSIVE perfect system",
                MAX_MEANING_SIZE);

            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, ACTIVE,  PPL)  then
            PPL_ON := TRUE;
            COMPOUND_TENSE := SUM_INFO.TENSE_VOICE_MOOD.TENSE;
           COMPOUND_TVM := (COMPOUND_TENSE, ACTIVE, SUM_INFO.TENSE_VOICE_MOOD.MOOD);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD);
      PPP_MEANING := HEAD(
     "FUT ACTIVE PPL + verb TO_BE => ACTIVE Periphrastic - about to, going to",
                MAX_MEANING_SIZE);

            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, PASSIVE, PPL)  then
            PPL_ON := TRUE;
            COMPOUND_TENSE := SUM_INFO.TENSE_VOICE_MOOD.TENSE;
           COMPOUND_TVM := (COMPOUND_TENSE, PASSIVE, SUM_INFO.TENSE_VOICE_MOOD.MOOD);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD);
      PPP_MEANING := HEAD(
  "FUT PASSIVE PPL + verb TO_BE => PASSIVE Periphrastic - should/ought/had to",
                MAX_MEANING_SIZE);

            end if;
          else
            PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
            PA_LAST := PA_LAST - 1;
            PPL_ON := FALSE;
          end if;
          J := J - 1;
        end loop;
      end CLEAR_PAS_NOM_PPL;


      PA_LAST := PA_LAST + 1;
      PA(PA_LAST) :=
          (HEAD("PPL+" & NEXT_WORD, MAX_STEM_SIZE),
                ((V,
                    (PPL_INFO.CON,
                     COMPOUND_TVM,
                     SUM_INFO.PERSON,
                     SUM_INFO.NUMBER)
                 ), 0, NULL_ENDING_RECORD, X, A),
                    PPP, NULL_MNPC);

    end if;

elsif IS_ESSE(NEXT_WORD) or IS_FUISSE(NEXT_WORD)  then     --  On NEXT_WORD

    for I in 1..PA_LAST  loop    --  Check for PPL
      if PA(I).IR.QUAL.POFS = VPAR and then
      (((PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)) and
                                                   IS_ESSE(NEXT_WORD)) or
       ((PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  ACTIVE,  PPL)) or
        (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  PASSIVE, PPL))) )  then

        --  There is at least one hit, fix PA, and advance J over the sum
        K := NK;

      end if;
    end loop;

    if K = NK  then      --  There was a PPL hit
      CLEAR_PAS_PPL:
      declare
        J : INTEGER := PA_LAST;
      begin
        while J >= 1  loop        --  Sweep backwards to kill empty suffixes
          if ((PA(J).IR.QUAL.POFS = PREFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.POFS = SUFFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.POFS = TACKON) and then (PPL_ON))  then
            null;



          elsif PA(J).IR.QUAL.POFS = VPAR   then

            if PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)  then
            PPL_ON := TRUE;

           COMPOUND_TVM := (PERF, PASSIVE, INF);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD);
            PPP_MEANING :=
                HEAD("PERF PASSIVE PPL + esse => PERF PASSIVE INF",
                      MAX_MEANING_SIZE);

            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, ACTIVE,  PPL)  then
            PPL_ON := TRUE;
           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD);
            if IS_ESSE(NEXT_WORD)  then
              COMPOUND_TVM := (FUT, ACTIVE, INF);
      PPP_MEANING := HEAD(
     "FUT ACTIVE PPL + esse => PRES Periphastic/FUT ACTIVE INF - be about/going to",
                MAX_MEANING_SIZE);
              -- also peri COMPOUND_TVM := (PRES, ACTIVE, INF);
            else   --  fuisse
              COMPOUND_TVM := (PERF, ACTIVE, INF);
     PPP_MEANING := HEAD(
     "FUT ACT PPL+fuisse => PERF ACT INF Periphrastic - to have been about/going to",
                MAX_MEANING_SIZE);
            end if;


            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, PASSIVE, PPL)  then
            PPL_ON := TRUE;

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD);
            if IS_ESSE(NEXT_WORD)  then
              COMPOUND_TVM := (PRES, PASSIVE, INF);
      PPP_MEANING := HEAD(
     "FUT PASSIVE PPL + esse => PRES PASSIVE INF",
                MAX_MEANING_SIZE);
              -- also peri COMPOUND_TVM := (PRES, ACTIVE, INF);
            else   --  fuisse
              COMPOUND_TVM := (PERF, PASSIVE, INF);
     PPP_MEANING := HEAD(
     "FUT PASSIVE PPL + fuisse => PERF PASSIVE INF Periphrastic - about to, going to",
                MAX_MEANING_SIZE);
            end if;


            end if;
          else
            PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
            PA_LAST := PA_LAST - 1;
            PPL_ON := FALSE;
          end if;
          J := J - 1;
        end loop;
      end CLEAR_PAS_PPL;


      PA_LAST := PA_LAST + 1;
      PA(PA_LAST) :=
          (HEAD("PPL+" & NEXT_WORD, MAX_STEM_SIZE),
                ((V,
                    (PPL_INFO.CON,
                     COMPOUND_TVM,
                     0,
                     X)
                 ), 0, NULL_ENDING_RECORD, X, A),
                    PPP, NULL_MNPC);

    end if;

elsif IS_IRI(NEXT_WORD)  then              --  On NEXT_WORD = sum, esse, iri
  --  Look ahead for sum                                           

    for J in 1..PA_LAST  loop    --  Check for SUPINE
      if PA(J).IR.QUAL.POFS = SUPINE   and then
         PA(J).IR.QUAL.SUPINE.CS = ACC    then
         --  There is at least one hit, fix PA, and advance J over the iri
        K := NK;

      end if;
    end loop;

    if K = NK  then      --  There was a SUPINE hit
      CLEAR_PAS_SUPINE:
      declare
        J : INTEGER := PA_LAST;
      begin
        while J >= 1  loop        --  Sweep backwards to kill empty suffixes
          if ((PA(J).IR.QUAL.POFS = PREFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.POFS = SUFFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.POFS = TACKON) and then (PPL_ON))  then
            null;



          elsif PA(J).IR.QUAL.POFS = SUPINE  and then
                PA(J).IR.QUAL.SUPINE.CS = ACC  then

            PPL_ON := TRUE;
         SUPINE_INFO := (PA(J).IR.QUAL.SUPINE.CON,
                         PA(J).IR.QUAL.SUPINE.CS,
                         PA(J).IR.QUAL.SUPINE.NUMBER,
                         PA(J).IR.QUAL.SUPINE.GENDER);



      PA_LAST := PA_LAST + 1;
      PA(PA_LAST) :=
          (HEAD("SUPINE + iri", MAX_STEM_SIZE),
                ((V,
                    (SUPINE_INFO.CON,
                     (FUT, PASSIVE, INF),
                     0,
                     X)
                 ), 0, NULL_ENDING_RECORD, X, A),
                    PPP, NULL_MNPC);
      PPP_MEANING := HEAD(
     "SUPINE + iri => FUT PASSIVE INF - to be about/going/ready to be ~",
                MAX_MEANING_SIZE);

            K := NK;


          else
            PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
            PA_LAST := PA_LAST - 1;
            PPL_ON := FALSE;
          end if;
          J := J -1;
        end loop;
      end CLEAR_PAS_SUPINE;
    end if;

end if;       --  On NEXT_WORD = sum, esse, iri


end COMPOUNDS_WITH_SUM;
end if;       --  On WORDS_MODE(DO_COMPOUNDS)


--========================================================================
 end if;

--TEXT_IO.PUT_LINE("Before LISTing STEMS (PA_LAST > 0 to start) PA_LAST = " & 
--INTEGER'IMAGE(PA_LAST));
 
          if  WORDS_MODE(WRITE_OUTPUT_TO_FILE)      then
            LIST_STEMS(OUTPUT, INPUT_WORD, INPUT_LINE, PA, PA_LAST);
          else
            LIST_STEMS(CURRENT_OUTPUT, INPUT_WORD, INPUT_LINE, PA, PA_LAST);
          end if;

--TEXT_IO.PUT_LINE("After LISTing STEMS (PA_LAST > 0 to start) PA_LAST = " & 
--INTEGER'IMAGE(PA_LAST));
          
          
      PA_LAST := 0;

exception
  when others  =>
    PUT_STAT("Exception    at "
           & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)    
           & "   " & HEAD(INPUT_WORD, 28) & "   "  & INPUT_LINE);
    raise;

end PARSE_WORD_LATIN_TO_ENGLISH;


end if;

----------------------------------------------------------------------
----------------------------------------------------------------------


      J := K + 1;    --  In case it is end of line and we don't look for ' '

      exit when WORDS_MDEV(DO_ONLY_INITIAL_WORD);

    end loop OVER_LINE;        --  Loop on line

exception
  --   Have STORAGE_ERROR check in WORD too  ?????????????
  when STORAGE_ERROR  =>    --  I want to again, at least twice
    if WORDS_MDEV(DO_PEARSE_CODES) then
      TEXT_IO.PUT("00 ");
    end if;
    TEXT_IO.PUT_LINE(    --  ERROR_FILE,
                          "STORAGE_ERROR Exception in WORDS, try again");
    STORAGE_ERROR_COUNT := STORAGE_ERROR_COUNT + 1;
    if STORAGE_ERROR_COUNT >= 4  then  raise; end if;
    PA_LAST := 0;
  when GIVE_UP =>
    PA_LAST := 0;
    raise;
  when others  =>    --  I want to try to get on with the next line
    TEXT_IO.PUT_LINE(    --  ERROR_FILE,
                          "Exception in PARSE_LINE processing " & INPUT_LINE);
        if WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(UNKNOWNS, "00 ");
          end if;
          TEXT_IO.PUT(UNKNOWNS, INPUT_LINE(J..K));
          TEXT_IO.SET_COL(UNKNOWNS, 30);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, LINE_NUMBER, 5);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, WORD_NUMBER, 3);
          TEXT_IO.PUT_LINE(UNKNOWNS, "    ========   ERROR      ");
        end if;
      PA_LAST := 0;
end PARSE_LINE;     


--procedure CHANGE_LANGUAGE(C : CHARACTER) is
--begin
--  if UPPER_CASE(C) = 'L'  then
--    LANGUAGE := LATIN_TO_ENGLISH;
--    PREFACE.PUT_LINE("Language changed to " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
--  elsif UPPER_CASE(C) = 'E'  then  
--    if ENGLISH_DICTIONARY_AVAILABLE(GENERAL)  then
--      LANGUAGE:= ENGLISH_TO_LATIN;
--      PREFACE.PUT_LINE("Language changed to " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
--      PREFACE.PUT_LINE("Input a single English word (+ part of speech - N, ADJ, V, PREP, ...)");
--    else
--      PREFACE.PUT_LINE("No English dictionary available");
--    end if;
--  else
--    PREFACE.PUT_LINE("Bad LANGAUGE input - no change, remains " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
--  end if;
--exception 
--  when others  =>
--    PREFACE.PUT_LINE("Bad LANGAUGE input - no change, remains " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
--end CHANGE_LANGUAGE;
--    
--  


begin              --  PARSE
--  All Rights Reserved   -   William Armstrong Whitaker

--  INITIALIZE_WORD_PARAMETERS;
--  INITIALIZE_DEVELOPER_PARAMETERS;
--  INITIALIZE_WORD_PACKAGE;
--
  if METHOD = COMMAND_LINE_INPUT  then
    if TRIM(COMMAND_LINE) /= ""  then
      PARSE_LINE(COMMAND_LINE);
    end if;

  else

  PREFACE.PUT_LINE(
"Copyright (c) 1993-2006 - Free for any use - Version 1.97FC");
  PREFACE.PUT_LINE(
"For updates and latest version check http://www.erols.com/whitaker/words.htm");
  PREFACE.PUT_LINE(
"Comments? William Whitaker, Box 51225  Midland  TX  79710  USA - whitaker@erols.com");
  PREFACE.NEW_LINE;
  PREFACE.PUT_LINE(
"Input a word or line of Latin and ENTER to get the forms and meanings");
  PREFACE.PUT_LINE("    Or input " & START_FILE_CHARACTER &
           " and the name of a file containing words or lines");
  PREFACE.PUT_LINE("    Or input " & CHANGE_PARAMETERS_CHARACTER &
           " to change parameters and mode of the program");
  PREFACE.PUT_LINE("    Or input " & HELP_CHARACTER &
           " to get help wherever available on individual parameters");
  PREFACE.PUT_LINE(
"Two empty lines (just a RETURN/ENTER) from the keyboard exits the program");

  if ENGLISH_DICTIONARY_AVAILABLE(GENERAL)  then
    PREFACE.PUT_LINE("English-to-Latin available");
    PREFACE.PUT_LINE(
                   CHANGE_LANGUAGE_CHARACTER & "E changes to English-to-Latin, " &
                   CHANGE_LANGUAGE_CHARACTER & "L changes back     [tilde E]");
  end if;
  
  if CONFIGURATION = ONLY_MEANINGS  then
    PREFACE.PUT_LINE(
          "THIS VERSION IS HARDCODED TO GIVE DICTIONARY FORM AND MEANINGS ONLY");
    PREFACE.PUT_LINE(
        "IT CANNOT BE MODIFIED BY CHANGING THE DO_MEANINGS_ONLY PARAMETER");
  end if;

  GET_INPUT_LINES:
  loop
    GET_INPUT_LINE:
    begin                    --  Block to manipulate file of lines
      if (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then
        SCROLL_LINE_NUMBER := INTEGER(TEXT_IO.LINE(TEXT_IO.STANDARD_OUTPUT));
        PREFACE.NEW_LINE;
        PREFACE.PUT("=>");
      end if;

      LINE := BLANK_LINE;
      GET_LINE(LINE, L);
      if (L = 0) or else (TRIM(LINE(1..L)) = "")  then
        --LINE_NUMBER := LINE_NUMBER + 1;  --  Count blank lines 
        if (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then   --  INPUT is keyboard
          PREFACE.PUT("Blank exits =>");
          GET_LINE(LINE, L);             -- Second try
          if (L = 0) or else (TRIM(LINE(1..L)) = "")  then  -- Two in a row
            exit;
          end if;
        else                 --  INPUT is file
          --LINE_NUMBER := LINE_NUMBER + 1;   --  Count blank lines in file
          if END_OF_FILE(CURRENT_INPUT) then
            SET_INPUT(STANDARD_INPUT);
            CLOSE(INPUT);
          end if;
        end if;
      end if;
      
      if (TRIM(LINE(1..L)) /= "")  then  -- Not a blank line so L(1) (in file input)
        if LINE(1) = START_FILE_CHARACTER  then    --  To begin file of words
          if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT)) then
            TEXT_IO.PUT_LINE("Cannot have file of words (@FILE) in an @FILE");
          else
            TEXT_IO.OPEN(INPUT, TEXT_IO.IN_FILE, TRIM(LINE(2..L)));
            TEXT_IO.SET_INPUT(INPUT);
          end if;
        elsif LINE(1) = CHANGE_PARAMETERS_CHARACTER  and then
              (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT)) and then
              not CONFIG.SUPPRESS_PREFACE  then
          CHANGE_PARAMETERS;
        elsif LINE(1) = CHANGE_LANGUAGE_CHARACTER  then
           -- (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT)) and then
           --   not CONFIG.SUPPRESS_PREFACE  then
  --TEXT_IO.PUT_LINE("CHANGE CHARACTER   " & TRIM(LINE));
         CHANGE_LANGUAGE(LINE(2));
        elsif --  CONFIGURATION = DEVELOPER_VERSION  and then    --  Allow anyone to do it
              LINE(1) = CHANGE_DEVELOPER_MODES_CHARACTER  and then
              (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT)) and then
              not CONFIG.SUPPRESS_PREFACE  then
          CHANGE_DEVELOPER_MODES;
        else
          if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  then
            PREFACE.NEW_LINE;
            PREFACE.PUT_LINE(LINE(1..L));
          end if;
          if WORDS_MODE(WRITE_OUTPUT_TO_FILE)     then
            if not CONFIG.SUPPRESS_PREFACE     then
              NEW_LINE(OUTPUT);
              TEXT_IO.PUT_LINE(OUTPUT, LINE(1..L));
            end if;
          end if;
          LINE_NUMBER := LINE_NUMBER + 1;  --  Count lines to be parsed
          PARSE_LINE(LINE(1..L));
        end if;
      end if;
  
    exception
      when NAME_ERROR | USE_ERROR =>
        if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  then
          SET_INPUT(STANDARD_INPUT);
          CLOSE(INPUT);
        end if;
        PUT_LINE("An unknown or unacceptable file name. Try Again");
      when END_ERROR =>          --  The end of the input file resets to CON:
        if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  then
          SET_INPUT(STANDARD_INPUT);
          CLOSE(INPUT);
          if METHOD = COMMAND_LINE_FILES  then raise GIVE_UP; end if;
        else
          PUT_LINE("Raised END_ERROR, although in STANDARD_INPUT");
          PUT_LINE("^Z is inappropriate keyboard input, WORDS should be terminated with a blank line");
          raise GIVE_UP;
        end if;
      when STATUS_ERROR =>      --  The end of the input file resets to CON:
          PUT_LINE("Raised STATUS_ERROR");
    end GET_INPUT_LINE;                     --  end Block to manipulate file of lines

  end loop GET_INPUT_LINES;          --  Loop on lines

  end if;     --  On command line input

  begin
    STEM_IO.OPEN(STEM_FILE(LOCAL), STEM_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                                                      "LOCAL"));
          --  Failure to OPEN will raise an exception, to be handled below
    if STEM_IO.IS_OPEN(STEM_FILE(LOCAL)) then
      STEM_IO.DELETE(STEM_FILE(LOCAL));
    end if;
  exception
    when others =>
      null;      --  If cannot OPEN then it does not exist, so is deleted
  end;
  --  The rest of this seems like overkill, it might have been done elsewhere
  begin
    if
      DICT_IO.IS_OPEN(DICT_FILE(LOCAL)) then
      DICT_IO.DELETE(DICT_FILE(LOCAL));
    else
      DICT_IO.OPEN(DICT_FILE(LOCAL), DICT_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                                      "LOCAL"));
      DICT_IO.DELETE(DICT_FILE(LOCAL));
    end if;
  exception when others => null; end;   --  not there, so don't have to DELETE
  begin
    if
      DICT_IO.IS_OPEN(DICT_FILE(ADDONS))  then
      DICT_IO.DELETE(DICT_FILE(ADDONS));
    else
      DICT_IO.OPEN(DICT_FILE(ADDONS), DICT_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                                      "ADDONS"));
     DICT_IO.DELETE(DICT_FILE(ADDONS));
    end if;
  exception when others => null; end;   --  not there, so don't have to DELETE
  begin
    if
      DICT_IO.IS_OPEN(DICT_FILE(UNIQUE)) then
      DICT_IO.DELETE(DICT_FILE(UNIQUE));
    else
      DICT_IO.OPEN(DICT_FILE(UNIQUE), DICT_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                                      "UNIQUE"));
      DICT_IO.DELETE(DICT_FILE(UNIQUE));
    end if;
  exception when others => null; end;   --  not there, so don't have to DELETE

exception
  when STORAGE_ERROR  =>    --  Have tried at least twice, fail
    PREFACE.PUT_LINE("Continuing STORAGE_ERROR Exception in PARSE");
    PREFACE.PUT_LINE("If insufficient memory in DOS, try removing TSRs");
  when GIVE_UP  =>
    PREFACE.PUT_LINE("Giving up!");
  when others  =>
    PREFACE.PUT_LINE("Unexpected exception raised in PARSE");
end PARSE;
