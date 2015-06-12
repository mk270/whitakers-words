with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with CONFIG;
with PREFACE;
package body WORD_SUPPORT_PACKAGE is


    function LEN(S : STRING) return INTEGER is
    begin
      return TRIM(S)'LENGTH;
    end LEN;

    function EFF_PART(PART : PART_OF_SPEECH_TYPE) return PART_OF_SPEECH_TYPE is
      begin
        if PART = VPAR   then
          return V;
        elsif PART = SUPINE  then
          return V;
        else
          return PART;
        end if;
      end EFF_PART;

    function ADJ_COMP_FROM_KEY(KEY : STEM_KEY_TYPE) return COMPARISON_TYPE is
    begin
      case KEY is
        when 0 | 1 | 2  => return POS;
        when 3          => return COMP;
        when 4          => return SUPER;
        when others     => return X;
      end case;
    end ADJ_COMP_FROM_KEY;

    function ADV_COMP_FROM_KEY(KEY : STEM_KEY_TYPE) return COMPARISON_TYPE is
    begin
      case KEY is
        when 1  => return POS;
        when 2  => return COMP;
        when 3  => return SUPER;
        when others  => return X;
      end case;
    end ADV_COMP_FROM_KEY;

  function NUM_SORT_FROM_KEY(KEY : STEM_KEY_TYPE) return NUMERAL_SORT_TYPE is
  begin
      case KEY is
        when 1  => return CARD;
        when 2  => return ORD;
        when 3  => return DIST;
        when 4  => return ADVERB;
        when others  => return X;
      end case;
  end NUM_SORT_FROM_KEY;


  function FIRST_INDEX(INPUT_WORD : STRING;
                  D_K : DICTIONARY_FILE_KIND := DEFAULT_DICTIONARY_FILE_KIND)
                       return STEM_IO.COUNT is
    WD : constant STRING := TRIM(INPUT_WORD);  --  string may not start at 1
  begin
    if D_K = LOCAL  then
      return DDLF(WD(WD'FIRST), 'a', D_K);
    elsif WD'LENGTH < 2 then
      return   0; --  BDLF(WD(WD'FIRST), ' ', D_K);
    else
      return DDLF(WD(WD'FIRST), WD(WD'FIRST+1), D_K);
    end if;
  end FIRST_INDEX;

  function  LAST_INDEX(INPUT_WORD : STRING;
                 D_K : DICTIONARY_FILE_KIND := DEFAULT_DICTIONARY_FILE_KIND)
                       return STEM_IO.COUNT is
    WD : constant STRING := TRIM(INPUT_WORD);
  begin        --  remember the string may not start at 1
    if D_K = LOCAL  then
      return DDLL(WD(WD'FIRST), 'a', D_K);
    elsif WD'LENGTH < 2 then
      return   0; --  BDLL(WD(WD'FIRST), ' ', D_K);
    else
      return DDLL(WD(WD'FIRST), WD(WD'FIRST+1), D_K);
    end if;
  end  LAST_INDEX;

--procedure PUT_INDICES(CH : STRING; 
--                        D_K : DICTIONARY_KIND) is
--    WD : STRING(1..2) := CH(1..2);
--  begin
--    if CH = "  "  then
--      if BBLL(CH(1), CH(2), D_K) >= BBLF(CH(1), CH(2), D_K)  then
--        PUT("CH = ("); PUT(CH); PUT(") index is of range  "); 
--        PUT(BBLF(CH(1), CH(2), D_K)); PUT(".."); PUT(BBLL(CH(1), CH(2), D_K)); 
--        PUT("    number ");
--        PUT(BBLL(CH(1), CH(2), D_K) - BBLF(CH(1), CH(2), D_K) + 1);
--      end if;
--    elsif CH(2) = ' '  then
--      if BDLL(CH(1), CH(2), D_K) >= BDLF(CH(1), CH(2), D_K)  then
--        PUT("CH = ("); PUT(CH); PUT(") index is of range  "); 
--        PUT(BDLF(CH(1), CH(2), D_K)); PUT(".."); PUT(BDLL(CH(1), CH(2), D_K)); 
--        PUT("    number ");
--        PUT(BDLL(CH(1), CH(2), D_K) - BDLF(CH(1), CH(2), D_K) + 1);
--      end if;
--    else
--      if LAST_INDEX(WD, D_K) >= FIRST_INDEX(WD, D_K)  then
--        PUT("CH = ("); PUT(WD); PUT(") index is of range  "); 
--        PUT(FIRST_INDEX(WD, D_K)); PUT(".."); PUT(LAST_INDEX(WD, D_K)); 
--        PUT("    number ");
--        PUT(LAST_INDEX(WD, D_K) - FIRST_INDEX(WD, D_K) + 1);
--      end if;
--    end if;
--    NEW_LINE;
--  end PUT_INDICES;


  procedure LOAD_BDL_FROM_DISK is
    use STEM_IO;
    DS : DICTIONARY_STEM;
    INDEX_FIRST,
    INDEX_LAST  : STEM_IO.COUNT := 0;
    K : INTEGER := 0;
  begin

--PUT_LINE("LOADING BDL FROM DISK");
if DICTIONARY_AVAILABLE(GENERAL)  then
--  The blanks are on the GENERAL dictionary
  LOADING_BDL_FROM_DISK:
  declare
      D_K : DICTIONARY_KIND := GENERAL;
  begin
      if not IS_OPEN(STEM_FILE(D_K))  then
  --TEXT_IO.PUT_LINE("LOADING_BDL is going to OPEN " &  
  --ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME, 
  --DICTIONARY_KIND'IMAGE(D_K)));
        OPEN(STEM_FILE(D_K), STEM_IO.IN_FILE,
                             ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                             DICTIONARY_KIND'IMAGE(D_K)));
  --TEXT_IO.PUT_LINE("OPENing was successful");
      end if;
  --TEXT_IO.PUT_LINE("BDL OPEN");
      INDEX_FIRST := BBLF(' ', ' ', DICTIONARY_KIND(D_K));
      INDEX_LAST  := BBLL(' ', ' ', DICTIONARY_KIND(D_K));

      SET_INDEX(STEM_FILE(D_K), STEM_IO.POSITIVE_COUNT(INDEX_FIRST));
      for J in INDEX_FIRST..INDEX_LAST  loop
        READ(STEM_FILE(D_K), DS);
        K := K + 1;
        BDL(K) := DS;
      end loop;
      CLOSE(STEM_FILE(D_K));
--TEXT_IO.PUT_LINE("BDL LOADED FROM DISK   K = " & INTEGER'IMAGE(K));
  exception
    when NAME_ERROR =>
      TEXT_IO.PUT_LINE("LOADING BDL FROM DISK had NAME_ERROR on " &
        ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                               DICTIONARY_KIND'IMAGE(D_K)));
      TEXT_IO.PUT_LINE("The will be no blank stems loaded");
    when USE_ERROR =>
      TEXT_IO.PUT_LINE("LOADING BDL FROM DISK had USE_ERROR on " &
        ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                               DICTIONARY_KIND'IMAGE(D_K)));
      TEXT_IO.PUT_LINE("There will be no blank stems loaded");
  end LOADING_BDL_FROM_DISK;
end if;

    --  Now load the stems of just one letter
    for D_K in GENERAL..DICTIONARY_KIND'LAST loop
      if DICTIONARY_AVAILABLE(D_K)  then
        exit when D_K = LOCAL;
--TEXT_IO.PUT_LINE("OPENING BDL STEMFILE " & EXT(D_K));
        if not IS_OPEN(STEM_FILE(D_K))  then
--PUT_LINE("LOADING_BDL is going to OPEN " &  
--ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME, 
--DICTIONARY_KIND'IMAGE(D_K)));
          OPEN(STEM_FILE(D_K), STEM_IO.IN_FILE,
                               ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                               DICTIONARY_KIND'IMAGE(D_K)));
--STEMFILE." & EXT(D_K));
--PUT_LINE("OPENing was successful");
        end if;
        for I in CHARACTER range 'a'..'z'  loop
          INDEX_FIRST := BDLF(I, ' ', D_K);
          INDEX_LAST  := BDLL(I, ' ', D_K);
          if INDEX_FIRST > 0  then
            SET_INDEX(STEM_FILE(D_K), STEM_IO.POSITIVE_COUNT(INDEX_FIRST));
            for J in INDEX_FIRST..INDEX_LAST  loop
              READ(STEM_FILE(D_K), DS);
             K := K + 1;
              BDL(K) := DS;
            end loop;
          end if;
        end loop;
--TEXT_IO.PUT_LINE("Single letters LOADED FROM DISK   K = " & INTEGER'IMAGE(K));
        CLOSE(STEM_FILE(D_K));
      end if;

    end loop;
    BDL_LAST := K;

--TEXT_IO.PUT("FINISHED LOADING BDL FROM DISK     BDL_LAST = ");
--TEXT_IO.PUT(INTEGER'IMAGE(BDL_LAST));
--TEXT_IO.NEW_LINE;

  end LOAD_BDL_FROM_DISK;




  procedure LOAD_INDICES_FROM_INDX_FILE(INDXFILE_NAME : STRING;
                                        D_K : DICTIONARY_KIND) is
    use TEXT_IO;
    use INFLECTIONS_PACKAGE.INTEGER_IO;
    use STEM_IO;
    use COUNT_IO;
    CH : STRING(1..2);
    M, N : STEM_IO.COUNT;
    NUMBER_OF_BLANK_STEMS,
    NUMBER_OF_NON_BLANK_STEMS : STEM_IO.COUNT := 0;
    S : STRING(1..100) := (others => ' ');
    LAST, L : INTEGER := 0;

    function MAX(A, B : STEM_IO.COUNT) return STEM_IO.COUNT is
    begin
      if A >= B then  return A; end if; return B;
    end MAX;

  begin
    OPEN(INDX_FILE(D_K), TEXT_IO.IN_FILE,
                         ADD_FILE_NAME_EXTENSION(INDX_FILE_NAME,
                         DICTIONARY_KIND'IMAGE(D_K)));
 --"INDXFILE." & EXT(D_K)); --  $$$$$$$$$$$$

    PREFACE.PUT(DICTIONARY_KIND'IMAGE(D_K));
    PREFACE.PUT(" Dictionary loading");

    if D_K = GENERAL  then
      GET_LINE(INDX_FILE(D_K), S, LAST);
      CH := S(1..2);
      GET(S(4..LAST), M, L);
      BBLF(CH(1), CH(2), D_K) := M;
      GET(S(L+1..LAST), N, L);
      BBLL(CH(1), CH(2), D_K) := N;
      NUMBER_OF_BLANK_STEMS := MAX(NUMBER_OF_BLANK_STEMS, N);
    end if;

    while not END_OF_FILE(INDX_FILE(D_K))  loop
      GET_LINE(INDX_FILE(D_K), S, LAST);
      exit when LAST = 0;
      CH := S(1..2);
      GET(S(4..LAST), M, L);
      if CH(2) = ' '  then
        BDLF(CH(1), CH(2), D_K) := M;
      else
        DDLF(CH(1), CH(2), D_K) := M;
      end if;
      GET(S(L+1..LAST), N, L);
      if CH(2) = ' '  then
        BDLL(CH(1), CH(2), D_K) := N;
        NUMBER_OF_BLANK_STEMS := MAX(NUMBER_OF_BLANK_STEMS, N);
      else
        DDLL(CH(1), CH(2), D_K) := N;
        NUMBER_OF_NON_BLANK_STEMS := MAX(NUMBER_OF_NON_BLANK_STEMS, N);
      end if;
    end loop;
    CLOSE(INDX_FILE(D_K));
    PREFACE.SET_COL(33); PREFACE.PUT("--  ");
        if not CONFIG.SUPPRESS_PREFACE  then
          PUT(STEM_IO.COUNT((NUMBER_OF_NON_BLANK_STEMS)), 6);
        end if;   --  Kludge for when TEXT_IO.COUNT too small
    PREFACE.PUT(" stems");
    PREFACE.SET_COL(55); PREFACE.PUT_LINE("--  Loaded correctly");
  end LOAD_INDICES_FROM_INDX_FILE;


end WORD_SUPPORT_PACKAGE;
