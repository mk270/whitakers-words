with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with PREFACE;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
pragma ELABORATE(INFLECTIONS_PACKAGE);
pragma ELABORATE(DICTIONARY_PACKAGE);
package body ADDONS_PACKAGE is
  use TEXT_IO;
  use PART_OF_SPEECH_TYPE_IO;
  use TARGET_ENTRY_IO;
  use PART_ENTRY_IO;
  --use KIND_ENTRY_IO;
  use STEM_KEY_TYPE_IO;

  function EQU(C, D : CHARACTER) return BOOLEAN is
  begin
    if (D = 'u') or (D = 'v')  then
      if (C = 'u') or (C = 'v')  then
        return TRUE;
      else
        return FALSE;
      end if;
    else
      return C = D;
    end if;
  end EQU;

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


  procedure LOAD_ADDONS (FILE_NAME : in STRING) is
    use PART_OF_SPEECH_TYPE_IO;
    use TACKON_ENTRY_IO;
    use PREFIX_ENTRY_IO;
    use SUFFIX_ENTRY_IO;
    --use DICT_IO;

    S : STRING(1..100);
    L, LAST, TIC, PRE, SUF, TAC, PAC : INTEGER := 0;
    ADDONS_FILE : TEXT_IO.FILE_TYPE;
    D_K : constant DICTIONARY_KIND := ADDONS;
    POFS: PART_OF_SPEECH_TYPE;
    DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
    MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    M : INTEGER := 1;
    --TG : TARGET_ENTRY;
    TN : TACKON_ENTRY;
    PM : PREFIX_ITEM;
    TS : STEM_TYPE;

    procedure GET_NO_COMMENT_LINE(F : in TEXT_IO.FILE_TYPE;
                                   S : out STRING; LAST : out INTEGER) is
      T : STRING(1..250) := (others => ' ');
      L : INTEGER := 0;
    begin
      LAST := 0;
      while not END_OF_FILE(F)  loop
        GET_LINE(F, T, L);
        if L >= 2  and then
           (HEAD(TRIM(T), 250)(1..2) = "--"  or
            HEAD(TRIM(T), 250)(1..2) = "  ")  then
          null;
        else
          S(1..L) := T(1..L);
          LAST := L;
          exit;
        end if;
      end loop;
    end GET_NO_COMMENT_LINE;

    procedure EXTRACT_FIX(S : in STRING;
                       XFIX : out FIX_TYPE; XC : out CHARACTER) is
      ST : constant STRING := TRIM(S);
      L : INTEGER := ST'LENGTH;
      J : INTEGER := 0;
    begin
      for I in 1..L  loop
        J := I;
        exit when ( (I < L) and then (ST(I+1) = ' ') );
      end loop;
      XFIX := HEAD(ST(1..J), MAX_FIX_SIZE);
      if J = L  then     --  there is no CONNECT CHARACTER
        XC := ' ';
        return;
      else
        for I in J+1..L  loop
          if ST(I) /= ' '  then
            XC := ST(I);
            exit;
          end if;
        end loop;
      end if;
      return;
    end EXTRACT_FIX;

  begin
    OPEN(ADDONS_FILE, IN_FILE, FILE_NAME);
    PREFACE.PUT("ADDONS");
    PREFACE.PUT(" loading ");

--    if DICT_IO.IS_OPEN(DICT_FILE(D_K))  then
--      DICT_IO.DELETE(DICT_FILE(D_K));
--    end if;
--    DICT_IO.CREATE(DICT_FILE(D_K), DICT_IO.INOUT_FILE,
--          --ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, DICTIONARY_KIND'IMAGE(D_K)));
--       "");
--       
    while not END_OF_FILE(ADDONS_FILE)  loop                                 

      DE := NULL_DICTIONARY_ENTRY;
      GET_NO_COMMENT_LINE(ADDONS_FILE, S, LAST);
--TEXT_IO.PUT_LINE(S(1..LAST));
      GET(S(1..LAST), POFS, L);
      case POFS is
        when TACKON  =>
            TS := HEAD(TRIM(S(L+1..LAST)), MAX_STEM_SIZE);
            DE.STEMS(1) := TS;
            
            GET_LINE(ADDONS_FILE, S, LAST);
            GET(S(1..LAST), TN, L);
            GET_LINE(ADDONS_FILE, S, LAST);
            MEAN := HEAD(S(1..LAST), MAX_MEANING_SIZE);

          if  TN.BASE.POFS= PACK   and then
             (TN.BASE.PACK.DECL.WHICH = 1 or
              TN.BASE.PACK.DECL.WHICH = 2)  and then
              MEAN(1..9) = "PACKON w/"  then
            PAC := PAC + 1;
            PACKONS (PAC).POFS:= POFS;
            PACKONS(PAC).TACK := TS;
            PACKONS(PAC).ENTR := TN;
--            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
--            DE.MEAN := MEAN;
--            DICT_IO.WRITE(DICT_FILE(D_K), DE);
            PACKONS (PAC).MNPC := M;
            MEANS(M) := MEAN;
            M := M + 1;

          else
            TAC := TAC + 1;
            TACKONS (TAC).POFS:= POFS;
            TACKONS(TAC).TACK := TS;
            TACKONS(TAC).ENTR := TN;
--            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
--            DE.MEAN := MEAN;
--            DICT_IO.WRITE(DICT_FILE(D_K), DE);
--            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
            TACKONS (TAC).MNPC := M;
            MEANS(M) := MEAN;
            M := M + 1;
          end if;

          NUMBER_OF_PACKONS  := PAC;
          NUMBER_OF_TACKONS  := TAC;

        when PREFIX  =>

          EXTRACT_FIX(S(L+1..LAST), PM.FIX, PM.CONNECT);
          GET_LINE(ADDONS_FILE, S, LAST);
          GET(S(1..LAST), PM.ENTR, L);
          GET_LINE(ADDONS_FILE, S, LAST);
          MEAN := HEAD(S(1..LAST), MAX_MEANING_SIZE);


          if  PM.ENTR.ROOT = PACK     then
            TIC := TIC + 1;
            TICKONS (TIC).POFS:= POFS;
            TICKONS(TIC).FIX  := PM.FIX;
            TICKONS(TIC).CONNECT  := PM.CONNECT;
            TICKONS(TIC).ENTR := PM.ENTR;
--            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
--            DE.MEAN := MEAN;
--            DICT_IO.WRITE(DICT_FILE(D_K), DE);
--            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
            TICKONS (TIC).MNPC := M;
            MEANS(M) := MEAN;
            M := M + 1;
 
 
          else
            PRE := PRE + 1;
            PREFIXES(PRE).POFS:= POFS;
            PREFIXES(PRE).FIX  := PM.FIX;
            PREFIXES(PRE).CONNECT  := PM.CONNECT;
            PREFIXES(PRE).ENTR := PM.ENTR;
--            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
            DE.MEAN := MEAN;
--            DICT_IO.WRITE(DICT_FILE(D_K), DE);
--            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
            PREFIXES(PRE).MNPC := M;
            MEANS(M) := MEAN;
            M := M + 1;
          end if;

          NUMBER_OF_TICKONS  := TIC;
          NUMBER_OF_PREFIXES := PRE;

        when SUFFIX  =>
        SUF := SUF + 1;
        SUFFIXES(SUF).POFS:= POFS;
--TEXT_IO.PUT_LINE(S(1..LAST));
       EXTRACT_FIX(S(L+1..LAST), SUFFIXES(SUF).FIX, SUFFIXES(SUF).CONNECT);
--TEXT_IO.PUT("@1");
       GET_LINE(ADDONS_FILE, S, LAST);
--TEXT_IO.PUT("@2");
--TEXT_IO.PUT_LINE(S(1..LAST) & "<");
--TEXT_IO.PUT("@2");
       GET(S(1..LAST), SUFFIXES(SUF).ENTR, L);
--TEXT_IO.PUT("@3");
     GET_LINE(ADDONS_FILE, S, LAST);
--TEXT_IO.PUT("@4");
      MEAN := HEAD(S(1..LAST), MAX_MEANING_SIZE);
--TEXT_IO.PUT("@5");
--
--        DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
--        DE.MEAN := MEAN;
--        DICT_IO.WRITE(DICT_FILE(D_K), DE);
--        --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
        SUFFIXES(SUF).MNPC := M;
        MEANS(M) := MEAN;
        M := M + 1;

        NUMBER_OF_SUFFIXES := SUF;

        when others  =>
          TEXT_IO.PUT_LINE("Bad ADDON    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
          TEXT_IO.PUT_LINE(S(1..LAST));
          raise TEXT_IO.DATA_ERROR;
      end case;

    end loop;

    PREFACE.PUT(TAC, 1); PREFACE.PUT("+");
    PREFACE.PUT(PAC, 2); PREFACE.PUT(" TACKONS ");
    PREFACE.PUT(TIC, 1); PREFACE.PUT("+");
    PREFACE.PUT(PRE, 3); PREFACE.PUT(" PREFIXES ");
    PREFACE.PUT(SUF, 3); PREFACE.PUT(" SUFFIXES ");

    PREFACE.SET_COL(60); PREFACE.PUT_LINE("--  Loaded correctly");
    CLOSE(ADDONS_FILE);

--for I in MEANS'RANGE  loop
--  TEXT_IO.PUT(INTEGER'IMAGE(INTEGER(I))); TEXT_IO.PUT_LINE("--" & MEANS(I));
--end loop;  
  

  
    exception
    when TEXT_IO.NAME_ERROR  =>
      PREFACE.PUT_LINE("No ADDONS file ");
      null;
    when TEXT_IO.DATA_ERROR  =>
      PREFACE.PUT_LINE(S(1..LAST));
      PREFACE.PUT_LINE("No further ADDONS read ");
      CLOSE(ADDONS_FILE);
    when others      =>
      PREFACE.PUT_LINE("Exception in LOAD_ADDONS");
      PREFACE.PUT_LINE(S(1..LAST));
 end LOAD_ADDONS;
          
          
          

  function SUBTRACT_TACKON(W : STRING; X : TACKON_ITEM) return STRING is
    WD : constant STRING := TRIM(W);
    L  : constant INTEGER := WD'LENGTH;
    XF : constant STRING := TRIM(X.TACK);
    Z  : constant INTEGER := XF'LENGTH;
  begin
--PUT_LINE("In SUB TACKON " & INTEGER'IMAGE(L) & INTEGER'IMAGE(Z));
    if WORDS_MDEV(USE_TACKONS) and then
      L > Z  and then
       --WD(L-Z+1..L) = XF(1..Z)  then
       EQU(WD(L-Z+1..L),  XF(1..Z)) then
--PUT("In SUBTRACT_TACKON we got a hit   "); PUT_LINE(X.TACK);
      return WD(1..L-Z);
    else
--PUT("In SUBTRACT_TACKON    NO    hit   "); PUT_LINE(X.TACK);
      return W;
    end if;
  end SUBTRACT_TACKON;

  function SUBTRACT_PREFIX(W : STRING; X : PREFIX_ITEM) return STEM_TYPE is
    WD : constant STRING := TRIM(W);
    XF : constant STRING := TRIM(X.FIX);
    Z  : constant INTEGER := XF'LENGTH;
    ST : STEM_TYPE := HEAD(WD, MAX_STEM_SIZE);
  begin
    if WORDS_MDEV(USE_PREFIXES) and then
       X /= NULL_PREFIX_ITEM and then
       WD'LENGTH > Z  and then
       --WD(1..Z) = XF(1..Z)  and then
       EQU(WD(1..Z),  XF(1..Z)) and then
    ( (X.CONNECT = ' ') or (WD(Z+1) = X.CONNECT) )  then
      ST(1..WD'LENGTH-Z) := WD(Z+1..WD'LAST);
      ST(WD'LENGTH-Z+1..MAX_STEM_SIZE) :=
          NULL_STEM_TYPE(WD'LENGTH-Z+1..MAX_STEM_SIZE);
    end if;
--PUT_LINE("SUBTRACT_PREFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
    return ST;
  end SUBTRACT_PREFIX;

  function SUBTRACT_SUFFIX(W : STRING; X : SUFFIX_ITEM) return STEM_TYPE is
    WD : constant STRING := TRIM(W);
    L  : constant INTEGER := WD'LENGTH;
    XF : constant STRING := TRIM(X.FIX);
    Z  : constant INTEGER := XF'LENGTH;
    ST : STEM_TYPE := HEAD(WD, MAX_STEM_SIZE);
  begin
--PUT_LINE("In SUBTRACT_SUFFIX  Z = " & INTEGER'IMAGE(Z) & 
--"  CONNECT >" & X.CONNECT & '<');
    if WORDS_MDEV(USE_SUFFIXES) and then
      X /= NULL_SUFFIX_ITEM and then
       WD'LENGTH > Z  and then
       --WD(L-Z+1..L) = XF(1..Z)  and then
       EQU(WD(L-Z+1..L),  XF(1..Z))  and then
    ( (X.CONNECT = ' ') or (WD(L-Z) = X.CONNECT) )  then
--PUT_LINE("In SUBTRACT_SUFFIX we got a hit");
      ST(1..WD'LENGTH-Z) := WD(1..WD'LENGTH-Z);
      ST(WD'LENGTH-Z+1..MAX_STEM_SIZE) :=
          NULL_STEM_TYPE(WD'LENGTH-Z+1..MAX_STEM_SIZE);
    end if;
--PUT_LINE("SUBTRACT_SUFFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
    return ST;
  end SUBTRACT_SUFFIX;

  function ADD_PREFIX(STEM : STEM_TYPE;
                    PREFIX : PREFIX_ITEM) return STEM_TYPE is
    FPX : constant STRING := TRIM(PREFIX.FIX) & STEM;
  begin
    if WORDS_MDEV(USE_PREFIXES)  then
      return HEAD(FPX, MAX_STEM_SIZE);
    else
      return STEM;
    end if;
  end ADD_PREFIX;

  function ADD_SUFFIX(STEM : STEM_TYPE;
                    SUFFIX : SUFFIX_ITEM) return STEM_TYPE is
    FPX : constant STRING := TRIM(STEM) & SUFFIX.FIX;
  begin
    if WORDS_MDEV(USE_SUFFIXES)  then
      return HEAD(FPX, MAX_STEM_SIZE);
    else
      return STEM;
    end if;
  end ADD_SUFFIX;


--  package body TARGET_ENTRY_IO is separate;

--  package body TACKON_ENTRY_IO is separate;

--  package body TACKON_LINE_IO is separate;

--  package body PREFIX_ENTRY_IO is separate;

--  package body PREFIX_LINE_IO is separate;

--  package body SUFFIX_ENTRY_IO is separate;

--  package body SUFFIX_LINE_IO is separate;


  package body TARGET_ENTRY_IO is
  use PART_OF_SPEECH_TYPE_IO;
  use NOUN_ENTRY_IO;
  use PRONOUN_ENTRY_IO;
  use PROPACK_ENTRY_IO;
  use ADJECTIVE_ENTRY_IO;
  use NUMERAL_ENTRY_IO;
  use ADVERB_ENTRY_IO;
  use VERB_ENTRY_IO;
--  use KIND_ENTRY_IO;
--
--  use NOUN_KIND_TYPE_IO;
--  use PRONOUN_KIND_TYPE_IO;
--  use INFLECTIONS_PACKAGE.INTEGER_IO;
--  use VERB_KIND_TYPE_IO;
  
  SPACER : CHARACTER := ' ';

  NOUN  : NOUN_ENTRY;
  PRONOUN : PRONOUN_ENTRY;
  PROPACK : PROPACK_ENTRY;
  ADJECTIVE : ADJECTIVE_ENTRY;
  NUMERAL : NUMERAL_ENTRY;
  ADVERB : ADVERB_ENTRY;
  VERB : VERB_ENTRY;

--  NOUN_KIND  : NOUN_KIND_TYPE;
--  PRONOUN_KIND : PRONOUN_KIND_TYPE;
--  PROPACK_KIND : PRONOUN_KIND_TYPE;
--  NUMERAL_VALUE : NUMERAL_VALUE_TYPE;
--  VERB_KIND : VERB_KIND_TYPE;
  
  --KIND : KIND_ENTRY;

  P : TARGET_ENTRY;


  procedure GET(F : in FILE_TYPE; P : out TARGET_ENTRY) is
    PS : TARGET_POFS_TYPE := X;
  begin
    GET(F, PS);
    GET(F, SPACER);
    case PS is
      when N =>
        GET(F, NOUN);
        --GET(F, NOUN_KIND);
        P := (N, NOUN);  --, NOUN_KIND);
      when PRON =>
        GET(F, PRONOUN);
        --GET(F, PRONOUN_KIND);
        P := (PRON, PRONOUN);  --, PRONOUN_KIND);
      when PACK =>
        GET(F, PROPACK);
        --GET(F, PROPACK_KIND);
        P := (PACK, PROPACK);  --, PROPACK_KIND);
      when ADJ =>
        GET(F, ADJECTIVE);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(F, NUMERAL);
        --GET(F, NUMERAL_VALUE);
        P := (NUM, NUMERAL);  --, NUMERAL_VALUE);
      when ADV =>
        GET(F, ADVERB);
        P := (ADV, ADVERB);
      when V =>
        GET(F, VERB);
        --GET(F, VERB_KIND);
        P := (V, VERB);  --, VERB_KIND);
      when X =>
        P := (POFS=> X);
    end case;
    return;
  end GET;

  procedure GET(P : out TARGET_ENTRY) is
    PS : TARGET_POFS_TYPE := X;
  begin
    GET(PS);
    GET(SPACER);
    case PS is
      when N =>
        GET(NOUN);
        --GET(NOUN_KIND);
        P := (N, NOUN);  --, NOUN_KIND);
      when PRON =>
        GET(PRONOUN);
        --GET(PRONOUN_KIND);
        P := (PRON, PRONOUN);  --, PRONOUN_KIND);
      when PACK =>
        GET(PROPACK);
        --GET(PROPACK_KIND);
        P := (PACK, PROPACK);  --, PROPACK_KIND);
      when ADJ =>
        GET(ADJECTIVE);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(NUMERAL);
        --GET(NUMERAL_VALUE);
        P := (NUM, NUMERAL);  --, NUMERAL_VALUE);
      when ADV =>
        GET(ADVERB);
        P := (ADV, ADVERB);
      when V =>
        GET(VERB);
        --GET(VERB_KIND);
        P := (V, VERB);  --, VERB_KIND);
      when X =>
        P := (POFS=> X);
    end case;
    return;
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in TARGET_ENTRY) is
    C : POSITIVE := POSITIVE(COL(F));
  begin
    PUT(F, P.POFS);
    PUT(F, ' ');
    case P.POFS is
      when N =>
        PUT(F, P.N);
        --PUT(F, P.NOUN_KIND);
      when PRON =>
        PUT(F, P.PRON);
        --PUT(F, P.PRONOUN_KIND);
      when PACK =>
        PUT(F, P.PACK);
        --PUT(F, P.PROPACK_KIND);
      when ADJ =>
        PUT(F, P.ADJ);
      when NUM =>
        PUT(F, P.NUM);
        --PUT(F, P.NUMERAL_VALUE);
      when ADV =>
        PUT(F, P.ADV);
      when V =>
        PUT(F, P.V);
        --PUT(F, P.VERB_KIND);
      when others =>
        null;
    end case;
    PUT(F, STRING'((INTEGER(COL(F))..TARGET_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
  return;
  end PUT;


  procedure PUT(P : in TARGET_ENTRY) is
    C : POSITIVE := POSITIVE(COL);
  begin
    PUT(P.POFS);
    PUT(' ');
    case P.POFS is
      when N =>
        PUT(P.N);
        --PUT(P.NOUN_KIND);
      when PRON =>
        PUT(P.PRON);
        --PUT(P.PRONOUN_KIND);
      when PACK =>
        PUT(P.PACK);
        --PUT(P.PROPACK_KIND);
      when ADJ =>
        PUT(P.ADJ);
      when NUM =>
        PUT(P.NUM);
        --PUT(P.NUMERAL_VALUE);
      when ADV =>
        PUT(P.ADV);
      when V =>
        PUT(P.V);
        --PUT(P.VERB_KIND);
      when others =>
        null;
    end case;
    PUT(STRING'((INTEGER(COL)..TARGET_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
    return;
  end PUT;

  procedure GET(S : in STRING; P : out TARGET_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
    PS : TARGET_POFS_TYPE := X;
  begin
    GET(S, PS, L);
    L := L + 1;
    case PS is
      when N =>
        GET(S(L+1..S'LAST), NOUN, LAST);
        --GET(S(L+1..S'LAST), NOUN_KIND, LAST);
        P := (N, NOUN);  --, NOUN_KIND);
      when PRON =>
        GET(S(L+1..S'LAST), PRONOUN, LAST);
        --GET(S(L+1..S'LAST), PRONOUN_KIND, LAST);
        P := (PRON, PRONOUN);  --, PRONOUN_KIND);
      when PACK =>
        GET(S(L+1..S'LAST), PROPACK, LAST);
        --GET(S(L+1..S'LAST), PROPACK_KIND, LAST);
        P := (PACK, PROPACK);  --, PROPACK_KIND);
      when ADJ =>
        GET(S(L+1..S'LAST), ADJECTIVE, LAST);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(S(L+1..S'LAST), NUMERAL, LAST);
        --GET(S(L+1..S'LAST), NUMERAL_VALUE, LAST);
        P := (NUM, NUMERAL);  --, NUMERAL_VALUE);
      when ADV =>  
        GET(S(L+1..S'LAST), ADVERB, LAST);
        P := (ADV, ADVERB);
      when V =>
        GET(S(L+1..S'LAST), VERB, LAST);
        --GET(S(L+1..S'LAST), VERB_KIND, LAST);
        P := (V, VERB);  --, VERB_KIND);
      when X =>
        P := (POFS=> X);
    end case;
    return;
  end GET;


  procedure PUT(S : out STRING; P : in TARGET_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.POFS);
    L := M + 1;
    S(L) :=  ' ';
    case P.POFS is
      when N =>
        M := L + NOUN_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.N);
--        M := L + NOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.NOUN_KIND);
      when PRON =>
        M := L + PRONOUN_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PRON);
--        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PRONOUN_KIND);
      when PACK =>
        M := L + PROPACK_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PACK);
--        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PROPACK_KIND);
      when ADJ =>
        M := L + ADJECTIVE_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.ADJ);
      when NUM =>
        M := L + NUMERAL_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.NUM);
--        M := L + NUMERAL_VALUE_TYPE_IO_DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PRONOUN_KIND);
      when ADV =>
        M := L + ADVERB_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.ADV);
      when V =>
        M := L + VERB_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.V);
--        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PRONOUN_KIND);
      when others =>
        null;
    end case;
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end TARGET_ENTRY_IO;


package body TACKON_ENTRY_IO is
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; I : out TACKON_ENTRY) is
  begin
    GET(F, I.BASE);
  end GET;

  procedure GET(I : out TACKON_ENTRY) is
  begin
    GET(I.BASE);
  end GET;

  procedure PUT(F : in FILE_TYPE; I : in TACKON_ENTRY) is
  begin
    PUT(F, I.BASE);
  end PUT;

  procedure PUT(I : in TACKON_ENTRY) is
  begin
    PUT(I.BASE);
  end PUT;

  procedure GET(S : in STRING; I : out TACKON_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), I.BASE, LAST);
  end GET;

  procedure PUT(S : out STRING; I : in TACKON_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), I.BASE);
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end TACKON_ENTRY_IO;


  package body PREFIX_ENTRY_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';

    PE : PREFIX_ENTRY;

    procedure GET(F : in FILE_TYPE; P : out PREFIX_ENTRY) is
    begin
      GET(F, P.ROOT);
      GET(F, SPACER);
      GET(F, P.TARGET);
     end GET;


    procedure GET(P : out PREFIX_ENTRY) is
    begin
      GET(P.ROOT);
      GET(SPACER);
      GET(P.TARGET);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in PREFIX_ENTRY) is
    begin
      PUT(F, P.ROOT);
      PUT(F, ' ');
      PUT(F, P.TARGET);
     end PUT;

    procedure PUT(P : in PREFIX_ENTRY) is
    begin
      PUT(P.ROOT);
      PUT(' ');
      PUT(P.TARGET);
     end PUT;

    procedure GET(S : in STRING; P : out PREFIX_ENTRY; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
    begin
      GET(S(L+1..S'LAST), P.ROOT, L);
      L := L + 1;
      GET(S(L+1..S'LAST), P.TARGET, LAST);
    end GET;


    procedure PUT(S : out STRING; P : in PREFIX_ENTRY) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ROOT);
      L := M + 1;
      S(L) :=  ' ';
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.TARGET);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end PREFIX_ENTRY_IO;



  package body SUFFIX_ENTRY_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use TARGET_ENTRY_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';

    PE : SUFFIX_ENTRY;

    procedure GET(F : in FILE_TYPE; P : out SUFFIX_ENTRY) is
    begin
      GET(F, P.ROOT);
      GET(F, SPACER);
      GET(F, P.ROOT_KEY);
      GET(F, SPACER);
      GET(F, P.TARGET);
      GET(F, SPACER);
      GET(F, P.TARGET_KEY);
     end GET;


    procedure GET(P : out SUFFIX_ENTRY) is
    begin
      GET(P.ROOT);
      GET(SPACER);
      GET(P.ROOT_KEY);
      GET(SPACER);
      GET(P.TARGET);
      GET(SPACER);
      GET(P.TARGET_KEY);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_ENTRY) is
    begin
      PUT(F, P.ROOT);
      PUT(F, ' ');
      PUT(F, P.ROOT_KEY, 2);
      PUT(F, ' ');
      PUT(F, P.TARGET);
      PUT(F, ' ');
      PUT(F, P.TARGET_KEY, 2);
     end PUT;

    procedure PUT(P : in SUFFIX_ENTRY) is
    begin
      PUT(P.ROOT);
      PUT(' ');
      PUT(P.ROOT_KEY, 2);
      PUT(' ');
      PUT(P.TARGET);
      PUT(' ');
      PUT(P.TARGET_KEY, 2);
     end PUT;

    procedure GET(S : in STRING; P : out SUFFIX_ENTRY; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
    begin
--TEXT_IO.PUT("#1" & INTEGER'IMAGE(L));
      GET(S(L+1..S'LAST), P.ROOT, L);
--TEXT_IO.PUT("#2" & INTEGER'IMAGE(L));
      L := L + 1;
      GET(S(L+1..S'LAST), P.ROOT_KEY, L);
--TEXT_IO.PUT("#3" & INTEGER'IMAGE(L));
      L := L + 1;
      GET(S(L+1..S'LAST), P.TARGET, L);
--TEXT_IO.PUT("#4" & INTEGER'IMAGE(L));
      L := L + 1;
      GET(S(L+1..S'LAST), P.TARGET_KEY, LAST);
--TEXT_IO.PUT("#5" & INTEGER'IMAGE(LAST));
    end GET;


    procedure PUT(S : out STRING; P : in SUFFIX_ENTRY) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ROOT);
      L := M + 1;
      S(L) :=  ' ';
      M := L + 2;
      PUT(S(L+1..M), P.ROOT_KEY);
      L := M + 1;
      S(L) :=  ' ';
      M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.TARGET);
      L := M + 1;
      S(L) :=  ' ';
      M := L + 2;
      PUT(S(L+1..M), P.TARGET_KEY);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end SUFFIX_ENTRY_IO;



 begin    --  Initiate body of ADDONS_PACKAGE
--TEXT_IO.PUT_LINE("Initializing ADDONS_PACKAGE");

  PREFIX_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                   PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
  TARGET_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                   NUMERAL_ENTRY_IO.DEFAULT_WIDTH; --  Largest

  SUFFIX_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                   2 + 1 +
                                   TARGET_ENTRY_IO.DEFAULT_WIDTH + 1 +
                                   2;
  TACKON_ENTRY_IO.DEFAULT_WIDTH := TARGET_ENTRY_IO.DEFAULT_WIDTH;

  
  end ADDONS_PACKAGE;
