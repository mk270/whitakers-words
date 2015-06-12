with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;   --  for STEM_IO
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with PREFACE;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with UNIQUES_PACKAGE; use UNIQUES_PACKAGE;
pragma ELABORATE(INFLECTIONS_PACKAGE);
pragma ELABORATE(DICTIONARY_PACKAGE);
pragma ELABORATE(ADDONS_PACKAGE);
package body LINE_STUFF is




  procedure LOAD_DICTIONARY(DICT : in out DICTIONARY;
                            DICTIONARY_FILE_NAME : STRING)  is
  --  For loading a DICTIONARY list from a file
  --  Only used now for DICT.LOC
   
    DICTIONARY_FILE : FILE_TYPE;
    BLK_STEM : constant STEM_TYPE := NULL_STEM_TYPE;
    STS : STEMS_TYPE := NULL_STEMS_TYPE;
    PT  : PART_ENTRY  := NULL_PART_ENTRY;
    TRAN : TRANSLATION_RECORD := NULL_TRANSLATION_RECORD;
    KIND : KIND_ENTRY := NULL_KIND_ENTRY;
    VALUE : NUMERAL_VALUE_TYPE := 0;
    MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    
    FC1, FC2, FC3, FC4 : CHARACTER;

    LINE, ST_LINE, BLANK_LINE : STRING(1..100) := (others => ' ');
    L, LL, LLL, LAST    : INTEGER := 0;
    NUMBER_OF_DICTIONARY_ENTRIES : INTEGER := 0;

    procedure GET_STEM(S : in STRING;
                       STEM : out STEM_TYPE; LAST : out INTEGER) is
      I  : INTEGER := 1;
      L  : INTEGER := S'FIRST;
    begin
      STEM := NULL_STEM_TYPE;
      --  Squeeze left
      while L <= S'LAST and then S(L) = ' '  loop
        L := L + 1;
      end loop;
      --  Count until the first blank
      --  Return that string
      while L <= S'LAST and then S(L) /= ' '  loop
        STEM(I) := S(L);
        I := I + 1;
        L := L + 1;
      end loop;
      --  Return  last 
      LAST := L;

    end GET_STEM;


  begin

    OPEN(DICTIONARY_FILE, IN_FILE, DICTIONARY_FILE_NAME);
    PREFACE.PUT("Dictionary loading");

    while not END_OF_FILE(DICTIONARY_FILE)  loop
--TEXT_IO.PUT_LINE("GETTING");
      ST_LINE := BLANK_LINE;
      GET_NON_COMMENT_LINE(DICTIONARY_FILE, ST_LINE, LAST);      --  STEMS
--TEXT_IO.PUT_LINE("READ STEMS");
  
      LINE := BLANK_LINE;
--TEXT_IO.PUT("1 ");
   GET_NON_COMMENT_LINE(DICTIONARY_FILE, LINE, L);           --  PART
--TEXT_IO.PUT("2 ");
   PART_ENTRY_IO.GET(LINE(1..L), PT, LL);
--TEXT_IO.PUT("3 ");
 ----  KIND_ENTRY_IO.GET(LINE(LL+1..L), PT.POFS, KIND, LL);
--TEXT_IO.PUT("4 ");
   TRANSLATION_RECORD_IO.GET(LINE(LL+1..L), TRAN, LLL);
--TEXT_IO.PUT("5 ");
--TEXT_IO.PUT_LINE("READ PART");
  
--  Specialize for parts
--  If ADV then look if the CO is something other than X
--  If so (like POS) then only that stem is active, and the others => xxx
--  Same for ADJ
--  If the ADJ or ADV stems have different first letters then make them 
--  different dictionary entries  --  Do this in LOAD and in DICT.DIC
--TEXT_IO.PUT_LINE("GETTING STEMS IN LOAD_DICTIONARY");

      STS := NULL_STEMS_TYPE;
      LL := 1;
      --  Extract up to 4 stems                  
      for I in 1..NUMBER_OF_STEMS(PT.POFS)  loop   --  EXTRACT STEMS
        GET_STEM(ST_LINE(LL..LAST), STS(I), LL);
      end loop;
      
      
--for I in 1..NUMBER_OF_STEMS(PT.POFS)  loop  
--  TEXT_IO.PUT(STS(I));      
--end loop;
--TEXT_IO.NEW_LINE;
 
      LINE := BLANK_LINE;
      GET_NON_COMMENT_LINE(DICTIONARY_FILE, LINE, L);         --  MEANING
      MEAN := HEAD(TRIM(LINE(1..L)), MAX_MEANING_SIZE);         
--TEXT_IO.PUT_LINE("READ MEANING");
      
      
      
    --  Now take care of other first letters in a gross way
    FC1 := LOWER_CASE(STS(1)(1));
    FC2 := LOWER_CASE(STS(2)(1));
    FC3 := LOWER_CASE(STS(3)(1));
    FC4 := LOWER_CASE(STS(4)(1));
    if FC1 = 'v'  then FC1 := 'u';  end if;
    if FC1 = 'j'  then FC1 := 'i';  end if;
    if FC2 = 'v'  then FC2 := 'u';  end if;
    if FC2 = 'j'  then FC2 := 'i';  end if;
    if FC3 = 'v'  then FC3 := 'u';  end if;
    if FC3 = 'j'  then FC3 := 'i';  end if;
    if FC4 = 'v'  then FC4 := 'u';  end if;
    if FC4 = 'j'  then FC4 := 'i';  end if;
    if PT.POFS = N  then
      if (STS(2)(1) /= STS(1)(1) and then
         STS(2)(1) /= ' '  and then
         STS(2)(1..3) /= ZZZ_STEM ) then
        DICT(FC1) :=
             new DICTIONARY_ITEM'(( (STS(1), ZZZ_STEM, BLK_STEM, BLK_STEM),
                                  --PT, KIND, TRAN, MEAN), DICT(FC1));
                                    PT, TRAN, MEAN), DICT(FC1));
        DICT(FC2) :=
             new DICTIONARY_ITEM'( ( (ZZZ_STEM, STS(2), BLK_STEM, BLK_STEM),
                                     --PT, KIND, TRAN, MEAN), DICT(FC2) );
                                     PT, TRAN, MEAN), DICT(FC2) );
      else
        --DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
        DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, TRAN, MEAN),
                                                  DICT(FC1));
      end if;

    elsif (PT.POFS = PRON) or (PT.POFS = PACK)  then
      if (STS(2)(1) /= STS(1)(1) and then
         STS(2)(1) /= ' '  and then
         STS(2)(1..3) /= ZZZ_STEM ) then
       DICT(FC1) :=
             new DICTIONARY_ITEM'(( (STS(1), ZZZ_STEM, BLK_STEM, BLK_STEM),
                                    --PT, KIND, TRAN, MEAN), DICT(FC1));
                                    PT, TRAN, MEAN), DICT(FC1));
        DICT(FC2) :=
             new DICTIONARY_ITEM'( ( (ZZZ_STEM, STS(2), BLK_STEM, BLK_STEM),
                                     --PT, KIND, TRAN, MEAN), DICT(FC2) );
                                     PT, TRAN, MEAN), DICT(FC2) );
      else
          --DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
          DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, TRAN, MEAN),
                                                  DICT(FC1));
      end if;

    elsif PT.POFS = ADJ  then
      if PT.ADJ.CO   = X  then   --  X for all KINDs
        if (STS(2)(1) /= STS(1)(1) and then
           STS(2)(1) /= ' '  and then
           STS(2)(1..3) /= ZZZ_STEM ) or
          (STS(3)(1) /= STS(1)(1) and then
           STS(3)(1) /= ' '  and then
           STS(3)(1..3) /= ZZZ_STEM ) or
          (STS(4)(1) /= STS(1)(1) and then
           STS(4)(1) /= ' '  and then
           STS(4)(1..3) /= ZZZ_STEM ) then
          DICT(FC1) :=
                 new DICTIONARY_ITEM'(( (STS(1), BLK_STEM, BLK_STEM, BLK_STEM),
             (ADJ, (PT.ADJ.DECL, POS)),
             --KIND, TRAN, MEAN), DICT(FC1));
             TRAN, MEAN), DICT(FC1));
          DICT(FC2) :=
                 new DICTIONARY_ITEM'( ( (ZZZ_STEM, STS(2), BLK_STEM, BLK_STEM),
            (ADJ, (PT.ADJ.DECL, POS)),
            --KIND, TRAN, MEAN), DICT(FC2) );
            TRAN, MEAN), DICT(FC2) );
          DICT(FC3) :=
                 new DICTIONARY_ITEM'(( (ZZZ_STEM, ZZZ_STEM, STS(3), BLK_STEM),
           (ADJ, (PT.ADJ.DECL, COMP)),
           --KIND, TRAN, MEAN), DICT(FC3));
           TRAN, MEAN), DICT(FC3));
          DICT(FC4) :=
                 new DICTIONARY_ITEM'(( (ZZZ_STEM, ZZZ_STEM, ZZZ_STEM, STS(4)),
           (ADJ, (PT.ADJ.DECL, SUPER)),
           --KIND, TRAN, MEAN), DICT(FC4));
           TRAN, MEAN), DICT(FC4));
        end if;
      elsif PT.ADJ.CO   = POS   then
          DICT(FC1) :=
        new DICTIONARY_ITEM'(( (STS(1), BLK_STEM, BLK_STEM, BLK_STEM),
                             --(ADJ, (PT.ADJ.DECL, POS)), KIND, TRAN, MEAN),
                             (ADJ, (PT.ADJ.DECL, POS)), TRAN, MEAN),
                              DICT(FC1));
          DICT(FC2) :=
        new DICTIONARY_ITEM'(((BLK_STEM,  STS(2), BLK_STEM, BLK_STEM),
                             --(ADJ, (PT.ADJ.DECL, POS)), KIND, TRAN, MEAN),
                             (ADJ, (PT.ADJ.DECL, POS)), TRAN, MEAN),
                              DICT(FC2));
      elsif PT.ADJ.CO   = COMP  then
           DICT(FC1) :=
           new DICTIONARY_ITEM'(( (BLK_STEM, BLK_STEM, STS(1), BLK_STEM),
                       --(ADJ, (PT.ADJ.DECL, COMP)), KIND, TRAN, MEAN),
                       (ADJ, (PT.ADJ.DECL, COMP)), TRAN, MEAN),
                        DICT(FC1));
      elsif PT.ADJ.CO   = SUPER then
          DICT(FC1) :=
            new DICTIONARY_ITEM'(( (BLK_STEM, BLK_STEM, BLK_STEM, STS(1)),
                              --(ADJ, (PT.ADJ.DECL, SUPER)), KIND, TRAN, MEAN),
                              (ADJ, (PT.ADJ.DECL, SUPER)), TRAN, MEAN),
                                DICT(FC1));

      else
          --DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
          DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, TRAN, MEAN),
                                                  DICT(FC1));
      end if;

    elsif PT.POFS = ADV  then
      if PT.ADV.CO   = X  then   --  X for all KINDs
        if (STS(2)(1) /= STS(1)(1) and then
           STS(2)(1) /= ' '  and then
           STS(2)(1..3) /= ZZZ_STEM ) or
          (STS(3)(1) /= STS(1)(1) and then
           STS(3)(1) /= ' '  and then
           STS(3)(1..3) /= ZZZ_STEM ) then
          DICT(FC1) :=
                 new DICTIONARY_ITEM'(( (STS(1), BLK_STEM, BLK_STEM, BLK_STEM),
                             --(ADV, (CO => POS)), KIND, TRAN, MEAN), DICT(FC1));
                             (ADV, (CO => POS)), TRAN, MEAN), DICT(FC1));
          DICT(FC2) :=
                 new DICTIONARY_ITEM'(( (STS(2), BLK_STEM, BLK_STEM, BLK_STEM),
                             --(ADV, (CO => COMP)), KIND, TRAN, MEAN), DICT(FC2));
                             (ADV, (CO => COMP)), TRAN, MEAN), DICT(FC2));
          DICT(FC3) :=
                 new DICTIONARY_ITEM'(( (STS(3), BLK_STEM, BLK_STEM, BLK_STEM),
                            --(ADV, (CO => SUPER)), KIND, TRAN, MEAN), DICT(FC3));
                            (ADV, (CO => SUPER)), TRAN, MEAN), DICT(FC3));
        end if;
      elsif PT.ADV.CO   = POS   then          --  just a specific KIND
        DICT(FC1) :=
        new DICTIONARY_ITEM'(( (STS(1), BLK_STEM, BLK_STEM, BLK_STEM),
                             --(ADV, (CO => POS)), KIND, TRAN, MEAN),
                             (ADV, (CO => POS)), TRAN, MEAN),
                              DICT(FC1));
      elsif PT.ADV.CO   = COMP  then
         DICT(FC1) :=
         new DICTIONARY_ITEM'(( (BLK_STEM, STS(1), BLK_STEM, BLK_STEM),
                     --(ADV, (CO => COMP)), KIND, TRAN, MEAN),
                     (ADV, (CO => COMP)), TRAN, MEAN),
                      DICT(FC1));
      elsif PT.ADV.CO   = SUPER then
        DICT(FC1) :=
          new DICTIONARY_ITEM'(( (BLK_STEM, BLK_STEM, STS(1), BLK_STEM),
                            --(ADV, (CO => SUPER)), KIND, TRAN, MEAN),
                            (ADV, (CO => SUPER)), TRAN, MEAN),
                              DICT(FC1));
      else
          --DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
          DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, TRAN, MEAN),
                                                  DICT(FC1));
      end if;

    elsif PT.POFS = V  then
      if (STS(2)(1) /= STS(1)(1) and then
         STS(2)(1) /= ' '  and then
         STS(2)(1..3) /= ZZZ_STEM ) or
        (STS(3)(1) /= STS(1)(1) and then
         STS(3)(1) /= ' '  and then
         STS(3)(1..3) /= ZZZ_STEM ) or
        (STS(4)(1) /= STS(1)(1) and then
         STS(4)(1) /= ' '  and then
         STS(4)(1..3) /= ZZZ_STEM ) then
         DICT(FC1) :=
               new DICTIONARY_ITEM'(( (STS(1), ZZZ_STEM, ZZZ_STEM, ZZZ_STEM),
                           --PT, KIND, TRAN, MEAN), DICT(FC1) );
                           PT, TRAN, MEAN), DICT(FC1) );
        DICT(FC2) :=
               new DICTIONARY_ITEM'(( (ZZZ_STEM, STS(2), ZZZ_STEM, ZZZ_STEM),
                           --PT, KIND, TRAN, MEAN), DICT(FC2));
                           PT, TRAN, MEAN), DICT(FC2));
       DICT(FC3) :=
               new DICTIONARY_ITEM'(( (ZZZ_STEM, ZZZ_STEM, STS(3), ZZZ_STEM),
                          --PT, KIND, TRAN, MEAN), DICT(FC3));
                          PT, TRAN, MEAN), DICT(FC3));
       DICT(FC4) :=
               new DICTIONARY_ITEM'(( (ZZZ_STEM, ZZZ_STEM, ZZZ_STEM, STS(4)),
                          --PT, KIND, TRAN, MEAN), DICT(FC4));
                          PT, TRAN, MEAN), DICT(FC4));
       else
          --DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
          DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, TRAN, MEAN),
                                                  DICT(FC1));
      end if;

    elsif PT.POFS = NUM  then
      if PT.NUM.SORT = X  then   --  X for all KINDs
        if (STS(1)(1) /= ' '  and then
            STS(1)(1..3) /= ZZZ_STEM ) then
          DICT(FC1) :=
        new DICTIONARY_ITEM'(( (STS(1), BLK_STEM, BLK_STEM, BLK_STEM),
                             --(NUM, (PT.NUM.DECL, CARD)), KIND, TRAN, MEAN),
                             (NUM, (PT.NUM.DECL, CARD, VALUE)), TRAN, MEAN),
                              DICT(FC1));
        end if;
        if (STS(2)(1) /= ' '  and then
            STS(2)(1..3) /= ZZZ_STEM ) then
           DICT(FC2) :=
           new DICTIONARY_ITEM'(( (ZZZ_STEM, STS(2), BLK_STEM, BLK_STEM),
                       --(NUM, ((0, 0), ORD)), KIND, TRAN, MEAN),
                       (NUM, ((0, 0), ORD, VALUE)), TRAN, MEAN),
                        DICT(FC2));
        end if;
        if (STS(3)(1) /= ' '  and then
            STS(3)(1..3) /= ZZZ_STEM ) then
          DICT(FC3) :=
            new DICTIONARY_ITEM'(( (ZZZ_STEM, ZZZ_STEM, STS(3), BLK_STEM),
                              --(NUM, (PT.NUM.DECL, DIST)), KIND, TRAN, MEAN),
                              (NUM, (PT.NUM.DECL, DIST, VALUE)), TRAN, MEAN),
                                DICT(FC3));
        end if;
        if (STS(4)(1) /= ' '  and then
            STS(4)(1..3) /= ZZZ_STEM ) then
          DICT(FC4) :=
              new DICTIONARY_ITEM'(( (ZZZ_STEM, ZZZ_STEM, ZZZ_STEM, STS(4)),
                          --(NUM, (PT.NUM.DECL, ADVERB)), KIND, TRAN, MEAN),
                          (NUM, (PT.NUM.DECL, ADVERB, VALUE)), TRAN, MEAN),
                            DICT(FC4));
        end if;
      elsif PT.NUM.SORT = CARD  then
          DICT(FC1) :=
        new DICTIONARY_ITEM'(( (STS(1), BLK_STEM, BLK_STEM, BLK_STEM),
                             --(NUM, (PT.NUM.DECL, CARD)), KIND, TRAN, MEAN),
                             (NUM, (PT.NUM.DECL, CARD, VALUE)), TRAN, MEAN),
                              DICT(FC1));
      elsif PT.NUM.SORT = ORD   then
         DICT(FC1) :=
         new DICTIONARY_ITEM'(( (BLK_STEM, STS(1), BLK_STEM, BLK_STEM),
                     --(NUM, (PT.NUM.DECL, ORD)), KIND, TRAN, MEAN),
                     (NUM, (PT.NUM.DECL, ORD, VALUE)), TRAN, MEAN),
                      DICT(FC1));
      elsif PT.NUM.SORT = DIST  then
        DICT(FC1) :=
          new DICTIONARY_ITEM'(( (BLK_STEM, BLK_STEM, STS(1), BLK_STEM),
                            --(NUM, (PT.NUM.DECL, DIST)), KIND, TRAN, MEAN),
                            (NUM, (PT.NUM.DECL, DIST, VALUE)), TRAN, MEAN),
                              DICT(FC1));
      elsif PT.NUM.SORT = ADVERB  then
        DICT(FC1) :=
            new DICTIONARY_ITEM'(( (BLK_STEM, BLK_STEM, BLK_STEM, STS(1)),
                        --(NUM, (PT.NUM.DECL, ADVERB)), KIND, TRAN, MEAN),
                        (NUM, (PT.NUM.DECL, ADVERB, VALUE)), TRAN, MEAN),
                          DICT(FC1));
      end if;

    else
        --DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
        DICT(FC1) := new DICTIONARY_ITEM'((STS, PT, TRAN, MEAN),
                                                DICT(FC1));

    end if;
      NUMBER_OF_DICTIONARY_ENTRIES := NUMBER_OF_DICTIONARY_ENTRIES + 1;
    end loop;
    CLOSE(DICTIONARY_FILE);
    PREFACE.SET_COL(33); PREFACE.PUT("--  ");
    PREFACE.PUT(NUMBER_OF_DICTIONARY_ENTRIES, 6);
    PREFACE.PUT(" entries"); PREFACE.SET_COL(55);
    PREFACE.PUT_LINE("--  Loaded correctly");
  exception
      when others   =>
        PREFACE.PUT_LINE("    LOAD_DICTIONARY exception        !!!!!!!!!!");
        PREFACE.PUT_LINE(ST_LINE(1..LAST));
        PREFACE.PUT_LINE(LINE(1..L));
        CLOSE(DICTIONARY_FILE);
        PREFACE.SET_COL(33); PREFACE.PUT("--  ");
        PREFACE.PUT(NUMBER_OF_DICTIONARY_ENTRIES, 6);
        PREFACE.PUT(" entries"); PREFACE.SET_COL(55);
        PREFACE.PUT_LINE("--  Loaded anyway   ");
  end LOAD_DICTIONARY;

  procedure LOAD_STEM_FILE(D_K : DICTIONARY_KIND)  is
  --  This is used to load a dictionary access file, like DIC.LOC
  --  It uses the single first letter index rather than the two letter
  --  This dictionary must be searched with a somewhat different procedure
  --  Not used when one loads from a regular STEMFILE (which uses two letters)
    --use LATIN_DEBUG;
    use STEM_IO;
    use DICT_IO;
    I : STEM_IO.COUNT := 1;
    --M_P_R : MEANING_TYPE;
    M : DICT_IO.POSITIVE_COUNT := 1;
    DLC : DICTIONARY := DICT_LOC;
    --DS : DICTIONARY_STEM;
    --ZZZ_STEM : constant STEM_TYPE := "zzz" & (4..MAX_STEM_SIZE => ' '); --####
  begin
--PUT_LINE("LOAD_STEM_FILE for LOC");
    if IS_OPEN(STEM_FILE(D_K))  then
      DELETE(STEM_FILE(D_K));
    end if;
    CREATE(STEM_FILE(D_K), INOUT_FILE, ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                             DICTIONARY_KIND'IMAGE(D_K)));
--PUT_LINE("LOAD_STEM_FILE for LOC - Created STEM_FILE");
    if IS_OPEN(DICT_FILE(D_K))  then
      DELETE(DICT_FILE(D_K));
    end if;
    CREATE(DICT_FILE(D_K), INOUT_FILE, ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                             DICTIONARY_KIND'IMAGE(D_K)));
--PUT_LINE("LOAD_STEM_FILE for LOC - Created DICT_FILE");

--PUT_LINE("L_D_F  Start  M = " & INTEGER'IMAGE(INTEGER(M)));

    for FC in CHARACTER range 'a'..'z'  loop
    --  LOAD_DICTIONARY should have assured that all v were in u
--LATIN_DEBUG.PUT_LINE("L_D_F  Entering FC loop");
      DDLF(FC, 'a', D_K) := I;
      DDLL(FC, 'a', D_K) := 0;
      while DLC(FC) /= null  loop
--PUT_LINE("L_D_F  Setting Dictfile index M = " & INTEGER'IMAGE(INTEGER(M)));
        DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
-- %%%%%%%%%%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!%%%%%%%%%%%%%%%%%%%%%%%
--PUT_LINE(DLC(FC).DE.TRAN.MEAN); 
       -- M_P_R := DLC(FC).DE.TRAN.MEAN; 
        --DICT_IO.WRITE(DICT_FILE(D_K), M_P_R);   --@@@@@@@@@@@@@@@@@@@@@
        DICT_IO.WRITE(DICT_FILE(D_K), DLC(FC).DE);
        for K in STEM_KEY_TYPE range 1..4  loop
          if DLC(FC).DE.STEMS(K) /= NULL_STEM_TYPE  and
             DLC(FC).DE.STEMS(K) /= ZZZ_STEM  then
--LATIN_DEBUG.PUT(DLC(FC).DE.STEMS(K)); LATIN_DEBUG.PUT("  ..  ");
--LATIN_DEBUG.PUT(DLC(FC).DE.PART); LATIN_DEBUG.PUT("  ..  "); LATIN_DEBUG.PUT(K); 
--LATIN_DEBUG.PUT("  ..  "); LATIN_DEBUG.PUT(INTEGER(M)); LATIN_DEBUG.NEW_LINE;
             WRITE(STEM_FILE(D_K),
                   (DLC(FC).DE.STEMS(K), DLC(FC).DE.PART, K, M));
            DDLL(FC, 'a', D_K) := I;
--LATIN_DEBUG.PUT_LINE("L_D_F DDLL(FC, 'a', D_K) := I  = " & INTEGER'IMAGE(I));
            I := I + 1;
          end if;
        end loop;
        DLC(FC) := DLC(FC).SUCC;
        M := M + 1;
--PUT_LINE("L_D_F  22222  M = " & INTEGER'IMAGE(INTEGER(M)));
      end loop;
--PUT_LINE("L_D_F  33333  M = " & INTEGER'IMAGE(INTEGER(M)));
    end loop;
--PUT_LINE("L_D_F  44444  M = " & INTEGER'IMAGE(INTEGER(M)));
  end LOAD_STEM_FILE;



  package body TACKON_LINE_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use TACKON_ENTRY_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';


    procedure GET(F : in FILE_TYPE; P : out TACKON_LINE) is
    begin
      GET(F, P.POFS);
      GET(F, SPACER);
      GET(F, P.TACK);
      GET(F, SPACER);
      GET(F, P.ENTR);
      GET(F, SPACER);
      GET(F, P.MEAN);
   end GET;


    procedure GET(P : out TACKON_LINE) is
    begin
      GET(P.POFS);
      GET(SPACER);
      GET(P.TACK);
      GET(SPACER);
      GET(P.ENTR);
      GET(SPACER);
      GET(P.MEAN);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in TACKON_LINE) is
    begin
      PUT(F, P.POFS);
      PUT(F, ' ');
      PUT(F, P.TACK);
      PUT(F, ' ');
      PUT(F, P.ENTR);
      PUT(F, ' ');
      PUT(F, P.MEAN);
     end PUT;

    procedure PUT(P : in TACKON_LINE) is
    begin
      PUT(P.POFS);
      PUT(' ');
      PUT(P.TACK);
      PUT(' ');
      PUT(P.ENTR);
      PUT(' ');
      PUT(P.MEAN);
     end PUT;

    procedure GET(S : in STRING; P : out TACKON_LINE; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      GET(S(L+1..M), P.POFS, L);
      L := M + 1;
      M := L + MAX_STEM_SIZE;
      P.TACK := S(L+1..M);
      L := M + 1;
      M := L + TACKON_ENTRY_IO.DEFAULT_WIDTH;
      GET(S(L+1..M), P.ENTR, L);
      L := M + 1;
      M := L + MAX_MEANING_SIZE;
      P.MEAN := S(L+1..M);
      LAST := M;
    end GET;


    procedure PUT(S : out STRING; P : in TACKON_LINE) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.POFS);
      L := M + 1;
      S(L) := ' ';
      M := L + MAX_STEM_SIZE;
      S(L+1..M) := P.TACK;
      L := M + 1;
      S(L) := ' ';
      M := L + TACKON_ENTRY_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ENTR);
      L := M + 1;
      S(L) := ' ';
      M := L + MAX_MEANING_SIZE;
      S(L+1..M) := P.MEAN;
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end TACKON_LINE_IO;

  package body PREFIX_LINE_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use PREFIX_ENTRY_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';


    procedure GET(F : in FILE_TYPE; P : out PREFIX_LINE) is
    begin
      GET(F, P.POFS);
      GET(F, SPACER);
      GET(F, P.FIX);
      GET(F, SPACER);
      GET(F, P.CONNECT);
      GET(F, SPACER);
      GET(F, P.ENTR);
      GET(F, SPACER);
      GET(F, P.MEAN);
     end GET;


    procedure GET(P : out PREFIX_LINE) is
    begin
      GET(P.POFS);
      GET(SPACER);
      GET(P.FIX);
      GET(SPACER);
      GET(P.CONNECT);
      GET(SPACER);
      GET(P.ENTR);
      GET(SPACER);
      GET(P.MEAN);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in PREFIX_LINE) is
    begin
      PUT(F, P.POFS);
      PUT(F, ' ');
      PUT(F, P.FIX);
      PUT(F, ' ');
      PUT(F, P.CONNECT);
      PUT(F, ' ');
      PUT(F, P.ENTR);
      PUT(F, ' ');
      PUT(F, P.MEAN);
    end PUT;

    procedure PUT(P : in PREFIX_LINE) is
    begin
      PUT(P.POFS);
      PUT(' ');
      PUT(P.FIX);
      PUT(' ');
      PUT(P.CONNECT);
      PUT(' ');
      PUT(P.ENTR);
      PUT(' ');
      PUT(P.MEAN);
    end PUT;

    procedure GET(S : in STRING; P : out PREFIX_LINE; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.POFS, L);
      L := M;
      L := L + 1;
      M := L + MAX_STEM_SIZE;
      P.FIX := S(L+1..M);
      L := M;
      L := L + 1;
      M := L + 1;
      P.CONNECT := S(L+1);
      L := L + 1;
      M := L + PREFIX_ENTRY_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.ENTR, L);
      L := M + 1;
      M := L + MAX_MEANING_SIZE;
      P.MEAN := S(L+1..M);
      LAST := M;
    end GET;


    procedure PUT(S : out STRING; P : in PREFIX_LINE) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.POFS);
      L := M + 1;
      S(L) :=  ' ';
      M := L + MAX_STEM_SIZE;
      S(L+1..M) := P.FIX;
      L := M + 1;
      S(L) :=  ' ';
      M := L + 1;
      S(L+1) := P.CONNECT;
      M := L + PREFIX_ENTRY_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ENTR);
      L := M + 1;
      S(L) :=  ' ';
      M := L + MAX_MEANING_SIZE;
      S(L+1..M) := P.MEAN;
      M := L + 1;
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end PREFIX_LINE_IO;


  package body SUFFIX_LINE_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use SUFFIX_ENTRY_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';

    procedure GET(F : in FILE_TYPE; P : out SUFFIX_LINE) is
    begin
      GET(F, P.POFS);
      GET(F, SPACER);
      GET(F, P.FIX);
      GET(F, SPACER);
      GET(F, P.CONNECT);
      GET(F, SPACER);
      GET(F, P.ENTR);
      GET(F, SPACER);
      GET(F, P.MEAN);
     end GET;


    procedure GET(P : out SUFFIX_LINE) is
    begin
      GET(P.POFS);
      GET(SPACER);
      GET(P.FIX);
      GET(SPACER);
      GET(P.CONNECT);
      GET(SPACER);
      GET(P.ENTR);
      GET(SPACER);
      GET(P.MEAN);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_LINE) is
    begin
      PUT(F, P.POFS);
      PUT(F, ' ');
      PUT(F, P.FIX);
      PUT(F, ' ');
      PUT(F, P.CONNECT);
      PUT(F, ' ');
      PUT(F, P.ENTR);
      PUT(F, ' ');
      PUT(F, P.MEAN);
     end PUT;

    procedure PUT(P : in SUFFIX_LINE) is
    begin
      PUT(P.POFS);
      PUT(' ');
      PUT(P.FIX);
      PUT(' ');
      PUT(P.CONNECT);
      PUT(' ');
      PUT(P.ENTR);
      PUT(' ');
      PUT(P.MEAN);
     end PUT;

    procedure GET(S : in STRING; P : out SUFFIX_LINE; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.POFS, L);
      L := M;
      L := L + 1;
      M := L + MAX_STEM_SIZE;
      P.FIX := S(L+1..M);
      L := M;
      L := L + 1;
      M := L + 1;
      P.CONNECT := S(L+1);
      L := L + 1;
      M := L + SUFFIX_ENTRY_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.ENTR, L);
      L := M + 1;
      M := L + MAX_MEANING_SIZE;
      P.MEAN := S(L+1..M);
      LAST := M;
    end GET;


    procedure PUT(S : out STRING; P : in SUFFIX_LINE) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.POFS);
      L := M + 1;
      S(L) :=  ' ';
      M := L + MAX_STEM_SIZE;
      S(L+1..M) := P.FIX;
      L := M + 1;
      S(L) :=  ' ';
      M := L + 1;
      S(L+1) := P.CONNECT;
      L := M + 1;
      S(L) :=  ' ';
      M := L + SUFFIX_ENTRY_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ENTR);
      L := M + 1;
      S(L) :=  ' ';
      M := L + MAX_MEANING_SIZE;
      S(L+1..M) := P.MEAN;
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end SUFFIX_LINE_IO;

  package body UNIQUE_ENTRY_IO is
    use QUALITY_RECORD_IO;
    use KIND_ENTRY_IO;
    use TRANSLATION_RECORD_IO;
    SPACER : CHARACTER;

    PE : UNIQUE_ENTRY;

    procedure GET(F : in FILE_TYPE; P : out UNIQUE_ENTRY) is
      UE : UNIQUE_ENTRY;
    begin
      GET(F, UE.STEM);
      GET(F, SPACER);
      GET(F, UE.QUAL);
      GET(F, SPACER);
      GET(F, UE.QUAL.POFS, UE.KIND);
      GET(F, SPACER);
      GET(F, UE.TRAN);
      P := UE;
    end GET;


    procedure GET(P : out UNIQUE_ENTRY) is
      UE : UNIQUE_ENTRY;
    begin
      GET(P.STEM);
      GET(SPACER);
      GET(UE.QUAL);
      GET(SPACER);
      GET(UE.QUAL.POFS, UE.KIND);
      GET(SPACER);
      GET(P.TRAN);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in UNIQUE_ENTRY) is
    begin
      PUT(F, P.STEM);
      PUT(F, ' ');
      PUT(F, P.QUAL);
      PUT(F, ' ');
      PUT(F, P.QUAL.POFS, P.KIND);
      PUT(F, ' ');
      PUT(F, P.TRAN);
     end PUT;

    procedure PUT(P : in UNIQUE_ENTRY) is
    begin
      PUT(P.STEM);
      PUT(' ');
      PUT(P.QUAL);
      PUT(' ');
      PUT(P.QUAL.POFS, P.KIND);
      PUT(' ');
      PUT(P.TRAN);
     end PUT;

    procedure GET(S : in STRING; P : out UNIQUE_ENTRY; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + MAX_STEM_SIZE;
      P.STEM := S(L+1..M);
      L := L + 1;
      M := L + QUALITY_RECORD_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.QUAL, L);
      L := L + 1;
      M := L + KIND_ENTRY_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.QUAL.POFS, P.KIND, L);
      L := L + 1;
      M := L + MAX_MEANING_SIZE;
      GET(S(L+1..S'LAST), P.TRAN, LAST);
    end GET;


    procedure PUT(S : out STRING; P : in UNIQUE_ENTRY) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + MAX_STEM_SIZE;
      S(L+1..M) := P.STEM;
      L := M + 1;
      S(L) :=  ' ';
      M := L + QUALITY_RECORD_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.QUAL);
      L := M + 1;
      S(L) :=  ' ';
      M := L + KIND_ENTRY_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.QUAL.POFS, P.KIND);
      L := M + 1;
      S(L) :=  ' ';
      M := M + MAX_MEANING_SIZE;
      PUT(S(L+1..M), P.TRAN);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end UNIQUE_ENTRY_IO;



  procedure LOAD_UNIQUES(UNQ : in out LATIN_UNIQUES; FILE_NAME : in STRING) is
    use INFLECTIONS_PACKAGE.INTEGER_IO;
    use QUALITY_RECORD_IO;
    use PART_ENTRY_IO;
    use KIND_ENTRY_IO;
    use TRANSLATION_RECORD_IO;
    use DICT_IO;
    
    UNIQUES_FILE : TEXT_IO.FILE_TYPE;
    LINE, STEM_LINE, BLANKS : STRING(1..100) := (others => ' ');
    LAST, L : INTEGER := 0;
    STEM : STEM_TYPE := NULL_STEM_TYPE;
    QUAL : QUALITY_RECORD;
    KIND : KIND_ENTRY;
    --PART : PART_ENTRY := NULL_PART_ENTRY;
    TRAN : TRANSLATION_RECORD := NULL_TRANSLATION_RECORD;
    MNPC : MNPC_TYPE := NULL_MNPC;
    MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    M : DICT_IO.POSITIVE_COUNT := 1;
    D_K : constant DICTIONARY_KIND := UNIQUE;

    NUMBER_OF_UNIQUES_ENTRIES : INTEGER := 0;

  begin
--TEXT_IO.PUT_LINE("UNIQUES started");
    TEXT_IO.OPEN(UNIQUES_FILE, TEXT_IO.IN_FILE, FILE_NAME);
    PREFACE.SET_COL(1);
    PREFACE.PUT("UNIQUES file loading");

--    if DICT_IO.IS_OPEN(DICT_FILE(D_K))  then
--      DICT_IO.DELETE(DICT_FILE(D_K));
--    end if;
--    DICT_IO.CREATE(DICT_FILE(D_K), DICT_IO.INOUT_FILE,  "");
--         -- ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, DICTIONARY_KIND'IMAGE(D_K)));

    while not END_OF_FILE(UNIQUES_FILE)  loop
      STEM_LINE := BLANKS;
      GET_LINE(UNIQUES_FILE, STEM_LINE, LAST);      --  STEM 
      STEM := HEAD(TRIM(STEM_LINE(1..LAST)), MAX_STEM_SIZE);

      LINE := BLANKS;
      GET_LINE(UNIQUES_FILE, LINE, LAST);    --  QUAL, KIND, TRAN       
      GET(LINE(1..LAST), QUAL, L);
      GET(LINE(L+1..LAST), QUAL.POFS, KIND, L);
      AGE_TYPE_IO.GET(LINE(L+1..LAST), TRAN.AGE, L);
      AREA_TYPE_IO.GET(LINE(L+1..LAST), TRAN.AREA, L);
      GEO_TYPE_IO.GET(LINE(L+1..LAST), TRAN.GEO, L);
      FREQUENCY_TYPE_IO.GET(LINE(L+1..LAST), TRAN.FREQ, L);
      SOURCE_TYPE_IO.GET(LINE(L+1..LAST), TRAN.SOURCE, L);


      LINE := BLANKS;
      GET_LINE(UNIQUES_FILE, LINE, L);         --  MEAN
      MEAN := HEAD(TRIM(LINE(1..L)), MAX_MEANING_SIZE);
--@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      declare
        UNIQUE_DE : DICTIONARY_ENTRY;
        PART      : PART_ENTRY         := NULL_PART_ENTRY;
      begin
        case PART.POFS is
          when N  =>
            PART := (N, (QUAL.N.DECL, QUAL.N.GENDER, KIND.N_KIND));
          when PRON =>
            PART := (PRON, (QUAL.PRON.DECL, KIND.PRON_KIND));
          when PACK =>
            PART := (PACK, (QUAL.PACK.DECL, KIND.PACK_KIND));
          when ADJ =>
            PART := (ADJ, (QUAL.ADJ.DECL, QUAL.ADJ.CO));
          when NUM =>
            PART := (NUM, (QUAL.NUM.DECL, QUAL.NUM.SORT, KIND.NUM_VALUE));
          when ADV =>
            PART := (ADV, (CO => QUAL.ADV.CO));  
          when V =>
            PART := (V, (QUAL.V.CON, KIND.V_KIND));
          when others  =>
            PART := NULL_PART_ENTRY;
        end case;



        UNIQUE_DE.STEMS := (STEM, 
                            NULL_STEM_TYPE, NULL_STEM_TYPE, NULL_STEM_TYPE);
        UNIQUE_DE.PART  :=  PART;
        --UNIQUE_DE.KIND  :=  KIND;
        UNIQUE_DE.TRAN  :=  TRAN;
        UNIQUE_DE.MEAN  :=  MEAN;
                                          
--        DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
--        DICT_IO.WRITE(DICT_FILE(D_K), UNIQUE_DE);
        
        UNIQUES_DE(M) := UNIQUE_DE;
      end;
--@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@      

       MNPC := M;

       if (LOWER_CASE(STEM(1)) = 'v') then
         UNQ('u') :=
             new UNIQUE_ITEM'(STEM, QUAL, KIND, MNPC, UNQ(LOWER_CASE('u')));
       elsif (LOWER_CASE(STEM(1)) = 'j') then
         UNQ('i') :=
             new UNIQUE_ITEM'(STEM, QUAL, KIND, MNPC, UNQ(LOWER_CASE('i')));
       else
         UNQ(LOWER_CASE(STEM(1))) :=
             new UNIQUE_ITEM'(STEM, QUAL, KIND, MNPC, UNQ(LOWER_CASE(STEM(1))));
       end if;

      M := M + 1;
      NUMBER_OF_UNIQUES_ENTRIES := INTEGER(M) - 1;
    
    end loop;
    CLOSE(UNIQUES_FILE);
    PREFACE.SET_COL(33);
    PREFACE.PUT("--  "); PREFACE.PUT(NUMBER_OF_UNIQUES_ENTRIES, 6);
    PREFACE.PUT(" entries");
    PREFACE.SET_COL(55); PREFACE.PUT_LINE("--  Loaded correctly");
  exception
    when TEXT_IO.NAME_ERROR  =>
      PREFACE.PUT_LINE("There is no UNIQUES file");
    when others   =>
    PREFACE.NEW_LINE;
    PREFACE.PUT_LINE("LOAD_UNIQUES exception        !!!!!!!!!!!!!!!!!!!!!");
    PREFACE.PUT_LINE(STEM_LINE(1..LAST));
    PREFACE.PUT_LINE(LINE(1..L));
      CLOSE(UNIQUES_FILE);
    PREFACE.SET_COL(33);
    PREFACE.PUT("--  "); PREFACE.PUT(NUMBER_OF_UNIQUES_ENTRIES, 6);
    PREFACE.PUT(" entries");
    PREFACE.SET_COL(55); PREFACE.PUT_LINE("--  Loaded before error");
      --raise;
    end LOAD_UNIQUES;
    
    
    
    

begin

--  PARSE_LINE_IO.DEFAULT_WIDTH :=
--                                   MAX_STEM_SIZE + 1 +
--                                   INFLECTION_RECORD_IO.DEFAULT_WIDTH + 1 +
--                                   DICTIONARY_KIND_IO.DEFAULT_WIDTH + 1 +
--                                   MAX_MEANING_SIZE;


  PREFIX_LINE_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                  MAX_STEM_SIZE + 1 +
                                  1 + 1 +
                                  PREFIX_ENTRY_IO.DEFAULT_WIDTH + 1 +
                                  MAX_MEANING_SIZE;
  SUFFIX_LINE_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                  MAX_STEM_SIZE + 1 +
                                  1 + 1 +
                                  SUFFIX_ENTRY_IO.DEFAULT_WIDTH + 1 +
                                  MAX_MEANING_SIZE;
  TACKON_LINE_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                  MAX_STEM_SIZE + 1 +
                                  TACKON_ENTRY_IO.DEFAULT_WIDTH + 1 +
                                  MAX_MEANING_SIZE;

  UNIQUE_ENTRY_IO.DEFAULT_WIDTH := MAX_STEM_SIZE + 1 +
                                   INFLECTION_RECORD_IO.DEFAULT_WIDTH + 1 +
                                   TRANSLATION_RECORD_IO.DEFAULT_WIDTH;



end LINE_STUFF;
