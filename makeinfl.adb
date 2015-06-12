with TEXT_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with IO_EXCEPTIONS;
procedure MAKEINFL is
  package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
  use TEXT_IO;
  use INTEGER_IO;
  use STEM_KEY_TYPE_IO;
  use INFLECTION_RECORD_IO;
  use QUALITY_RECORD_IO;
  use ENDING_RECORD_IO;
  use AGE_TYPE_IO;
  use FREQUENCY_TYPE_IO;
  use LEL_SECTION_IO;
  
  PORTING : constant BOOLEAN := TRUE;    --FALSE for WAKEINFL;

  M, N   : INTEGER := 0;
  N1, N2, N3, N4, N5 : INTEGER := 0;
  
  OUTPUT : TEXT_IO.FILE_TYPE;
  INFLECTIONS_SECTIONS_FILE : LEL_SECTION_IO.FILE_TYPE;

procedure FILE_INFLECTIONS_SECTIONS is
--  Reads the INFLECTS. file and prepares an inflections list
--  Then it writes that list into an array
--  Loads the inflection array into a file for later retrieval
  use TEXT_IO;
  use INFLECTION_RECORD_IO;
  use INTEGER_IO;

  INFLECTIONS_FILE : TEXT_IO.FILE_TYPE;
  INFLECTIONS_SECTIONS_FILE : LEL_SECTION_IO.FILE_TYPE;
  IR : INFLECTION_RECORD;
  LINE, BLANKS : STRING(1..100) := (others => ' ');
  LAST, L : INTEGER := 0;
  SN : ENDING_SIZE_TYPE := ENDING_SIZE_TYPE'FIRST;
  SX : CHARACTER := ' ';

  type INFLECTION_ITEM;
  type INFLECTION_LIST is access INFLECTION_ITEM;

  type INFLECTION_ITEM is
    record
      IR   : INFLECTION_RECORD;
      SUCC : INFLECTION_LIST;
    end record;

  type LATIN_INFLECTIONS is array (INTEGER range 0..MAX_ENDING_SIZE,
                          CHARACTER  range ' '..'z') of INFLECTION_LIST;
  NULL_LATIN_INFLECTIONS : LATIN_INFLECTIONS := (others => (others => null));

  L_I : LATIN_INFLECTIONS := NULL_LATIN_INFLECTIONS;

  LEL : LEL_SECTION := (others => NULL_INFLECTION_RECORD);
  J1, J2, J3, J4, J5 : INTEGER := 0;

procedure NULL_LEL is
begin
  for I in LEL'RANGE  loop
    LEL(I) := NULL_INFLECTION_RECORD;
  end loop;
end NULL_LEL;

procedure LOAD_INFLECTIONS_LIST is
  --  Takes the INFLECT. file and populates the L_I list of inflections
  --  indexed on ending size and last letter of ending
  begin
PUT_LINE("Begin  LOAD_INFLECTIONS_LIST");
    NUMBER_OF_INFLECTIONS := 0;

    L_I := NULL_LATIN_INFLECTIONS;
    OPEN(INFLECTIONS_FILE, IN_FILE, INFLECTIONS_FULL_NAME);
    TEXT_IO.PUT("INFLECTIONS file loading");
    while not END_OF_FILE(INFLECTIONS_FILE)  loop

      READ_A_LINE:
      begin
      GET_NON_COMMENT_LINE(INFLECTIONS_FILE, LINE, LAST);

        if LAST > 0  then
          GET(LINE(1..LAST), IR, L);
          SN := IR.ENDING.SIZE;
          if SN = 0  then
            SX := ' ';
          else
            SX := IR.ENDING.SUF(SN);
          end if;
          L_I(SN, SX) := new INFLECTION_ITEM'(IR, L_I(SN, SX));
          NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + 1;
--TEXT_IO.PUT(INTEGER'IMAGE(NUMBER_OF_INFLECTIONS) & "  "); INFLECTION_RECORD_IO.PUT(IR); NEW_LINE;
        end if;
      exception
        when CONSTRAINT_ERROR | IO_EXCEPTIONS.DATA_ERROR  =>
          PUT_LINE("****" & LINE(1..LAST));
      end READ_A_LINE;

    end loop;
    CLOSE(INFLECTIONS_FILE);
PUT_LINE("INFLECTIONS_LIST LOADED   " & INTEGER'IMAGE(NUMBER_OF_INFLECTIONS));

end LOAD_INFLECTIONS_LIST;


  procedure LIST_TO_LEL_FILE  is
  --  From ILC (=L_I) list of inflections, prepares the LEL inflections array
  use LEL_SECTION_IO;
    I : INTEGER := 0;
    ILC : LATIN_INFLECTIONS := L_I;
  begin

    CREATE(INFLECTIONS_SECTIONS_FILE, OUT_FILE, INFLECTIONS_SECTIONS_NAME);

NULL_LEL;
ILC := L_I;                              --  Resetting the list to start over
    while ILC(0, ' ') /= null  loop
      J5 := J5 + 1;
      LEL(J5) := ILC(0, ' ').IR;
      ILC(0, ' ') := ILC(0, ' ').SUCC;
    end loop;
WRITE(INFLECTIONS_SECTIONS_FILE, LEL, 5);
N5 := J5;

NULL_LEL;
ILC := L_I;                              --  Resetting the list to start over
    for CH in CHARACTER range 'a'..'z'  loop
    for N in reverse 1..MAX_ENDING_SIZE  loop
      while ILC(N, CH) /= null  loop
if   not
   (ILC(N, CH).IR.QUAL.POFS = PRON  and then
   (ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 1  or
    ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 2))  then

if CH in INFLECTIONS_SECTION_1  then
  J1 := J1 + 1;
  LEL(J1) := ILC(N, CH).IR;

end if;
end if;
    ILC(N, CH) := ILC(N, CH).SUCC;
      end loop;
    end loop;
    end loop;
    WRITE(INFLECTIONS_SECTIONS_FILE, LEL, 1);
    N1 := J1;

NULL_LEL;
ILC := L_I;                              --  Resetting the list to start over
    for CH in CHARACTER range 'a'..'z'  loop
    for N in reverse 1..MAX_ENDING_SIZE  loop
      while ILC(N, CH) /= null  loop
if   not
   (ILC(N, CH).IR.QUAL.POFS = PRON  and then
   (ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 1  or
    ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 2))  then

if CH in INFLECTIONS_SECTION_2  then
  J2 := J2 + 1;
  LEL(J2) := ILC(N, CH).IR;

end if;
end if;
    ILC(N, CH) := ILC(N, CH).SUCC;
      end loop;
    end loop;
    end loop;
    WRITE(INFLECTIONS_SECTIONS_FILE, LEL, 2);
    N2 := J2;


NULL_LEL;
ILC := L_I;                              --  Resetting the list to start over
    for CH in CHARACTER range 'a'..'z'  loop
    for N in reverse 1..MAX_ENDING_SIZE  loop
      while ILC(N, CH) /= null  loop
if   not
   (ILC(N, CH).IR.QUAL.POFS = PRON  and then
   (ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 1  or
    ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 2))  then

if CH in INFLECTIONS_SECTION_3  then
  J3 := J3 + 1;
  LEL(J3) := ILC(N, CH).IR;

end if;
end if;
    ILC(N, CH) := ILC(N, CH).SUCC;
      end loop;
    end loop;
    end loop;
    WRITE(INFLECTIONS_SECTIONS_FILE, LEL, 3);
    N3 := J3;


NULL_LEL;
ILC := L_I;                              --  Resetting the list to start over
    for CH in CHARACTER range 'a'..'z'  loop
    for N in reverse 1..MAX_ENDING_SIZE  loop
      while ILC(N, CH) /= null  loop
if   not
   (ILC(N, CH).IR.QUAL.POFS = PRON  and then
   (ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 1  or
    ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 2))  then

if (CH in INFLECTIONS_SECTION_4)  then
  J4 := J4 + 1;
  LEL(J4) := ILC(N, CH).IR;

end if;
end if;
    ILC(N, CH) := ILC(N, CH).SUCC;
      end loop;
    end loop;
    end loop;
    
    --  Now put the PACK in 4            --  Maybe it should be in 5 ????
ILC := L_I;                              --  Resetting the list to start over
for CH in CHARACTER range 'a'..'z'  loop
    for N in reverse 1..MAX_ENDING_SIZE  loop
      while ILC(N, CH) /= null  loop
if (ILC(N, CH).IR.QUAL.POFS = PRON  and then
   (ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 1  or
    ILC(N, CH).IR.QUAL.PRON.DECL.WHICH = 2))  then  --  2 no longer PACK

  J4 := J4 + 1;
  LEL(J4) := ILC(N, CH).IR;

end if;
    ILC(N, CH) := ILC(N, CH).SUCC;
      end loop;
    end loop;
    end loop;
    WRITE(INFLECTIONS_SECTIONS_FILE, LEL, 4);
    N4 := J4;

    CLOSE(INFLECTIONS_SECTIONS_FILE);

  end LIST_TO_LEL_FILE;


begin

  LOAD_INFLECTIONS_LIST;

  TEXT_IO.SET_COL(33);
  TEXT_IO.PUT("--  ");
  INTEGER_IO.PUT(NUMBER_OF_INFLECTIONS);
  TEXT_IO.PUT_LINE(" entries    --  Loaded correctly");

  LIST_TO_LEL_FILE;                     --  Load arrays to file 
  TEXT_IO.PUT_LINE("File INFLECTS.SEC  --  Loaded");

exception
  when others =>
    TEXT_IO.PUT_LINE("Exception in FILE_INFLECTIONS_SECTIONS");
end FILE_INFLECTIONS_SECTIONS;

use INFLECTIONS_PACKAGE;
begin

  PUT_LINE("Produces INFLECTS.SEC file from INFLECTS.");

  FILE_INFLECTIONS_SECTIONS;

if not PORTING  then
  PUT_LINE("using FILE_INFLECTIONS_SECTIONS, also produces INFLECTS.LIN file");

  CREATE(OUTPUT, OUT_FILE, "INFLECTS.LIN");
end if;

  ESTABLISH_INFLECTIONS_SECTION;

  LEL_SECTION_IO.OPEN(INFLECTIONS_SECTIONS_FILE, IN_FILE,
                      INFLECTIONS_SECTIONS_NAME);

if not PORTING  then
    for I in BEL'RANGE    loop                     --  Blank endings
      if  BEL(I) /= NULL_INFLECTION_RECORD  then
        M := M + 1;
        PUT(OUTPUT, BEL(I).QUAL); 
        SET_COL(OUTPUT, 50);
        PUT(OUTPUT, BEL(I).KEY, 1); 
        SET_COL(OUTPUT, 52);
        PUT(OUTPUT, BEL(I).ENDING); 
        SET_COL(OUTPUT, 62);
        PUT(OUTPUT, BEL(I).AGE); 
        SET_COL(OUTPUT, 64);
        PUT(OUTPUT, BEL(I).FREQ); 
        NEW_LINE(OUTPUT);
      end if;
    end loop;
end if;

for N in 1..4  loop
READ(INFLECTIONS_SECTIONS_FILE, LEL, LEL_SECTION_IO.POSITIVE_COUNT(N));

if not PORTING  then
    for I in LEL'RANGE    loop                     --  Non-blank endings
      if  LEL(I) /= NULL_INFLECTION_RECORD  then
        M := M + 1;
        PUT(OUTPUT, LEL(I).QUAL); 
        SET_COL(OUTPUT, 50);
        PUT(OUTPUT, LEL(I).KEY, 1); 
        SET_COL(OUTPUT, 52);
        PUT(OUTPUT, LEL(I).ENDING); 
        SET_COL(OUTPUT, 62);
        PUT(OUTPUT, LEL(I).AGE); 
        SET_COL(OUTPUT, 64);
        PUT(OUTPUT, LEL(I).FREQ); 
        NEW_LINE(OUTPUT);
      end if;
    end loop;
end if;

end loop;

NEW_LINE;
PUT("LINE_INFLECTIONS finds "); PUT(M); PUT_LINE(" inflections"); NEW_LINE;


for I in Character range ' '..' '  loop
INTEGER_IO.PUT(0); PUT("    "); PUT(I); PUT("    "); PUT(BELF(0, I));
        PUT("  ");   PUT(BELL(0, I));
        PUT("    "); PUT(BELL(0, I) - BELF(0, I) + 1); NEW_LINE;
end loop;
NEW_LINE;

for I in Character range 'a'..'z'  loop
for N in reverse 1..MAX_ENDING_SIZE  loop
if (LELL(N, I) > 0)  and then (LELF(N, I) <= LELL(N, I))  then
PUT(N); PUT("    "); PUT(I); PUT("    "); PUT(LELF(N, I));
        PUT("  ");   PUT(LELL(N, I));
        PUT("    "); PUT(LELL(N, I) - LELF(N, I) + 1); NEW_LINE;
end if;
end loop;
end loop;
NEW_LINE;

for I in Character range 'a'..'z'  loop
for N in reverse 1..MAX_ENDING_SIZE  loop
if (PELL(N, I) > 0)  and then (PELF(N, I) <= PELL(N, I))  then
PUT(N); PUT("    "); PUT(I); PUT("    "); PUT(PELF(N, I));
        PUT("  ");   PUT(PELL(N, I));
        PUT("    "); PUT(PELL(N, I) - PELF(N, I) + 1); NEW_LINE;
end if;
end loop;
end loop;
NEW_LINE;


NEW_LINE;
PUT(N5);  PUT("    ");
PUT(N1);  PUT("    ");
PUT(N2);  PUT("    ");
PUT(N3);  PUT("    ");
PUT(N4);  PUT("    ");
NEW_LINE;


end MAKEINFL;
