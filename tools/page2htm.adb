with TEXT_IO; use TEXT_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
procedure PAGE2HTM is
  LINE, LINE_OUT, BLANK_LINE : STRING(1..300) := (others => ' ');
  LAST, COLONS : INTEGER := 0;
  
  INPUT, OUTPUT : FILE_TYPE;
  
begin 
  PUT_LINE("DICTPAGE.RAW (sorted) -> DICTPAGE.HTM");
  PUT_LINE("For use in preparing a DICTPAGE.HTM after running DICTPAGE and sorting.");
  
  OPEN(INPUT, IN_FILE, "DICTPAGE.RAW");
  CREATE(OUTPUT, OUT_FILE, "DICTPAGE.HTM");
  
  while not END_OF_FILE(INPUT)  loop
    GET_LINE(INPUT, LINE, LAST);
    if LINE(1) /= '#'  then
        PUT_LINE("BAD LINE   >" & LINE(1..LAST));
    end if;
    for I in 1..LAST  loop
      if LINE(I) = '['  then
        PUT(OUTPUT, "<B>" & LINE(2..I-1) & "</B>  ");
        PUT_LINE(OUTPUT, TRIM(LINE(I..I+6) & "<BR>"));
      end if;
    if LINE(I..I+1) = "::"  then
        PUT_LINE(OUTPUT, TRIM(LINE(I+2..LAST)) & "<BR>");
        exit;
      end if;
    end loop;  --  On LINE
    
  end loop;  --  On file
  
end PAGE2HTM;

 
