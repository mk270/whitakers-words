with Ada.TEXT_IO;  
procedure PATCH is 
  package INTEGER_IO is new Ada.TEXT_IO.INTEGER_IO (INTEGER); 
  use Ada.TEXT_IO; 
  use INTEGER_IO; 
  
  F1, F2, F3  : FILE_TYPE; 
  F           : STRING (1..100); 
  BLANKS      : STRING (1..250) := (others => ' '); 
  S, T        : STRING (1..250) := BLANKS; 
  L           : INTEGER := 0; 
  N           : INTEGER := 0; 
  LS, LT      : INTEGER := 0; 
begin 
  PUT_LINE ("Takes in two files and produces a third which is the pair"); 
  PUT_LINE ("as columns with N blanks between"); 
  PUT_LINE ("Does this while there are corresponding lines in both files"); 
  
  PUT ("What is first file to PATCH from =>"); 
  GET_LINE (F, L); 
  PUT ("=> "); 
  OPEN (F1, IN_FILE, F (1..L)); 
  PUT_LINE ("Opened first input file"); 
  
  PUT ("What is second file to PATCH from =>"); 
  GET_LINE (F, L); 
  PUT ("=> "); 
  OPEN (F2, IN_FILE, F (1..L)); 
  PUT_LINE ("Opened second input file"); 
  
  PUT ("How many blank columns to leave between =>"); 
  GET (N); 
  SKIP_LINE; 
  NEW_LINE; 
  
  PUT ("Where to put the resulting PATCHed file =>"); 
  GET_LINE (F, L); 
  PUT ("=> "); 
  CREATE (F3, OUT_FILE, F (1..L)); 
  PUT_LINE ("Created PATCHed output file"); 
  
  while (not END_OF_FILE (F1) and not END_OF_FILE (F2)) loop 
    GET_LINE (F1, S, LS); 
    GET_LINE (F2, T, LT); 
    PUT_LINE (F3, S (1..LS) & BLANKS (1..N) & T (1..LT)); 
  end loop; 
  CLOSE (F1); 
  CLOSE (F2); 
  CLOSE (F3); 
  PUT_LINE ("Finshed PATCH"); 
  
exception 
  when others => 
    PUT_LINE ("Unexpected exception in PATCH"); 
    CLOSE (F3); 
end PATCH;
