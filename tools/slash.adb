with Ada.TEXT_IO;  
procedure SLASH is 
  package INTEGER_IO is new Ada.TEXT_IO.INTEGER_IO (INTEGER); 
  use Ada.TEXT_IO; 
  use INTEGER_IO; 
  
  F1, F2, F3  : FILE_TYPE; 
  F           : STRING (1..100); 
  S           : STRING (1..2500); 
  BS          : STRING (1..2500) := (others => ' '); 
  N           : INTEGER := 0; 
  L           : INTEGER := 0; 
  LS          : INTEGER := 0; 

  type REPLY_TYPE is (COLUMNS, LINES);
  REPLY : REPLY_TYPE;          
  REPLY_CHARACTER : CHARACTER;

  function WHICH(R : CHARACTER) return REPLY_TYPE is
  begin
    case R is
      when 'C' | 'c'  =>  return COLUMNS;
      when 'L' | 'l'  =>  return LINES;  
      when others     =>  
        raise DATA_ERROR;
    end case;
  end WHICH;


begin 
  PUT_LINE("Breaks a file into two, by row or column.");
  
  PUT ("What file to SLASH from =>"); 
  GET_LINE (F, L); 
  PUT ("=> "); 
  OPEN (F1, IN_FILE, F (1..L)); 
  PUT_LINE ("Opened input file"); 
  
  PUT ("Do you wish to SLASH C)olumns or L)ines? =>"); 
  GET (REPLY_CHARACTER); 
  SKIP_LINE; 
  REPLY := WHICH(REPLY_CHARACTER);
  NEW_LINE; 
  
  PUT ("How many lines/columns to leave after SLASHing =>"); 
  GET (N); 
  SKIP_LINE; 
  NEW_LINE; 
  
  PUT ("Where to put the first  =>"); 
  GET_LINE (F, L); 
  PUT ("=> "); 
  CREATE (F2, OUT_FILE, F (1..L)); 
  PUT_LINE ("Created SLASH file first"); 
  
  PUT ("Where to put the rest  =>"); 
  GET_LINE (F, L); 
  PUT ("=> "); 
  CREATE (F3, OUT_FILE, F (1..L)); 
  PUT_LINE ("Created SLASH file rest"); 
            

  if REPLY = COLUMNS  then
  
    while not END_OF_FILE (F1) loop 
      S := BS;
      GET_LINE (F1, S, LS); 
      if LS <= N then            --  Line shorter than break 
        PUT_LINE (F2, S (1..LS)); 
        PUT_LINE (F3, "");       --  Put a blank line so there will be a line
      else                       --  Line runs past break 
        PUT_LINE (F2, S (1..N)); 
        PUT_LINE (F3, S (N + 1..LS)); 
      end if; 
    end loop; 
    CLOSE (F2); 
    CLOSE (F3); 
  
  elsif REPLY = LINES  then
    
    FIRST:
    begin
      for I in 1..N loop 
        GET_LINE (F1, S, LS); 
        PUT_LINE (F2, S (1..LS)); 
      end loop; 
    exception
      when END_ERROR  =>
        null;
    end FIRST;
    CLOSE (F2);
 
    SECOND:
    begin
      loop 
        GET_LINE (F1, S, LS); 
        PUT_LINE (F3, S (1..LS)); 
      end loop; 
    exception
      when END_ERROR  =>
        null;
    end SECOND;
    CLOSE (F3); 
  
  end if;                   

  PUT_LINE ("Done SLASHing"); 
  
  
exception
  when DATA_ERROR  => 
    PUT_LINE("***************** WRONG REPLY *****************");
    NEW_LINE(2);            
    PUT_LINE("Try again");
  when others      => 
    NEW_LINE(2);            
    PUT_LINE("Unexpected exception raised in SLASH  *********");
end SLASH; 
