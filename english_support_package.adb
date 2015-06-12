 package body ENGLISH_SUPPORT_PACKAGE is
   --use EWDS_DIRECT_IO;
   use TEXT_IO;    
   
 package body EWDS_RECORD_IO is
  package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
  use PART_OF_SPEECH_TYPE_IO;
  use FREQUENCY_TYPE_IO;
  use TEXT_IO;
  use INTEGER_IO;
  SPACER : CHARACTER := ' ';
  NWIDTH : constant := 5;
   
  procedure GET(F : in TEXT_IO.FILE_TYPE; P : out EWDS_RECORD) is
  begin
    GET(F, P.W);
    GET(F, SPACER);
    GET(F, P.AUX);
    GET(F, SPACER);
    GET(F, P.N);
    GET(F, SPACER);
    GET(F, P.POFS);
    GET(F, SPACER);
    GET(F, P.FREQ);
    GET(F, SPACER);
    GET(F, P.SEMI);
    GET(F, SPACER);
    GET(F, P.KIND);
    GET(F, SPACER);
    GET(F, P.RANK);
end GET;

  procedure GET(P : out EWDS_RECORD) is
  begin
    GET(P.W);
    GET(SPACER);
    GET(P.AUX);
    GET(SPACER);
    GET(P.N);
    GET(SPACER);
    GET(P.POFS);
    GET(SPACER);
    GET(P.FREQ);
    GET(SPACER);
    GET(P.SEMI);
    GET(SPACER);
    GET(P.KIND);
    GET(SPACER);
    GET(P.RANK);
end GET;

  procedure PUT(F : in TEXT_IO.FILE_TYPE; P : in EWDS_RECORD) is
  begin
    PUT(F, P.W);
    PUT(F, ' ');
    PUT(F, P.AUX);
    PUT(F, ' ');
    PUT(F, P.N);
    PUT(F, ' ');
    PUT(F, P.POFS);
    PUT(F, ' ');
    PUT(F, P.FREQ);
    PUT(F, ' ');
    PUT(F, P.SEMI, NWIDTH);
    PUT(F, ' ');
    PUT(F, P.KIND, NWIDTH);
    PUT(F, ' ');
    PUT(F, P.RANK, NWIDTH);
 end PUT;

  procedure PUT(P : in EWDS_RECORD) is
  begin
    PUT(P.W);
    PUT(' ');
    PUT(P.AUX);
    PUT(' ');
    PUT(P.N);
    PUT(' ');
    PUT(P.POFS);
    PUT(' ');
    PUT(P.FREQ);
    PUT(' ');
    PUT(P.SEMI, NWIDTH);
    PUT(' ');
    PUT(P.KIND, NWIDTH);
    PUT(' ');
    PUT(P.RANK, NWIDTH);
end PUT;

  procedure GET(S : in STRING; P : out EWDS_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    P.W := S(L+1..L+EWORD_SIZE);
    L := L + EWORD_SIZE + 1;
    P.AUX := S(L+1..L+AUX_WORD_SIZE);
    L := L + AUX_WORD_SIZE + 1;
    GET(S(L+1..S'LAST), P.N, L);
    L := L + 1;           
    GET(S(L+1..S'LAST), P.POFS, L);
    L := L + 1;           
    GET(S(L+1..S'LAST), P.FREQ, L);
    L := L + 1;           
    GET(S(L+1..S'LAST), P.SEMI, L);
    L := L + 1;           
    GET(S(L+1..S'LAST), P.KIND, L);
    L := L + 1;           
    GET(S(L+1..S'LAST), P.RANK, LAST);
  end GET;

    
  procedure PUT(S : out STRING; P : in EWDS_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + EWORD_SIZE;
    S(L+1..M) :=  P.W;
    L := M + 1;
    S(L) :=  ' ';
    M := L + AUX_WORD_SIZE;
    S(L+1..M) := P.AUX;
    L := M + 1;
    S(L) :=  ' ';
    M := L + LINE_NUMBER_WIDTH;
    PUT(S(L+1..M), P.N);
    S(L) :=  ' ';
    M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.POFS);
    S(L) :=  ' ';
    M := L + FREQUENCY_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.FREQ);
    S(L) :=  ' ';
    M := L + PRIORITY_WIDTH;
    PUT(S(L+1..M), P.SEMI, NWIDTH);
    S(L) :=  ' ';
    M := L + PRIORITY_WIDTH;
    PUT(S(L+1..M), P.KIND, NWIDTH);
    S(L) :=  ' ';
    M := L + PRIORITY_WIDTH;
    PUT(S(L+1..M), P.RANK, NWIDTH);

    
    
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end EWDS_RECORD_IO;


end ENGLISH_SUPPORT_PACKAGE;

