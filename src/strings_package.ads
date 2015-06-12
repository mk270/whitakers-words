   with TEXT_IO;
   package STRINGS_PACKAGE is
      type TRIM_END is (LEFT, RIGHT, BOTH);
   
      NULL_STRING : constant STRING(2..1) := (others => ' ');
   
   
      function MAX(A, B : INTEGER) return INTEGER;
      function MIN(A, B : INTEGER) return INTEGER;
   
   
      function LOWER_CASE(C : CHARACTER) return CHARACTER;
      function LOWER_CASE(S : STRING) return STRING;
   
      function UPPER_CASE(C : CHARACTER) return CHARACTER;
      function UPPER_CASE(S : STRING) return STRING;
   
      function TRIM(SOURCE : in STRING;
                    SIDE   : in TRIM_END := BOTH) return STRING;
   --  Equivalent to Ada.Strings.Fixed.Trim(Source, Both);
   
      function HEAD(SOURCE : in STRING; 
                    COUNT  : in NATURAL; 
                    PAD    : in CHARACTER := ' ') return STRING;  
   
   
      procedure GET_NON_COMMENT_LINE(F : in TEXT_IO.FILE_TYPE; 
                                     S : out STRING; LAST : out INTEGER);
   
   
   end STRINGS_PACKAGE;  
