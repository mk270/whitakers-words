with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package TRICKS_PACKAGE is
    
  procedure SYNCOPE(W : STRING;
                    PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER);           

  procedure TRY_TRICKS(W : STRING; 
                       PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER;   
                       LINE_NUMBER : INTEGER; WORD_NUMBER : INTEGER);

  procedure TRY_SLURY(W : STRING;
                      PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER;
                      LINE_NUMBER : INTEGER; WORD_NUMBER : INTEGER);   

  procedure ROMAN_NUMERALS(INPUT_WORD : STRING;
                           PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER);
  

end TRICKS_PACKAGE;
