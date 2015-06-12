with TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package LIST_PACKAGE is

         
--  SCROLL_LINE_NUMBER : INTEGER := 0;
--  OUTPUT_SCROLL_COUNT : INTEGER := 0;
--
  
  procedure LIST_STEMS(OUTPUT   : TEXT_IO.FILE_TYPE;
                       RAW_WORD : STRING;
                       INPUT_LINE : STRING;
                       PA       : in out PARSE_ARRAY; 
                       PA_LAST  : in out INTEGER);
                       
                       
  procedure LIST_ENTRY(OUTPUT   : TEXT_IO.FILE_TYPE;
                       D_K      : DICTIONARY_KIND;
                       MN       : DICT_IO.COUNT);

                         
  procedure UNKNOWN_SEARCH(UNKNOWN       :  in STRING;
                           UNKNOWN_COUNT : out DICT_IO.COUNT);
                                  
  procedure LIST_NEIGHBORHOOD(OUTPUT : TEXT_IO.FILE_TYPE; INPUT_WORD : STRING);

end LIST_PACKAGE;
