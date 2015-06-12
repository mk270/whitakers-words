with TEXT_IO;
package WORD_PARAMETERS is
--  This package defines a number of parameters that areused in the program
--  The default values are set in the body, so that they may be changed easily

  CHANGE_PARAMETERS_CHARACTER        : CHARACTER := '#';
  CHANGE_LANGUAGE_CHARACTER          : CHARACTER := '~';
  HELP_CHARACTER                     : CHARACTER := '?';

  --  These files are used by the program if requested, but not necessary
  --  They are all text files and human readable

  --  MODE_FILE is used by the program to remember MODE values between runs
  MODE_FILE : TEXT_IO.FILE_TYPE;

  --  OUTPUT is used to write out and save the results of a run
  OUTPUT : TEXT_IO.FILE_TYPE;
  INPUT  : TEXT_IO.FILE_TYPE;
  --  UNKNOWNS is used to record the words that the program fails to find
  UNKNOWNS : TEXT_IO.FILE_TYPE;

  --  This is a flag to tell if there has been trimming for this word
  TRIMMED : BOOLEAN := FALSE;
  
  
  type MODE_TYPE is (      
                      TRIM_OUTPUT, 
                      
                      HAVE_OUTPUT_FILE,        
                      WRITE_OUTPUT_TO_FILE,    

                      DO_UNKNOWNS_ONLY,        
                      WRITE_UNKNOWNS_TO_FILE,  
                    
                      IGNORE_UNKNOWN_NAMES,    
                      IGNORE_UNKNOWN_CAPS,    
                      DO_COMPOUNDS,            
                      DO_FIXES,                
                      DO_TRICKS,               
                    
                      DO_DICTIONARY_FORMS,     
                      SHOW_AGE,                
                      SHOW_FREQUENCY,          

                      DO_EXAMPLES,             
                      DO_ONLY_MEANINGS,        
                      DO_STEMS_FOR_UNKNOWN       );         

  package MODE_TYPE_IO is new TEXT_IO.ENUMERATION_IO(MODE_TYPE); 

  type MODE_ARRAY is array (MODE_TYPE) of BOOLEAN;


  WORDS_MODE : MODE_ARRAY;        --  Initialized in body

  
  
  procedure CHANGE_PARAMETERS;  

 
  procedure INITIALIZE_WORD_PARAMETERS;

end WORD_PARAMETERS;
