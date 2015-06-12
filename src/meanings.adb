   with Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with CONFIG; use CONFIG;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with WORD_PACKAGE; use WORD_PACKAGE;
   with PARSE;
   procedure MEANINGS is
      INPUT_LINE  : STRING(1..250) := (others => ' ');
      ARGUMENTS_START : INTEGER := 1;
   begin
      --  The language shift in argumants must take place here
      --  since later parsing of line ignores non-letter characters
      CONFIGURATION := ONLY_MEANINGS;
 

      --The main mode of usage for WORDS is a simple call, followed by screen interaction.
      if Ada.Command_Line.ARGUMENT_COUNT = 0  then      --  Simple WORDS
         METHOD := INTERACTIVE;                          --  Interactive
         SUPPRESS_PREFACE := FALSE;
         SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
         INITIALIZE_WORD_PARAMETERS;
         INITIALIZE_DEVELOPER_PARAMETERS;
         INITIALIZE_WORD_PACKAGE;
         PARSE;
         
      --But there are other, command line options.
      --WORDS may be called with arguments on the same line, 
      --in a number of different modes.
      --
      else
         SUPPRESS_PREFACE := TRUE;
         INITIALIZE_WORD_PARAMETERS;
         INITIALIZE_DEVELOPER_PARAMETERS;
         INITIALIZE_WORD_PACKAGE;
   
      --Single parameter, either a simple Latin word or an input file.
      --WORDS amo
      --WORDS infile
      if Ada.Command_Line.ARGUMENT_COUNT = 1  then      --  Input 1 word in-line
         ONE_ARGUMENT:
         declare
            INPUT_NAME  : constant STRING := TRIM(Ada.Command_Line.Argument(1));
         begin
            OPEN(INPUT, IN_FILE, INPUT_NAME); --  Try file name, not raises NAME_ERROR
            METHOD := COMMAND_LINE_FILES;
            SET_INPUT(INPUT);
            SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
            PARSE;          --  No additional arguments, so just go to PARSE now
            exception                  --  Triggers on INPUT
               when NAME_ERROR  =>                   --  Raised NAME_ERROR therefore
                  METHOD := COMMAND_LINE_INPUT;      --  Found word in command line
         end ONE_ARGUMENT;
      
      --With two arguments the options are: inputfile and outputfile,
      --two Latin words, or a language shift to English (Latin being the startup default)
      --and an English  word (with no part of speech).
      --WORDS infile outfile
      --WORDS amo amas
      --WORDS ^e  love
      elsif Ada.Command_Line.ARGUMENT_COUNT = 2  then    --  INPUT and OUTPUT files
         TWO_ARGUMENTS:                                   --  or multiwords in-line
         declare
            INPUT_NAME  : constant STRING := TRIM(Ada.Command_Line.Argument(1));
            OUTPUT_NAME : constant STRING := TRIM(Ada.Command_Line.Argument(2));
         begin
           if INPUT_NAME(1) = CHANGE_LANGUAGE_CHARACTER  then
             if (INPUT_NAME'LENGTH > 1)  then 
                 CHANGE_LANGUAGE(INPUT_NAME(2));
                 ARGUMENTS_START := 2;
                 METHOD := COMMAND_LINE_INPUT;      --  Parse the one word 
              end if; 
            else
               OPEN(INPUT, IN_FILE, INPUT_NAME);
               CREATE(OUTPUT, OUT_FILE, OUTPUT_NAME);
               METHOD := COMMAND_LINE_FILES;
         
               SET_INPUT(INPUT);
               SET_OUTPUT(OUTPUT);
         
               SUPPRESS_PREFACE := TRUE;
               OUTPUT_SCREEN_SIZE := INTEGER'LAST;
               PARSE;           --  No additional arguments, so just go to PARSE now
         
               SET_INPUT(Ada.TEXT_IO.STANDARD_INPUT);    --  Clean up
               SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
               CLOSE(OUTPUT);
            end if;
            exception                  --  Triggers on either INPUT or OUTPUT  !!!
               when NAME_ERROR  =>
                  METHOD := COMMAND_LINE_INPUT;            --  Found words in command line
         
         end TWO_ARGUMENTS;
     
      --With three arguments there could be three Latin words or a language shift
      --and and English word and part of speech.
      --WORDS amo amas amat
      --WORDS ^e love v
      elsif Ada.Command_Line.ARGUMENT_COUNT = 3  then    --  INPUT and OUTPUT files
         THREE_ARGUMENTS:                                   --  or multiwords in-line
         declare
            ARG1 : constant STRING := TRIM(Ada.Command_Line.Argument(1));
            ARG2 : constant STRING := TRIM(Ada.Command_Line.Argument(2));
            ARG3 : constant STRING := TRIM(Ada.Command_Line.Argument(3));
         begin
           if ARG1(1) = CHANGE_LANGUAGE_CHARACTER  then
             if (ARG1'LENGTH > 1)  then 
                 CHANGE_LANGUAGE(ARG1(2));
                 ARGUMENTS_START := 2;
                 METHOD := COMMAND_LINE_INPUT;      --  Parse the one word 
              end if; 
            else
               METHOD := COMMAND_LINE_INPUT;
            end if;
             
         end THREE_ARGUMENTS;
      
      --More than three arguments must all be Latin words.
      --WORDS amo amas amat amamus amatis amant
      else    --  More than three arguments
      
         METHOD := COMMAND_LINE_INPUT;
      end if;
   
   
      if METHOD = COMMAND_LINE_INPUT  then            --  Process words in command line
         MORE_ARGUMENTS:
         begin
  --Ada.TEXT_IO.PUT_LINE("MORE_ARG  ARG_START = " & INTEGER'IMAGE(ARGUMENTS_START));
           SUPPRESS_PREFACE := TRUE;
            for I in ARGUMENTS_START..Ada.Command_Line.Argument_Count  loop  --  Assemble input words 
               INPUT_LINE := HEAD(TRIM(INPUT_LINE) & " " & Ada.Command_Line.Argument(I), 250);
            end loop;
  --Ada.TEXT_IO.PUT_LINE("To PARSE >" & TRIM(INPUT_LINE));
            PARSE(TRIM(INPUT_LINE));
         end MORE_ARGUMENTS;
      end if;
      end if;
   
   end MEANINGS;
   
   
   

