-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936–2010)
--
-- This is a free program, which means it is proper to copy it and pass
-- it on to your friends. Consider it a developmental item for which
-- there is no charge. However, just for form, it is Copyrighted
-- (c). Permission is hereby freely given for any and all use of program
-- and data. You can sell it as your own, but at least tell me.
-- 
-- This version is distributed without obligation, but the developer
-- would appreciate comments and suggestions.
-- 
-- All parts of the WORDS system, source code and data files, are made freely
-- available to anyone who wishes to use them, for whatever purpose.

with ada.command_line;
with ada.text_io; use ada.text_io;
with strings_package; use strings_package;
with config; use config;
with word_parameters; use word_parameters;
with developer_parameters; use developer_parameters;
with word_package; use word_package;
with process_input;

procedure words_main(configuration : configuration_type) is
   input_line  : string(1..250) := (others => ' ');
   arguments_start : integer := 1;
begin
   --  The language shift in argumants must take place here
   --  since later parsing of line ignores non-letter characters
   -- configuration := developer_version;

   --The main mode of usage for WORDS is a simple call, followed by screen interaction.
   if ada.command_line.argument_count = 0  then      --  Simple WORDS
      method := interactive;                          --  Interactive
      suppress_preface := false;
      set_output(ada.text_io.standard_output);
      initialize_word_parameters;
      initialize_developer_parameters;
      initialize_word_package;
      process_input(configuration);

      --But there are other, command line options.
      --WORDS may be called with arguments on the same line,
      --in a number of different modes.
      --
   else
      suppress_preface := true;
      initialize_word_parameters;
      initialize_developer_parameters;
      initialize_word_package;

      --Single parameter, either a simple Latin word or an input file.
      --WORDS amo
      --WORDS infile
      if ada.command_line.argument_count = 1  then      --  Input 1 word in-line
     one_argument:
         declare
            input_name  : constant string := trim(ada.command_line.argument(1));
         begin
            open(input, in_file, input_name); --  Try file name, not raises NAME_ERROR
            method := command_line_files;
            set_input(input);
            set_output(ada.text_io.standard_output);
            --  No additional arguments, so just go to PARSE now
            process_input(configuration);
         exception                  --  Triggers on INPUT
            when name_error  =>                   --  Raised NAME_ERROR therefore
               method := command_line_input;      --  Found word in command line
         end one_argument;

         --With two arguments the options are: inputfile and outputfile,
         --two Latin words, or a language shift to English (Latin being the startup default)
         --and an English  word (with no part of speech).
         --WORDS infile outfile
         --WORDS amo amas
         --WORDS ^e  love
      elsif ada.command_line.argument_count = 2  then    --  INPUT and OUTPUT files
     two_arguments:                                   --  or multiwords in-line
         declare
            input_name  : constant string := trim(ada.command_line.argument(1));
            output_name : constant string := trim(ada.command_line.argument(2));
         begin
            if input_name(1) = change_language_character  then
               if (input_name'length > 1)  then
                  change_language(input_name(2));
                  arguments_start := 2;
                  method := command_line_input;      --  Parse the one word
               end if;
            else
               open(input, in_file, input_name);
               create(output, out_file, output_name);
               method := command_line_files;

               set_input(input);
               set_output(output);

               suppress_preface := true;
               output_screen_size := integer'last;
               --  No additional arguments, so just go to PARSE now
               process_input(configuration);

               set_input(ada.text_io.standard_input);    --  Clean up
               set_output(ada.text_io.standard_output);
               close(output);
            end if;
         exception                  --  Triggers on either INPUT or OUTPUT  !!!
            when name_error  =>
               method := command_line_input;            --  Found words in command line

         end two_arguments;

         --With three arguments there could be three Latin words or a language shift
         --and and English word and part of speech.
         --WORDS amo amas amat
         --WORDS ^e love v
      elsif ada.command_line.argument_count = 3  then    --  INPUT and OUTPUT files
     three_arguments:                                   --  or multiwords in-line
         declare
            arg1 : constant string := trim(ada.command_line.argument(1));
            arg2 : constant string := trim(ada.command_line.argument(2));
            arg3 : constant string := trim(ada.command_line.argument(3));
         begin
            if arg1(1) = change_language_character  then
               if (arg1'length > 1)  then
                  change_language(arg1(2));
                  arguments_start := 2;
                  method := command_line_input;      --  Parse the one word
               end if;
            else
               method := command_line_input;
            end if;

         end three_arguments;

         --More than three arguments must all be Latin words.
         --WORDS amo amas amat amamus amatis amant
      else    --  More than three arguments

         method := command_line_input;
      end if;

      if method = command_line_input  then            --  Process words in command line
     more_arguments:
         begin
            --Ada.TEXT_IO.PUT_LINE("MORE_ARG  ARG_START = " & INTEGER'IMAGE(ARGUMENTS_START));
            suppress_preface := true;
            for i in arguments_start..ada.command_line.argument_count  loop  --  Assemble input words
               input_line := head(trim(input_line) & " " & ada.command_line.argument(i), 250);
            end loop;
            --Ada.TEXT_IO.PUT_LINE("To PARSE >" & TRIM(INPUT_LINE));
            process_input(configuration, trim(input_line));
         end more_arguments;
      end if;
   end if;

end words_main;
