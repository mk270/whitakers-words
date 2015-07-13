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

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
use Latin_Utils;
with Latin_Utils.Config; use Latin_Utils.Config;
with word_parameters; use word_parameters;
with developer_parameters; use developer_parameters;
with word_package; use word_package;
with process_Input;

procedure words_main (configuration : configuration_type) is
   Input_Line  : String (1 .. 250) := (others => ' ');
   Arguments_start : Integer := 1;
begin
   --  The language shift in argumants must take place here
   --  since later parsing of line ignores non-letter Characters
   -- configuration := developer_version;

   -- The main mode of usage for WORDS is a simple call, followed by
   -- screen interaction.
   if Ada.Command_Line.Argument_Count = 0  then      --  Simple WORDS
      method := interactive;                          --  Interactive
      suppress_preface := False;
      Set_Output (Ada.Text_IO.Standard_Output);
      initialize_word_parameters;
      initialize_developer_parameters;
      initialize_word_package;
      process_Input (configuration);

      --But there are other, command line options.
      --WORDS may be called with Arguments on the same line,
      --in a number of different modes.
      --
   else
      suppress_preface := True;
      initialize_word_parameters;
      initialize_developer_parameters;
      initialize_word_package;

      --Single parameter, either a simple Latin word or an Input file.
      --WORDS amo
      --WORDS infile
      if Ada.Command_Line.Argument_Count = 1  then      --  InPut 1 word in-line
         one_Argument :
         declare
            Input_name  : constant String :=
              Trim (Ada.Command_Line.Argument (1));
         begin
            --  Try file name, not raises NAME_ERROR
            Open (Input, In_File, Input_name);
            method := Command_Line_files;
            Set_Input (Input);
            Set_Output (Ada.Text_IO.Standard_Output);
            --  No additional Arguments, so just go to PARSE now
            process_Input (configuration);
         exception                  --  Triggers on INPUT
            when Name_Error  =>                 --  Raised NAME_ERROR therefore
               method := Command_Line_Input;    --  Found word in command line
         end one_Argument;

         --With two Arguments the options are: Inputfile and Outputfile,
         --two Latin words, or a language shift to English (Latin being
         --the startup default)

         --and an English  word (with no part of speech).
         --WORDS infile outfile
         --WORDS amo amas
         --WORDS ^e  love
      elsif Ada.Command_Line.Argument_Count = 2 then --  INPUT and OUTPUT files
         two_Arguments :                             --  or multiwords in-line
         declare
            Input_name  : constant String :=
              Trim (Ada.Command_Line.Argument (1));
            Output_name : constant String :=
              Trim (Ada.Command_Line.Argument (2));
         begin
            if Input_name (1) = change_language_Character  then
               if Input_name'Length > 1 then
                  change_language (Input_name (2));
                  Arguments_start := 2;
                  method := Command_Line_Input;      --  Parse the one word
               end if;
            else
               Open (Input, In_File, Input_name);
               Create (Output, Out_File, Output_name);
               method := Command_Line_files;

               Set_Input (Input);
               Set_Output (Output);

               suppress_preface := True;
               Output_screen_size := Integer'Last;
               --  No additional Arguments, so just go to PARSE now
               process_Input (configuration);

               Set_Input (Ada.Text_IO.Standard_Input);    --  Clean up
               Set_Output (Ada.Text_IO.Standard_Output);
               Close (Output);
            end if;
         exception                  --  Triggers on either INPUT or OUTPUT  !!!
            when Name_Error  =>
               method := Command_Line_Input;   --  Found words in command line

         end two_Arguments;

         --With three Arguments there could be three Latin words
         -- or a language shift
         --and and English word and part of speech.
         --WORDS amo amas amat
         --WORDS ^e love v
      elsif Ada.Command_Line.Argument_Count = 3  then
         --  INPUT and OUTPUT files or multiwords in-line
         three_Arguments :
         declare
            arg1 : constant String := Trim (Ada.Command_Line.Argument (1));
            -- we probably don't need to define these for their side-effects
            -- arg2 : constant String := Trim (Ada.Command_Line.Argument (2));
            -- arg3 : constant String := Trim (Ada.Command_Line.Argument (3));
         begin
            if arg1 (1) = change_language_Character  then
               if arg1'Length > 1 then
                  change_language (arg1 (2));
                  Arguments_start := 2;
                  method := Command_Line_Input;      --  Parse the one word
               end if;
            else
               method := Command_Line_Input;
            end if;
         end three_Arguments;

         --More than three Arguments must all be Latin words.
         --WORDS amo amas amat amamus amatis amant
      else    --  More than three Arguments

         method := Command_Line_Input;
      end if;

      if method = Command_Line_Input  then   --  Process words in command line
         more_Arguments :
         begin
            suppress_preface := True;
            --  Assemble Input words
            for i in Arguments_start .. Ada.Command_Line.Argument_Count  loop
               Input_Line := Head (
                 Trim (Input_Line) & " " & Ada.Command_Line.Argument (i), 250);
            end loop;
            --Ada.TEXT_IO.PUT_LINE ("To PARSE >" & TRIM (INPUT_LINE));
            process_Input (configuration, Trim (Input_Line));
         end more_Arguments;
      end if;
   end if;
end words_main;
