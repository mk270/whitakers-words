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
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Words_Engine.Word_Package; use Words_Engine.Word_Package;
with Words_Engine.Initialization;
with Process_Input;

procedure Words_Main (Configuration : Configuration_Type) is
   Input_Line  : String (1 .. 250) := (others => ' ');
   Arguments_Start : Integer := 1;

begin
   --  The language shift in arguments must take place here
   --  since later parsing of line ignores non-letter Characters
   -- configuration := developer_version;

   -- The main mode of usage for WORDS is a simple call, followed by
   -- screen interaction.
   if Ada.Command_Line.Argument_Count = 0  then      --  Simple WORDS
      Method := Interactive;                          --  Interactive
      Suppress_Preface := False;
      Set_Output (Ada.Text_IO.Standard_Output);
      Words_Engine.Initialization.Initialize_Engine;
      Process_Input (Configuration);

      --But there are other, command line options.
      --WORDS may be called with Arguments on the same line,
      --in a number of different modes.
      --
   else
      Suppress_Preface := True;
      Words_Engine.Initialization.Initialize_Engine;

      --Single parameter, either a simple Latin word or an Input file.
      --WORDS amo
      --WORDS infile
      if Ada.Command_Line.Argument_Count = 1  then      --  InPut 1 word in-line
         One_Argument :
         declare
            Input_Name  : constant String :=
              Trim (Ada.Command_Line.Argument (1));
         begin
            --  Try file name, not raises NAME_ERROR
            Open (Input, In_File, Input_Name);
            Method := Command_Line_Files;
            Set_Input (Input);
            Set_Output (Ada.Text_IO.Standard_Output);
            --  No additional Arguments, so just go to PARSE now
            Process_Input (Configuration);
         exception                  --  Triggers on INPUT
            when Name_Error  =>                 --  Raised NAME_ERROR therefore
               Method := Command_Line_Input;    --  Found word in command line
         end One_Argument;

         --With two Arguments the options are: Inputfile and Outputfile,
         --two Latin words, or a language shift to English (Latin being
         --the startup default)

         --and an English  word (with no part of speech).
         --WORDS infile outfile
         --WORDS amo amas
         --WORDS ^e  love
      elsif Ada.Command_Line.Argument_Count = 2 then --  INPUT and OUTPUT files
         Two_Arguments :                             --  or multiwords in-line
         declare
            Input_Name  : constant String :=
              Trim (Ada.Command_Line.Argument (1));
            Output_Name : constant String :=
              Trim (Ada.Command_Line.Argument (2));
         begin
            if Input_Name (1) = Change_Language_Character  then
               if Input_Name'Length > 1 then
                  Change_Language (Input_Name (2));
                  Arguments_Start := 2;
                  Method := Command_Line_Input;      --  Parse the one word
               end if;
            else
               Open (Input, In_File, Input_Name);
               Create (Output, Out_File, Output_Name);
               Method := Command_Line_Files;

               Set_Input (Input);
               Set_Output (Output);

               Suppress_Preface := True;
               Output_Screen_Size := Integer'Last;
               --  No additional Arguments, so just go to PARSE now
               Process_Input (Configuration);

               Set_Input (Ada.Text_IO.Standard_Input);    --  Clean up
               Set_Output (Ada.Text_IO.Standard_Output);
               Close (Output);
            end if;
         exception                  --  Triggers on either INPUT or OUTPUT  !!!
            when Name_Error  =>
               Method := Command_Line_Input;   --  Found words in command line

         end Two_Arguments;

         --With three Arguments there could be three Latin words
         -- or a language shift
         --and and English word and part of speech.
         --WORDS amo amas amat
         --WORDS ^e love v
      elsif Ada.Command_Line.Argument_Count = 3  then
         --  INPUT and OUTPUT files or multiwords in-line
         Three_Arguments :
         declare
            Arg1 : constant String := Trim (Ada.Command_Line.Argument (1));
            -- we probably don't need to define these for their side-effects
            -- arg2 : constant String := Trim (Ada.Command_Line.Argument (2));
            -- arg3 : constant String := Trim (Ada.Command_Line.Argument (3));
         begin
            if Arg1 (1) = Change_Language_Character  then
               if Arg1'Length > 1 then
                  Change_Language (Arg1 (2));
                  Arguments_Start := 2;
                  Method := Command_Line_Input;      --  Parse the one word
               end if;
            else
               Method := Command_Line_Input;
            end if;
         end Three_Arguments;

         --More than three Arguments must all be Latin words.
         --WORDS amo amas amat amamus amatis amant
      else    --  More than three Arguments

         Method := Command_Line_Input;
      end if;

      if Method = Command_Line_Input  then   --  Process words in command line
         More_Arguments :
         begin
            Suppress_Preface := True;
            --  Assemble Input words
            for I in Arguments_Start .. Ada.Command_Line.Argument_Count  loop
               Input_Line := Head (
                 Trim (Input_Line) & " " & Ada.Command_Line.Argument (I), 250);
            end loop;
            --Ada.TEXT_IO.PUT_LINE ("To PARSE >" & TRIM (INPUT_LINE));
            Process_Input (Configuration, Trim (Input_Line));
         end More_Arguments;
      end if;
   end if;
end Words_Main;
