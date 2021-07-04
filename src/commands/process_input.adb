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

with Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Latin_Utils.Preface;
with Words_Engine.Word_Package; use Words_Engine.Word_Package;
with Latin_Utils.Config; use Latin_Utils.Config;
with Words_Engine.English_Support_Package;
use Words_Engine.English_Support_Package;
with Banner;
use Latin_Utils;

with Words_Engine.Parse;

pragma Elaborate (Support_Utils.Word_Parameters);

procedure Process_Input (Configuration : Configuration_Type;
                         Command_Line : String := "")
is
   -- use Inflection_Record_IO;
   use Ada.Text_IO;

   procedure Delete_If_Open (Dict_Name : Dictionary_Kind) is
   begin
      begin
         if Dict_IO.Is_Open (Dict_File (Dict_Name)) then
            Dict_IO.Delete (Dict_File (Dict_Name));
         else
            Dict_IO.Open (Dict_File (Dict_Name), Dict_IO.In_File,
                          Path (Dict_File_Name & '.' & Ext (Dict_Name)));
            Dict_IO.Delete (Dict_File (Dict_Name));
         end if;
      exception when others => null;
      end;   --  not there, so don't have to DELETE
   end Delete_If_Open;

   -- Get and handle a line of Input
   -- return value says whether there is more Input, i.e. False -> quit
   function Get_Input_Line return Boolean
   is
      Blank_Line : constant String (1 .. 2500) := (others => ' ');
      Line : String (1 .. 2500) := (others => ' ');
      L : Integer := 0;
   begin
      --  Block to manipulate file of lines
      if Name (Current_Input) = Name (Standard_Input) then
         Scroll_Line_Number :=
           Integer (Ada.Text_IO.Line (Ada.Text_IO.Standard_Output));
         Preface.New_Line;
         Preface.Put ("=>");
      end if;

      Line := Blank_Line;
      Get_Line (Line, L);
      if (L = 0) or else (Trim (Line (1 .. L)) = "")  then
         --  Count blank lines
         --LINE_NUMBER := LINE_NUMBER + 1;
         if Name (Current_Input) = Name (Standard_Input) then
            --  INPUT is keyboard
            Preface.Put ("Blank exits =>");
            Get_Line (Line, L);
            -- Second try
            if (L = 0) or else (Trim (Line (1 .. L)) = "")  then
               -- Two in a row
               return False;
            end if;
         else
            --  INPUT is file

            --LINE_NUMBER := LINE_NUMBER + 1;
            --  Count blank lines in file
            if End_Of_File (Current_Input) then
               Set_Input (Standard_Input);
               Close (Input);
            end if;
         end if;
      end if;

      if Trim (Line (1 .. L)) /= "" then
         -- Not a blank line so L (1) (in file Input)
         if Line (1) = Start_File_Character  then
            if Name (Current_Input) /= Name (Standard_Input) then
               Ada.Text_IO.Put_Line ("Cannot have file of words (@FILE) " &
                 "in an @FILE");
            else
               Ada.Text_IO.Open
                 (Input, Ada.Text_IO.In_File, Trim (Line (2 .. L)));
               Ada.Text_IO.Set_Input (Input);
            end if;
         elsif Line (1) = Change_Parameters_Character  and then
           (Name (Current_Input) = Name (Standard_Input)) and then
           not Config.Suppress_Preface
         then
            Change_Parameters;
         elsif Line (1) = Change_Language_Character  then
            Change_Language (Line (2));
         elsif
           Line (1) = Change_Developer_Modes_Character  and then
           (Name (Current_Input) = Name (Standard_Input)) and then
           not Config.Suppress_Preface
         then
            Change_Developer_Modes;
         else
            if Name (Current_Input) /= Name (Standard_Input) then
               Preface.New_Line;
               Preface.Put_Line (Line (1 .. L));
            end if;
            if Words_Mode (Write_Output_To_File)     then
               if not Config.Suppress_Preface     then
                  New_Line (Output);
                  Ada.Text_IO.Put_Line (Output, Line (1 .. L));
               end if;
            end if;
            --  Count lines to be parsed
            Line_Number := Line_Number + 1;

            Words_Engine.Parse.Parse_Line (Configuration, Line (1 .. L));
         end if;
      end if;

      return True;

   exception
      when Name_Error | Use_Error =>
         if Name (Current_Input) /= Name (Standard_Input) then
            Set_Input (Standard_Input);
            Close (Input);
         end if;
         Put_Line ("An unknown or unacceptable file name. Try Again");
         return True;
      when End_Error =>
         --  The end of the input file resets to CON:
         if Name (Current_Input) /= Name (Standard_Input) then
            Set_Input (Standard_Input);
            Close (Input);
            if Method = Command_Line_Files then
               raise Give_Up;
            end if;
            return True;
         else
            Put_Line ("Raised END_ERROR, although in STANDARD_INPUT");
            Put_Line ("^Z is inappropriate keyboard Input, " &
              "WORDS should be terminated with a blank line");
            raise Give_Up;
         end if;
      when Status_Error =>
         --  The end of the Input file resets to CON:
         Put_Line ("Raised STATUS_ERROR");
         return False;
   end Get_Input_Line;

begin
   --  PARSE
   if Method = Command_Line_Input  then
      if Trim (Command_Line) /= ""  then
         Words_Engine.Parse.Parse_Line (Configuration, Command_Line);
      end if;

   else
      Banner.Print_Main_Banner (Start_File_Character,
        Change_Parameters_Character, Help_Character);

      if English_Dictionary_Available (General)  then
         Preface.Put_Line ("English-to-Latin available");
         Preface.Put_Line (
           Change_Language_Character & "E changes to English-to-Latin, " &
           Change_Language_Character & "L changes back     [tilde E]");
      end if;

      if Configuration = Only_Meanings  then
         Banner.Print_Mode_Warning;
      end if;

      while Get_Input_Line loop
         null;
      end loop;

   end if;     --  On command line Input

   begin
      Stem_Io.Open (Stem_File (Local), Stem_Io.In_File,
                    Path (Stem_File_Name & '.' & Ext (Local)));
      --  Failure to OPEN will raise an exception, to be handled below
      if Stem_Io.Is_Open (Stem_File (Local)) then
         Stem_Io.Delete (Stem_File (Local));
      end if;
   exception
      when others =>
         null;      --  If cannot OPEN then it does not exist, so is deleted
   end;
   --  The rest of this seems like overkill, it might have been done elsewhere

   Delete_If_Open (Local);
   Delete_If_Open (Addons);
   Delete_If_Open (Unique);

exception
   when Storage_Error  =>    --  Have tried at least twice, fail
      Preface.Put_Line ("Continuing STORAGE_ERROR Exception in PARSE");
      Preface.Put_Line ("If insufficient memory in DOS, try removing TSRs");
   when Give_Up  =>
      Preface.Put_Line ("Giving up!");
   when others  =>
      Preface.Put_Line ("Unexpected exception raised in PARSE");
end Process_Input;
