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

with Latin_Utils.Preface;
use Latin_Utils;

package body Banner is
   procedure Print_Main_Banner (Start_File_Character : Character;
                                Change_Parameters_Character : Character;
                                Help_Character : Character)
   is
      Version : constant String := "Version 1.97FC";
      URL     : constant String := "http://www.erols.com/whitaker/words.htm";
      Address : constant String := "Box 51225  Midland  TX  79710  USA";
   begin

      Preface.Put_Line (
        "Copyright (c) 1993-2006 - Free for any use - " & Version);
      Preface.Put_Line (
        "For updates and latest version check " & URL);
      Preface.Put_Line (
        "Comments? William Whitaker, " & Address & " - whitaker@erols.com");
      Preface.New_Line;
      Preface.Put_Line (
        "Input a word or line of Latin and ENTER to " &
        "get the forms and meanings");
      Preface.Put_Line ("    Or Input " & Start_File_Character &
        " and the name of a file containing words or lines");
      Preface.Put_Line ("    Or Input " & Change_Parameters_Character &
        " to change parameters and mode of the program");
      Preface.Put_Line ("    Or Input " & Help_Character &
        " to get help wherever available on individual parameters");
      Preface.Put_Line (
        "Two empty lines (just a RETURN/ENTER) from the " &
        "keyboard exits the program");

   end Print_Main_Banner;

   procedure Print_Mode_Warning
   is
   begin
      Preface.Put_Line (
        "THIS VERSION IS HARDCODED TO GIVE DICTIONARY FORM AND MEANINGS ONLY");
      Preface.Put_Line (
        "IT CANNOT BE MODIFIED BY CHANGING THE DO_MEANINGS_ONLY PARAMETER");
   end Print_Mode_Warning;

end Banner;
