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

with preface; use preface;

package body banner is
   procedure print_main_banner(start_file_Character : Character;
                               change_parameters_Character : Character;
                               help_Character : Character)
   is
   begin

      preface.Put_Line(
        "Copyright (c) 1993-2006 - Free for any use - Version 1.97FC");
      preface.Put_Line(
        "For updates and latest version check http://www.erols.com/whitaker/words.htm");
      preface.Put_Line(
        "Comments? William Whitaker, Box 51225  Midland  TX  79710  USA - whitaker@erols.com");
      preface.New_Line;
      preface.Put_Line(
        "InPut a word or line of Latin and ENTER to Get the forms and meanings");
      preface.Put_Line("    Or Input " & start_file_Character &
        " and the name of a file containing words or lines");
      preface.Put_Line("    Or Input " & change_parameters_Character &
        " to change parameters and mode of the program");
      preface.Put_Line("    Or Input " & help_Character &
        " to Get help wherever available on individual parameters");
      preface.Put_Line(
        "Two empty lines (just a RETURN/ENTER) from the keyboard exits the program");

   end print_main_banner;

   procedure print_mode_warning
   is
   begin
      preface.Put_Line(
        "THIS VERSION IS HARDCODED TO GIVE DICTIONARY FORM AND MEANINGS ONLY");
      preface.Put_Line(
        "IT CANNOT BE MODIFIED BY CHANGING THE DO_MEANINGS_ONLY PARAMETER");
   end print_mode_warning;

end banner;
