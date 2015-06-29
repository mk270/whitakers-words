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
   procedure print_main_banner(start_file_character : character;
                               change_parameters_character : character;
                               help_character : character)
   is
   begin

      preface.put_line(
        "Copyright (c) 1993-2006 - Free for any use - Version 1.97FC");
      preface.put_line(
        "For updates and latest version check http://www.erols.com/whitaker/words.htm");
      preface.put_line(
        "Comments? William Whitaker, Box 51225  Midland  TX  79710  USA - whitaker@erols.com");
      preface.new_line;
      preface.put_line(
        "Input a word or line of Latin and ENTER to get the forms and meanings");
      preface.put_line("    Or input " & start_file_character &
        " and the name of a file containing words or lines");
      preface.put_line("    Or input " & change_parameters_character &
        " to change parameters and mode of the program");
      preface.put_line("    Or input " & help_character &
        " to get help wherever available on individual parameters");
      preface.put_line(
        "Two empty lines (just a RETURN/ENTER) from the keyboard exits the program");

   end print_main_banner;

   procedure print_mode_warning
   is
   begin
      preface.put_line(
        "THIS VERSION IS HARDCODED TO GIVE DICTIONARY FORM AND MEANINGS ONLY");
      preface.put_line(
        "IT CANNOT BE MODIFIED BY CHANGING THE DO_MEANINGS_ONLY PARAMETER");
   end print_mode_warning;

end banner;
