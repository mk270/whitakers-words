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

package Latin_Utils.Config is

   Output_screen_size : Integer := 20;

   type configuration_type is (developer_version, user_version, only_meanings);

   type method_type is (interactive, Command_Line_Input, Command_Line_files);
   method : method_type := interactive;

   type language_type is (latin_to_english, english_to_latin);
   language : language_type := latin_to_english;

   suppress_preface : Boolean := False;

end Latin_Utils.Config;
