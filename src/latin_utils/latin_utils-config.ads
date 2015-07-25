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

   Output_Screen_Size : Integer := 20;

   type Configuration_Type is (Developer_Version, User_Version, Only_Meanings);

   type Method_Type is (Interactive, Command_Line_Input, Command_Line_Files);
   Method : Method_Type := Interactive;

   type Language_Type is (Latin_To_English, English_To_Latin);
   Language : Language_Type := Latin_To_English;

   Suppress_Preface : Boolean := False;

end Latin_Utils.Config;
