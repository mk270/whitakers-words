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

with Dictionary_Package; use Dictionary_Package;
package tricks_package is

   procedure syncope(w : String;
                     pa : in out Parse_Array; pa_last : in out Integer);

   procedure try_tricks(w : String;
                        pa : in out Parse_Array; pa_last : in out Integer;
                                                 line_number : Integer; word_number : Integer);

   procedure try_slury(w : String;
                       pa : in out Parse_Array; pa_last : in out Integer;
                                                line_number : Integer; word_number : Integer);

   procedure roman_numerals(Input_word : String;
                            pa : in out Parse_Array; pa_last : in out Integer);

end tricks_package;
