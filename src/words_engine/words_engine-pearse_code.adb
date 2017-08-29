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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Words_Engine.Pearse_Code
is
   function Format (S : Symbol) return String
   is
      Code_Str : constant String := Integer'Image (Symbol'Pos (S));
      Padded   : constant String := Tail (Trim (Code_Str, Ada.Strings.Left),
                                          2, '0');
   begin
      return Head (Padded, 3, ' ');
   end Format;
end Words_Engine.Pearse_Code;
