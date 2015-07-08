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

package body Char_Utils is

   ---------------------------------------------------------------------------

   function Is_Punctuation (C : Character) return Boolean
   is
      Permitted : constant array (Character) of Boolean :=
        ( ' ' => True, ',' => True, '-' => True, ';' => True, ':' => True,
          '.' => True, '(' => True, '[' => True, '{' => True, '<' => True,
          ')' => True, ']' => True, '}' => True, '>' => True,
          others => False
        );
   begin
      return Permitted (C);
   end Is_Punctuation;

   ---------------------------------------------------------------------------

   function Is_Alpha_Etc (C : Character) return Boolean
   is
      Alpha_Etc : constant array (Character) of Boolean :=
         ( 'A' .. 'Z' => True, 'a' .. 'z' => True, '-' => True, '.' => True,
           others => False
         );
   begin
      return Alpha_Etc (C);
   end Is_Alpha_Etc;

   ---------------------------------------------------------------------------

end Char_Utils;
