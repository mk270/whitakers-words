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

   function Is_Member
      ( Needle   : Character;
        Haystack : Character_Array
      ) return Boolean
   is
   begin
      for C in Haystack'Range loop
         if Haystack (C) = Needle then
            return True;
         end if;
      end loop;
      return False;
   end Is_Member;

   ---------------------------------------------------------------------------

   function Is_Punctuation (C : Character) return Boolean
   is
      Permitted : constant Character_Array :=
        (
        ' ',
        ',',
        '-',
        ';',
        ':',
        '.',
        '(',
        '[',
        '{',
        '<',
        ')',
        ']',
        '}',
        '>'
        );
   begin
      return Is_Member (C, Permitted);
   end Is_Punctuation;

   ---------------------------------------------------------------------------

   function Is_Alpha_Etc (C : Character) return Boolean
   is
   begin
      return
        C in 'A' .. 'Z' or
        C in 'a' .. 'z' or
        C = '-' or
        C = '.';
   end Is_Alpha_Etc;

   ---------------------------------------------------------------------------

end Char_Utils;
