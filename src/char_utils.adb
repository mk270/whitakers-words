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

package body char_utils
is
   function is_member(needle : character;
                      haystack : character_array)
                     return boolean
   is
   begin
      for c in haystack'first .. haystack'last loop
         if haystack(c) = needle then
            return true;
         end if;
      end loop;
      return false;
   end is_member;

   function is_punctuation(c : character) return boolean
   is
      permitted : constant character_array :=
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
      return is_member(c, permitted);
   end is_punctuation;

   function is_alpha_etc(c : character) return boolean
   is
   begin
      return
        c in 'A' .. 'Z' or
        c in 'a' .. 'z' or
        c = '-' or
        c = '.';
   end is_alpha_etc;

end char_utils;
