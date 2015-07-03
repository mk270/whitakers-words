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

package char_utils
is
   type Character_array is array (Integer range <>) of Character;

   function is_member(needle : Character;
                      haystack : Character_array)
                     return Boolean;

   function is_punctuation(c : Character) return Boolean;

   -- is c alphabetic, or '.' or '-' ?
   function is_alpha_etc(c : Character) return Boolean;

end char_utils;
