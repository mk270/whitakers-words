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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Words_Engine.Trick_Tables is
   type Strings is array (Integer range <>) of Unbounded_String;

   function Member (Needle   : Unbounded_String;
                    Haystack : Strings) return Boolean;

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   function Common_Prefix (S : String) return Boolean;
end Words_Engine.Trick_Tables;
