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

--
-- These codes are named for classicist and WORDS redistributor Roger Pearse
--

package Words_Engine.Pearse_Code is

   type Symbol is (
     Unknown,       -- 00
     Inflection,    -- 01
     Citation_Form, -- 02
     Gloss,         -- 03
     Unknowns_2,    -- 04
     Affix,         -- 05
     Trick          -- 06
   );

   function Format (S : Symbol) return String;

end Words_Engine.Pearse_Code;
