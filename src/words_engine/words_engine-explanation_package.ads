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

with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
package Words_Engine.Explanation_Package is
   type Explanations is
     record
        Xxx_Meaning : Meaning_Type := Null_Meaning_Type; --  For TRICKS
        Yyy_Meaning : Meaning_Type := Null_Meaning_Type; --  For SYNCOPE
        Nnn_Meaning : Meaning_Type := Null_Meaning_Type; --  For Names
        Rrr_Meaning : Meaning_Type := Null_Meaning_Type; --  For Roman Numerals
        Ppp_Meaning : Meaning_Type := Null_Meaning_Type; --  For COMPOUNDED
     end record;
end Words_Engine.Explanation_Package;
