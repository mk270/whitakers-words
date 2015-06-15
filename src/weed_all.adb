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

with inflections_package; use inflections_package;
procedure weed_all(w : in out string) is
   --  In contrast to the Latin phase where the prioritization takes is at runtime
   --  for the English most of the work is done beforehand
   --  both the setting of a priority class for each entry in the scan of DICTLINE
   --  and the WEEDing/TRIMming done herein
   kill : boolean := false;
begin

   if w'length <= 1  then
      --if W(1)  not in  'A'..'Z'  then
      kill := true;
      --end if;

   else

      if   --  WORDS words
        w = "DECL" or
        w = "DAT"  or
        w = "ACC"  or
        w = "ABL"  or
        w = "ADJ"  or
        w = "AD"  or
        w = "BC"  or
        w = "COMP"  or
        w = "SUPER"  or
        w = "DEMONST"  or
        w = "INDEF"  or
        w = "INF"  or
        w = "KLUDGE"  or
        w = "NE"  or
        w = "NW"  or
        w = "SE"  or
        w = "SW"  or
        w = "NT"  or
        w = "OT"  or
        w = "PASS"  or
        w = "L+S"  or
        w = "St"

      then

         kill := true;
      end if;

      if
        --  Articles
        w = "a"    or
        w = "an"   or
        w = "the"  or
        w = "The"  or

        --  Others
        w = "no"

      then

         kill := true;
      end if;

      if   --  Fragments
        w = "ad"   or
        w = "de"   or
        w = "bi"   or
        w = "di"   or
        w = "re"   or
        w = "ex"
      then
         kill := true;
      end if;

      if
        w = "abb"   or     --  Abbreviation
                           --  Number suffixes
        w = "st"   or      --  1st
        w = "nd"   or      --  2nd
        w = "rd"   or      --  3rd
        w = "th"           --  4th
      then
         kill := true;
      end if;

      --  Kill abbreviations
      if w(w'last) = '.'  then
         kill := true;
      end if;

      --  Kill internal AREA
      if w(w'last) = ':'  then
         kill := true;
      end if;

   end if;

   if kill then
      for i in w'range  loop
         w(i) := '\';
      end loop;
   end if;

   --PUT_LINE("WEEDed ANY  "  & W & '|' & BOOLEAN'IMAGE(KILL));

end weed_all;
