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

procedure Weed_All (W : in out String) is
   --  In contrast to the Latin phase where the prioritization takes
   --  is at runtime for the English most of the work is done beforehand
   --  both the setting of a priority class for each entry in the scan
   --  of DICTLINE and the WEEDing/TRIMming done herein
   Kill : Boolean := False;
begin

   if W'Length <= 1  then
      --if W (1)  not in  'A'..'Z'  then
      Kill := True;
      --end if;

   else

      if   --  WORDS words
        W = "DECL" or
        W = "DAT"  or
        W = "ACC"  or
        W = "ABL"  or
        W = "ADJ"  or
        W = "AD"  or
        W = "BC"  or
        W = "COMP"  or
        W = "SUPER"  or
        W = "DEMONST"  or
        W = "INDEF"  or
        W = "INF"  or
        W = "KLUDGE"  or
        W = "NE"  or
        W = "NW"  or
        W = "SE"  or
        W = "SW"  or
        W = "NT"  or
        W = "OT"  or
        W = "PASS"  or
        W = "L+S"  or
        W = "St"

      then

         Kill := True;
      end if;

      if
        --  Articles
        W = "a"    or
        W = "an"   or
        W = "the"  or
        W = "The"  or

        --  Others
        W = "no"

      then

         Kill := True;
      end if;

      if   --  Fragments
        W = "ad"   or
        W = "de"   or
        W = "bi"   or
        W = "di"   or
        W = "re"   or
        W = "ex"
      then
         Kill := True;
      end if;

      if
        W = "abb"   or     --  Abbreviation
                           --  Number suffixes
        W = "st"   or      --  1st
        W = "nd"   or      --  2nd
        W = "rd"   or      --  3rd
        W = "th"           --  4th
      then
         Kill := True;
      end if;

      --  Kill abbreviations
      if W (W'Last) = '.'  then
         Kill := True;
      end if;

      --  Kill internal AREA
      if W (W'Last) = ':'  then
         Kill := True;
      end if;

   end if;

   if Kill then
      for I in W'Range  loop
         W (I) := '\';
      end loop;
   end if;

   --PUT_LINE ("WEEDed ANY  "  & W & '|' & BOOLEAN'IMAGE (KILL));

end Weed_All;
