with text_io; use text_io;
with inflections_package; use inflections_package;
procedure weed_all(w : in out string;
				   pofs : in part_of_speech_type) is
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
