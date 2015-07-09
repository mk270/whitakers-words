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

with text_io; use text_io;
with strings_package; use strings_package;
procedure invstems is
   line : string(1..250);
   ll : integer;
   subtype stem is string (1..18);
   blank_stem : constant stem := (others => ' ');
   sts : array (1..4) of stem;

   input, output : file_type;

   function invert(s : string) return string is
	  t : string(s'first..s'last);
   begin
	  if s(1) = ' '  then
		 return blank_stem;
	  else
		 for i in s'range  loop
			t(i) := s(s'last-i+1);
		 end loop;
		 return head(trim(t), 18);
	  end if;
   end invert;

begin
   put_line("Inverts the 4 stems of a DICTLINE form file INVERT_S.IN -> INVERT_S.OUT");

   create(output, out_file, "INVERT_S.OUT");
   open(input, in_file, "INVERT_S.IN");

   while not end_of_file(input)  loop
	  get_line(input, line, ll);
	  sts(1) := line(1..18);
	  sts(2) := line(20..37);
	  sts(3) := line(39..56);
	  sts(4) := line(58..75);
	  for i in 1..4  loop
		 sts(i) := invert(sts(i));
	  end loop;
	  line(1..18)  := sts(1) ;
	  line(20..37) := sts(2);
	  line(39..56) := sts(3);
	  line(58..75) := sts(4);
	  put_line(output, line(1..ll));

   end loop;

   close(output);

end invstems;
