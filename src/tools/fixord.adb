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

with text_io;
with strings_package; use strings_package;
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
procedure fixord is
   use text_io;

   input, output : text_io.file_type;

   s, line, blank_line : string(1..400) := (others => ' ');
   l, ll, last : integer := 0;

begin
   put_line("FIXORD.IN -> FIXORD.OUT");
   put_line("Makes a clean (no #) 3 line ED format from LISTORD output");

   create(output, out_file, "FIXORD.OUT");
   open(input, in_file, "FIXORD.IN");

over_lines:
	while not end_of_file(input) loop
	   s := blank_line;
	   get_line(input, s, last);
	   if trim(s(1..last)) /= ""  then   --  Rejecting blank lines

		  if s(1) /= '#'  then
			 put_line(output, s(1..last));
		  end if;

	   end if;  --  Rejecting blank lines
	end loop over_lines;

	close(output);
exception
   when text_io.data_error  =>
	  close(output);

end fixord;
