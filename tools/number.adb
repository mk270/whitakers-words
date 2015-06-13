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
procedure number is
   package integer_io is new text_io.integer_io(integer);
   use text_io;

   input : text_io.file_type;
   numbered : text_io.file_type;

   line : string(1..300) := (others => ' ');
   last, n : integer := 0;
   number_lines : boolean := false;

begin

   put_line(
	 "Takes a text file and produces a NUMBERED. file with line numbers");

   put_line("What file to NUMBER?");
   text_io.get_line(line, last);

   open(input, in_file, line(1..last));

   create(numbered, out_file, "NUMBERED.");

   while not end_of_file(input) loop
	  n := n + 1;

	  get_line(input, line, last);

	  text_io.put(numbered, integer'image(n));
	  set_col(numbered, 10);
	  text_io.put_line(numbered, line(1..last));

   end loop;

   close(numbered);

end number;
