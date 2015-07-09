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
procedure listdict is
   package integer_io is new text_io.integer_io(integer);
   use text_io;
   use dictionary_entry_io;
   use part_entry_io;
   use translation_record_io;
   use age_type_io;
   use area_type_io;
   use geo_type_io;
   use frequency_type_io;
   use source_type_io;

   start_stem_1  : constant := 1;
   start_stem_2  : constant := start_stem_1 + max_stem_size + 1;
   start_stem_3  : constant := start_stem_2 + max_stem_size + 1;
   start_stem_4  : constant := start_stem_3 + max_stem_size + 1;
   start_part    : constant := start_stem_4 + max_stem_size + 1;
   start_tran    : constant integer :=
	 start_part +
	 integer(part_entry_io.default_width + 1);
   finish_line   : constant integer :=
	 start_tran +
	 translation_record_io.default_width - 1;

   input, output : text_io.file_type;
   de : dictionary_entry;

   s, line, blank_line : string(1..400) := (others => ' ');
   l, ll, last : integer := 0;
   j : natural := 0;

   function add(stem, infl : string) return string is
   begin
	  return head(trim(stem) & trim(infl), 20);
   end add;

begin
   text_io.put_line("LISTDICT takes a WORDS LISTDICT.IN in LIN (WORDS DICTLINE) form and ");
   text_io.put_line("    produces LISTDICT.OUT in ED (3 line) form");

   create(output, out_file, "LISTDICT.OUT");
   open(input, in_file, "LISTDICT.IN");

over_lines:
	while not end_of_file(input) loop
	   j := j + 1;
	   s := blank_line;
	   get_line(input, s, last);
	   if trim(s(1..last)) /= ""  then   --  Rejecting blank lines

	  form_de:
		  begin

			 de.stems(1) := s(start_stem_1..start_stem_1+max_stem_size-1);
			 de.stems(2) := s(start_stem_2..start_stem_2+max_stem_size-1);
			 de.stems(3) := s(start_stem_3..start_stem_3+max_stem_size-1);
			 de.stems(4) := s(start_stem_4..start_stem_4+max_stem_size-1);
			 get(s(start_part..last), de.part, l);
			 get(s(l+1..last), de.tran.age, l);
			 get(s(l+1..last), de.tran.area, l);
			 get(s(l+1..last), de.tran.geo, l);
			 get(s(l+1..last), de.tran.freq, l);
			 get(s(l+1..last), de.tran.source, l);
			 de.mean := head(s(l+2..last), max_meaning_size);
			 --  Note that this allows initial blanks
			 --  L+2 skips over the SPACER, required because this is STRING, not ENUM

		  exception
			 when others =>
				put_line("Exception");
				put_line(s(1..last));
				integer_io.put(integer(j)); new_line;
				put(de); new_line;
				raise;
		  end form_de;

		  put_line(output, s(start_stem_1..start_part-1));
		  put(output, s(start_part..start_tran-1)); put(output, "      ");
		  put(output, de.tran.age); put(output, " ");
		  put(output, de.tran.area); put(output, " ");
		  put(output, de.tran.geo); put(output, " ");
		  put(output, de.tran.freq); put(output, " ");
		  put(output, de.tran.source); new_line(output);
		  put_line(output, trim(de.mean));

	   end if;  --  Rejecting blank lines
	end loop over_lines;

	close(output);
exception
   when text_io.data_error  =>
	  null;
   when others =>
	  put_line(s(1..last));
	  integer_io.put(integer(j)); new_line;
	  close(output);

end listdict;
