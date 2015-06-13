with text_io;
with strings_package; use strings_package;
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
with dictionary_form;
procedure dictpage is
   --  DICTPAGE.IN -> DICTPAGE.OUT
   --  Takes DICTLINE form, puts # and dictionary form at begining,
   --  a file that can be sorted to produce word order of paper dictionary
   package integer_io is new text_io.integer_io(integer);
   use text_io;
   use dictionary_entry_io;
   use part_entry_io;
   use kind_entry_io;
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
   j : integer := 1;

   function add(stem, infl : string) return string is
   begin
	  return head(trim(stem) & trim(infl), 20);
   end add;

begin
   put_line("DICTPAGE.IN -> DICTPAGE.OUT");
   put_line("Takes DICTLINE form, puts # and dictionary form at begining, a file");
   put_line("for sorting with ::, a DICTPAGE.RAW to process for paper-like dictionary");
   put_line("Process result with PAGE2HTM to create a pretty display");

   create(output, out_file, "DICTPAGE.OUT");
   open(input, in_file, "DICTPAGE.IN");

over_lines:
	while not end_of_file(input) loop
	   s := blank_line;
	   get_line(input, s, last);
	   if trim(s(1..last)) /= ""  then   --  Rejecting blank lines

	  form_de:
		  begin

			 de.stems(1) := s(start_stem_1..max_stem_size);
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
		  end form_de;

		  put(output, "#" & dictionary_form(de));

		  --            if DE.PART.POFS = N  then
		  --              TEXT_IO.PUT(OUTPUT, "  " & GENDER_TYPE'IMAGE(DE.PART.N.GENDER) & "  ");
		  --            end if;
		  --            if (DE.PART.POFS = V)  and then  (DE.PART.V.KIND in GEN..PERFDEF)  then
		  --              TEXT_IO.PUT(OUTPUT, "  " & VERB_KIND_TYPE'IMAGE(DE.PART.V.KIND) & "  ");
		  --            end if;

		  text_io.put(output, " [");
		  age_type_io.put(output, de.tran.age);
		  area_type_io.put(output, de.tran.area);
		  geo_type_io.put(output, de.tran.geo);
		  frequency_type_io.put(output, de.tran.freq);
		  source_type_io.put(output, de.tran.source);
		  text_io.put(output, "]");

		  put(output, " :: ");
		  put_line(output, de.mean);

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

end dictpage;
