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
procedure dictflag is
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
   use dict_io;

   be_ve : verb_entry := (con => (5, 1), kind => to_be);

   d_k : dictionary_kind := xxx;       --  ######################

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

   age_array : array (age_type'range) of integer := (others => 0);

   area_array : array (area_type'range) of integer := (others => 0);

   geo_array : array (geo_type'range) of integer := (others => 0);

   freq_array : array (frequency_type'range) of integer := (others => 0);

   source_array : array (source_type'range) of integer := (others => 0);

   dictfile : dict_io.file_type;
   input, output : text_io.file_type;
   de : dictionary_entry;

   s, line, blank_line : string(1..400) := (others => ' ');
   l, ll, last : integer := 0;
   j : dict_io.count := 0;
   mean_to_be : constant meaning_type :=
	 head("to be, exist; also used to form verb perfect passive tenses" &
	 " with NOM PERF PPL", max_meaning_size);

begin
   put_line(
	 "Takes a DICTLINE.D_K and produces a numeration of FLAGS");
   put("What dictionary to list, GENERAL or SPECIAL  =>");
   get_line(line, last);
   if last > 0  then
	  if trim(line(1..last))(1) = 'G'  or else
		trim(line(1..last))(1) = 'g'     then
		 d_k := general;
	  elsif trim(line(1..last))(1) = 'S'  or else
		trim(line(1..last))(1) = 's'     then
		 d_k := special;
	  else
		 put_line("No such dictionary");
		 raise text_io.data_error;
	  end if;
   end if;

   open(input, in_file, add_file_name_extension(dict_line_name,
	 dictionary_kind'image(d_k)));

   create(output, out_file, add_file_name_extension("FLAGS",
	 dictionary_kind'image(d_k)));

   while not end_of_file(input) loop
	  s := blank_line;
	  get_line(input, s, last);
	  if trim(s(1..last)) /= ""  then
		 l := 0;

	 form_de:
		 begin

			de.stems(1) := s(start_stem_1..max_stem_size);
            --NEW_LINE; PUT(DE.STEMS(1));
			de.stems(2) := s(start_stem_2..start_stem_2+max_stem_size-1);
			de.stems(3) := s(start_stem_3..start_stem_3+max_stem_size-1);
			de.stems(4) := s(start_stem_4..start_stem_4+max_stem_size-1);
            --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('@');
			get(s(start_part..last), de.part, l);
            --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('&'); PUT(S(L+1..LAST)); PUT('3');
            --   GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);

			get(s(l+1..last), de.tran.age, l);
			age_array(de.tran.age) := age_array(de.tran.age) + 1;
			get(s(l+1..last), de.tran.area, l);
			area_array(de.tran.area) := area_array(de.tran.area) + 1;
			get(s(l+1..last), de.tran.geo, l);
			geo_array(de.tran.geo) := geo_array(de.tran.geo) + 1;
			get(s(l+1..last), de.tran.freq, l);
			freq_array(de.tran.freq) := freq_array(de.tran.freq) + 1;
			get(s(l+1..last), de.tran.source, l);
			source_array(de.tran.source) := source_array(de.tran.source) + 1;

			de.mean := head(s(l+2..last), max_meaning_size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING, not ENUM

		 exception
			when others =>
			   new_line;
			   put_line("Exception");
			   put_line(s(1..last));
			   integer_io.put(integer(j)); new_line;
			   put(de); new_line;
		 end form_de;

		 j := j + 1;

	  end if;
   end loop;

   text_io.put(output, "Number of lines in DICTLINE "  &
	 dictionary_kind'image(d_k) & "  ");
   integer_io.put(output, integer(j));
   text_io.new_line(output);

   text_io.new_line(output, 4);
   text_io.put_line(output, "AGE");
   for i in age_type'range  loop
	  text_io.put(output, age_type'image(i));
	  text_io.set_col(output, 10);
	  text_io.put_line(output, integer'image(age_array(i)));
   end loop;

   text_io.new_line(output, 4);
   text_io.put_line(output, "AREA");
   for i in area_type'range  loop
	  text_io.put(output, area_type'image(i));
	  text_io.set_col(output, 10);
	  text_io.put_line(output, integer'image(area_array(i)));
   end loop;

   text_io.new_line(output, 4);
   text_io.put_line(output, "GEO");
   for i in geo_type'range  loop
	  text_io.put(output, geo_type'image(i));
	  text_io.set_col(output, 10);
	  text_io.put_line(output, integer'image(geo_array(i)));
   end loop;

   text_io.new_line(output, 4);
   text_io.put_line(output, "FREQ");
   for i in frequency_type'range  loop
	  text_io.put(output, frequency_type'image(i));
	  text_io.set_col(output, 10);
	  text_io.put_line(output, integer'image(freq_array(i)));
   end loop;

   text_io.new_line(output, 4);
   text_io.put_line(output, "SOURCE");
   for i in source_type'range  loop
	  text_io.put(output, source_type'image(i));
	  text_io.set_col(output, 10);
	  text_io.put_line(output, integer'image(source_array(i)));
   end loop;

   close(output);

exception
   when text_io.data_error  =>
	  null;
   when others =>
	  put_line(s(1..last));
	  integer_io.put(integer(j)); new_line;
	  close(output);

end dictflag;
