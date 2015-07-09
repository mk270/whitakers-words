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
with dictionary_form;
procedure uniqpage is

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

   uniques_file, uniqpage : text_io.file_type;

   s, line, blank_line, blanks : string(1..400) := (others => ' ');
   l, ll, last : integer := 0;

   stem : stem_type := null_stem_type;
   qual : quality_record;
   kind : kind_entry;
   tran : translation_record;
   mean : meaning_type;

   procedure get_line_unique(input : in text_io.file_type; s : out string; last : out natural) is
   begin
	  last := 0;
	  text_io.get_line(input, s, last);
	  if trim(s(1..last)) /= ""  then   --  Rejecting blank lines
		 null;
	  end if;
   end get_line_unique;

begin
   put_line("UNIQUES.LAT -> UNIQPAGE.PG");
   put_line("Takes UNIQUES form, single lines it, puts # at begining,");
   put_line("producing a .PG file for sorting to produce paper dictionary");
   create(uniqpage, out_file, "UNIQPAGE.PG");
   open(uniques_file, in_file, "UNIQUES.LAT");

over_lines:
    while not end_of_file(uniques_file)  loop
	   line := blanks;
	   get_line_unique(uniques_file, line, last);      --  STEM
	   stem := head(trim(line(1..last)), max_stem_size);

	   line := blanks;
	   get_line_unique(uniques_file, line, last);    --  QUAL, KIND, TRAN
	   quality_record_io.get(line(1..last), qual, l);
	   get(line(l+1..last), qual.pofs, kind, l);
	   age_type_io.get(line(l+1..last), tran.age, l);
	   area_type_io.get(line(l+1..last), tran.area, l);
	   geo_type_io.get(line(l+1..last), tran.geo, l);
	   frequency_type_io.get(line(l+1..last), tran.freq, l);
	   source_type_io.get(line(l+1..last), tran.source, l);

	   line := blanks;
	   get_line_unique(uniques_file, line, l);         --  MEAN
	   mean := head(trim(line(1..l)), max_meaning_size);

	   --      while not END_OF_FILE(UNIQUES_FILE) loop
	   --         S := BLANK_LINE;
	   --         GET_LINE(INPUT, S, LAST);
	   --         if TRIM(S(1..LAST)) /= ""  then   --  Rejecting blank lines
	   --
	   --

	   text_io.put(uniqpage, "#" & stem);

	   quality_record_io.put(uniqpage, qual);

	   -- PART := (V, (QUAL.V.CON, KIND.V_KIND));

	   if (qual.pofs = v)  and then  (kind.v_kind in gen..perfdef)  then
		  text_io.put(uniqpage, "  " & verb_kind_type'image(kind.v_kind) & "  ");
	   end if;

	   text_io.put(uniqpage, " [");
	   age_type_io.put(uniqpage, tran.age);
	   area_type_io.put(uniqpage, tran.area);
	   geo_type_io.put(uniqpage, tran.geo);
	   frequency_type_io.put(uniqpage, tran.freq);
	   source_type_io.put(uniqpage, tran.source);
	   text_io.put(uniqpage, "]");

	   put(uniqpage, " :: ");
	   put_line(uniqpage, mean);

	   --end if;  --  Rejecting blank lines
	end loop over_lines;

	close(uniqpage);
exception
   when text_io.data_error  =>
	  null;
   when others =>
	  put_line(s(1..last));
	  close(uniqpage);

end uniqpage;
