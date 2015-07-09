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
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
procedure linedict is
   package integer_io is new text_io.integer_io(integer);
   use dictionary_entry_io;
   use part_entry_io;
   use kind_entry_io;
   use age_type_io;
   use area_type_io;
   use geo_type_io;
   use frequency_type_io;
   use source_type_io;

   de : dictionary_entry;

   dictionary_file : file_type;
   output : file_type;

   st : stem_type := null_stem_type;
   blk_stem : constant stem_type := null_stem_type;
   sts : stems_type := null_stems_type;
   mean : meaning_type := null_meaning_type;
   pt  : part_entry  := null_part_entry;

   line, st_line, pt_line, mn_line, blank_line : string(1..300) :=
	 (others => ' ');
   l, ll, last, len : integer := 0;
   number_of_dictionary_entries : integer := 0;

   procedure get_stem(s : in string;
                      stem : out stem_type; last : out integer) is
	  i  : integer := 1;
	  l  : integer := s'first;
   begin
	  stem := null_stem_type;
      --  Squeeze left
	  while l <= s'last and then s(l) = ' '  loop
		 l := l + 1;
	  end loop;
      --  Count until the first blank
      --  Return that string
	  while l <= s'last and then s(l) /= ' '  loop
		 stem(i) := s(l);
		 i := i + 1;
		 l := l + 1;
	  end loop;
      --  Return  last
	  last := l;
   end get_stem;

begin
   put_line("LINEDICT.IN (EDIT format - 3 lines) -> LINEDICT.OUT (DICTLINE format)");

   create(output, out_file, "LINEDICT.OUT");

   open(dictionary_file, in_file, "LINEDICT.IN");
   put("Dictionary loading");

   while not end_of_file(dictionary_file)  loop
	  st_line := blank_line;
	  pt_line := blank_line;
	  mn_line := blank_line;
	  error_check:
		  begin

			 get_non_comment_line(dictionary_file, st_line, last);      --  STEMS

			 line := blank_line;
			 get_non_comment_line(dictionary_file, pt_line, l);           --  PART
			 get(pt_line(1..l), de.part, ll);
			 --            GET(PT_LINE(LL+1..L), DE.PART.POFS, DE.KIND, LL);

			 get(pt_line(ll+1..l), de.tran.age, ll);
			 get(pt_line(ll+1..l), de.tran.area, ll);
			 get(pt_line(ll+1..l), de.tran.geo, ll);
			 get(pt_line(ll+1..l), de.tran.freq, ll);
			 get(pt_line(ll+1..l), de.tran.source, ll);

			 de.stems := null_stems_type;
			 ll := 1;
			 --  Extract up to 4 stems

			 for i in 1..number_of_stems(de.part.pofs)  loop   --  EXTRACT STEMS
				get_stem(st_line(ll..last), de.stems(i), ll);
			 end loop;

			 line := blank_line;
			 get_non_comment_line(dictionary_file, mn_line, l);         --  MEANING

			 de.mean := head(trim(mn_line(1..l)), max_meaning_size);

			 put(output, de); new_line(output);

			 number_of_dictionary_entries := number_of_dictionary_entries + 1;

		  exception
			 when others  =>
				put_line("-------------------------------------------------------------");
				put_line(head(st_line, 78));
				put_line(head(pt_line, 78));
				put_line(head(mn_line, 78));
		  end error_check;

   end loop;
   close(dictionary_file);
   close(output);
   set_col(33); put("--  "); integer_io.put(number_of_dictionary_entries);
   put(" entries");    set_col(55); put_line("--  Loaded correctly");
end linedict;
