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
procedure linefile is
   package integer_io is new text_io.integer_io(integer);
   use text_io;
   use dictionary_entry_io;
   use dict_io;

   dictfile : dict_io.file_type;
   output : text_io.file_type;
   de : dictionary_entry;
   d_k : dictionary_kind := general;
   line : string(1..40) := (others => ' ');
   last : integer := 0;

begin
   put_line("Takes a DICTFILE.D_K and produces a DICTLINE.D_K");
   put("What dictionary to convert, GENERAL or SPECIAL  (Reply G or S) =>");
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

   open(dictfile, in_file, add_file_name_extension(dict_file_name,
	 dictionary_kind'image(d_k)));

   create(output, out_file, add_file_name_extension("DICT_NEW",
	 dictionary_kind'image(d_k)));

   while not end_of_file(dictfile)  loop
	  read(dictfile, de);
	  put(output, de);
	  text_io.new_line(output);
   end loop;

end linefile;
