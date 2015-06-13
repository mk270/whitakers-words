with text_io; 
with strings_package; use strings_package;  
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
procedure fil2dict is 
   package integer_io is new text_io.integer_io(integer);
   use text_io;
   use stem_key_type_io;
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
   
   
   d_k : dictionary_kind := xxx;       
   de: dictionary_entry := null_dictionary_entry;
   
   line : string(1..200) := (others => ' ');
   last : integer := 0;
   
   dictfile : dict_io.file_type;
   dictline : text_io.file_type;
   
begin
   put_line(
	 "Takes a DICTFILE.D_K and reconstructs the DICTLINE.D_K it came from");
   
   put("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
   text_io.get_line(line, last);
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
   
   
   dict_io.open(dictfile, in_file, add_file_name_extension(dict_file_name, 
	 dictionary_kind'image(d_k))); 
   
   
   
   create(dictline, out_file, add_file_name_extension(dict_line_name, 
	 "NEW"));
   --DICTIONARY_KIND'IMAGE(D_K)));
   
   
   while not end_of_file(dictfile)  loop
	  read(dictfile, de);
	  put(dictline, de);
   end loop;
   
end fil2dict;
