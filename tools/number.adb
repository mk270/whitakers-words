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
