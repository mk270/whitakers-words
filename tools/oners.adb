with text_io; use text_io;
procedure oners is
   package integer_io is new text_io.integer_io(integer);
   use integer_io;

   line, old_line : string(1..250) := (others => ' ');
   last, old_last : integer := 0;
   n : integer := 0;

   input, output : file_type;

begin
   put_line("ONERS.IN -> ONERS.OUT");
   put_line("Takes a sorted file to produce a file having just one of each identical line.");
   put_line("Puts a count of how many identical lines at the begining of each.");

   open(input, in_file, "ONERS.IN");
   create(output, out_file, "ONERS.OUT");

   get_line(input, old_line, old_last);

   while not end_of_file(input)  loop
	  get_line(input, line, last);
	  n := n + 1;
	  if line(1..last) /= old_line(1..old_last)  then
		 put(output, n);
		 put_line(output, "  " & old_line(1..old_last));
		 n := 0;
		 old_last := last;
		 old_line(1..old_last) := line(1..last);
	  end if;
   end loop;

   close(output);
end oners;
