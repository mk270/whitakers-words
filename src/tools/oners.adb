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
