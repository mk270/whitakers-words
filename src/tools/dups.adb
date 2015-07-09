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
procedure dups is
   package integer_text_io is new text_io.integer_io(integer);
   use integer_text_io;
   use text_io;

   input, output : file_type;
   s, line, oldline, blank_line : string(1..400) := (others => ' ');
   j, l, ll, last : integer := 0;
   mx, nx : natural := 0;

   line_number : integer := 0;
   number : integer := 0;

   procedure get_entry (mx, nx  : out natural) is
	  z : natural := 0;
	  ls : integer := 0;
	  enter_line : string(1..20);

	  procedure prompt_for_entry(entry_number : string) is
	  begin
		 put("Give starting and ending column ");
	  end prompt_for_entry;

   begin

	  get_line(enter_line, ls);
	  get(enter_line(1..ls), mx, last);
	  get(enter_line(last+1..ls), nx, last);

   end get_entry;

begin
   put_line("DUPS.IN -> DUPS.OUT    For sorted files");
   put_line("DUPS  checks for columns MX..NX being duplicates");
   get_entry(mx, nx);

   create(output, out_file, "DUPS.OUT");
   open(input, in_file, "DUPS.IN");

   while not end_of_file(input) loop
	  oldline := line;
	  line := blank_line;
	  get_line(input, line, last);
	  line_number := line_number + 1;
	  if line(mx..nx) = oldline(mx..nx)  and then
		(line(111) /= '|') then
		 number := number + 1 ;
		 put(output, line_number); put(output, "  ");
		 put_line(output, line(1..nx));
	  end if;
   end loop;

   close(output);

   new_line;
   put("Number of entries = "); put(line_number); new_line;
   put("Number of DUPS    = "); put(number); new_line;
   put("Ratio             = 1 :"); put(line_number/number); new_line;

exception
   when name_error  =>
	  put_line("No file to process");
	  close(output);

   when others =>
	  put("Exception on LINE"); put(line_number); new_line;
	  put_line(s(1..last));
	  close(output);

end dups;
