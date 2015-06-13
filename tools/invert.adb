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
procedure invert is
   line, parm : string(1..250);
   l, last : integer;
   n1, n2 : integer;

   input, output : file_type;
   package integer_io is new text_io.integer_io(integer);

   function invert(s : string) return string is
	  t : string(1..s'length);
   begin
	  for i in 1..t'length  loop
		 t(i) := s(s'last-i+1);
	  end loop;
	  return head(trim(t), s'length);

   end invert;

begin
   put_line("Inverts/reverses the order of columns N1..N2 of INVERT.IN -> INVERT.OUT");
   put("Give an N1 and N2 => ");
   get_line(parm, last);

   integer_io.get(parm(1..last), n1, l);
   integer_io.get(parm(l+1..last), n2, l);

   create(output, out_file, "INVERT.OUT");
   open(input, in_file, "INVERT.IN");

   while not end_of_file(input)  loop
	  get_line(input, line, last);

	  line(n1..n2)  := invert(line(n1..n2));
	  put('.');
	  put_line(output, line(1..last));

   end loop;

   close(output);

exception
   when others  =>
	  close(output);

end invert;
