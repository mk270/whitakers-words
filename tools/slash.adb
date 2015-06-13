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

with ada.text_io;
procedure slash is
   package integer_io is new ada.text_io.integer_io (integer);
   use ada.text_io;
   use integer_io;

   f1, f2, f3  : file_type;
   f           : string (1..100);
   s           : string (1..2500);
   bs          : string (1..2500) := (others => ' ');
   n           : integer := 0;
   l           : integer := 0;
   ls          : integer := 0;

   type reply_type is (columns, lines);
   reply : reply_type;
   reply_character : character;

   function which(r : character) return reply_type is
   begin
	  case r is
		 when 'C' | 'c'  =>  return columns;
		 when 'L' | 'l'  =>  return lines;
		 when others     =>
			raise data_error;
	  end case;
   end which;

begin
   put_line("Breaks a file into two, by row or column.");

   put ("What file to SLASH from =>");
   get_line (f, l);
   put ("=> ");
   open (f1, in_file, f (1..l));
   put_line ("Opened input file");

   put ("Do you wish to SLASH C)olumns or L)ines? =>");
   get (reply_character);
   skip_line;
   reply := which(reply_character);
   new_line;

   put ("How many lines/columns to leave after SLASHing =>");
   get (n);
   skip_line;
   new_line;

   put ("Where to put the first  =>");
   get_line (f, l);
   put ("=> ");
   create (f2, out_file, f (1..l));
   put_line ("Created SLASH file first");

   put ("Where to put the rest  =>");
   get_line (f, l);
   put ("=> ");
   create (f3, out_file, f (1..l));
   put_line ("Created SLASH file rest");

   if reply = columns  then

	  while not end_of_file (f1) loop
		 s := bs;
		 get_line (f1, s, ls);
		 if ls <= n then            --  Line shorter than break
			put_line (f2, s (1..ls));
			put_line (f3, "");       --  Put a blank line so there will be a line
		 else                       --  Line runs past break
			put_line (f2, s (1..n));
			put_line (f3, s (n + 1..ls));
		 end if;
	  end loop;
	  close (f2);
	  close (f3);

   elsif reply = lines  then

  first:
	  begin
		 for i in 1..n loop
			get_line (f1, s, ls);
			put_line (f2, s (1..ls));
		 end loop;
	  exception
		 when end_error  =>
			null;
	  end first;
	  close (f2);

  second:
	  begin
		 loop
			get_line (f1, s, ls);
			put_line (f3, s (1..ls));
		 end loop;
	  exception
		 when end_error  =>
			null;
	  end second;
	  close (f3);

   end if;

   put_line ("Done SLASHing");

exception
   when data_error  =>
	  put_line("***************** WRONG REPLY *****************");
	  new_line(2);
	  put_line("Try again");
   when others      =>
	  new_line(2);
	  put_line("Unexpected exception raised in SLASH  *********");
end slash;
