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
procedure patch is
   package integer_io is new ada.text_io.integer_io (integer);
   use ada.text_io;
   use integer_io;

   f1, f2, f3  : file_type;
   f           : string (1..100);
   blanks      : string (1..250) := (others => ' ');
   s, t        : string (1..250) := blanks;
   l           : integer := 0;
   n           : integer := 0;
   ls, lt      : integer := 0;
begin
   put_line ("Takes in two files and produces a third which is the pair");
   put_line ("as columns with N blanks between");
   put_line ("Does this while there are corresponding lines in both files");

   put ("What is first file to PATCH from =>");
   get_line (f, l);
   put ("=> ");
   open (f1, in_file, f (1..l));
   put_line ("Opened first input file");

   put ("What is second file to PATCH from =>");
   get_line (f, l);
   put ("=> ");
   open (f2, in_file, f (1..l));
   put_line ("Opened second input file");

   put ("How many blank columns to leave between =>");
   get (n);
   skip_line;
   new_line;

   put ("Where to put the resulting PATCHed file =>");
   get_line (f, l);
   put ("=> ");
   create (f3, out_file, f (1..l));
   put_line ("Created PATCHed output file");

   while (not end_of_file (f1) and not end_of_file (f2)) loop
	  get_line (f1, s, ls);
	  get_line (f2, t, lt);
	  put_line (f3, s (1..ls) & blanks (1..n) & t (1..lt));
   end loop;
   close (f1);
   close (f2);
   close (f3);
   put_line ("Finshed PATCH");

exception
   when others =>
	  put_line ("Unexpected exception in PATCH");
	  close (f3);
end patch;
