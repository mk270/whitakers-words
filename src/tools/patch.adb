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
   package Integer_IO is new ada.text_io.Integer_IO (Integer);
   use ada.text_io;
   use Integer_IO;

   f1, f2, f3  : File_Type;
   f           : String (1 .. 100);
   blanks      : String (1 .. 250) := (others => ' ');
   s, t        : String (1 .. 250) := blanks;
   l           : Integer := 0;
   n           : Integer := 0;
   ls, lt      : Integer := 0;
begin
   Put_Line ("Takes in two files and produces a third which is the pair");
   Put_Line ("as columns with N blanks between");
   Put_Line ("Does this while there are corresponding lines in both files");

   put ("What is first file to PATCH from =>");
   Get_Line (f, l);
   put ("=> ");
   Open (f1, In_File, f (1 .. l));
   Put_Line ("Opened first input file");

   put ("What is second file to PATCH from =>");
   Get_Line (f, l);
   put ("=> ");
   Open (f2, In_File, f (1 .. l));
   Put_Line ("Opened second input file");

   put ("How many blank columns to leave between =>");
   Get (n);
   skip_line;
   New_Line;

   put ("Where to put the resulting PATCHed file =>");
   Get_Line (f, l);
   put ("=> ");
   Create (f3, Out_File, f (1 .. l));
   Put_Line ("Created PATCHed output file");

   while (not End_Of_File (f1) and not End_Of_File (f2)) loop
      Get_Line (f1, s, ls);
      Get_Line (f2, t, lt);
      Put_Line (f3, s (1 .. ls) & blanks (1 .. n) & t (1 .. lt));
   end loop;
   Close (f1);
   Close (f2);
   Close (f3);
   Put_Line ("Finshed PATCH");

exception
   when others =>
      Put_Line ("Unexpected exception in PATCH");
      Close (f3);
end patch;
