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

with Text_IO;
--with Strings_package; use Strings_package;
--with latin_file_names; use latin_file_names;
--with inflections_package; use inflections_package;
--with dictionary_package; use dictionary_package;
--with line_stuff; use line_stuff;
procedure Number is
   use Text_IO;

   Input : Text_IO.File_Type;
   Numbered : Text_IO.File_Type;

   Line : String (1 .. 300) := (others => ' ');
   Last, N : Integer := 0;

begin

   Put_Line (
     "Takes a text file and produces a NUMBERED. file with line numbers");

   Put_Line ("What file to NUMBER?");
   Text_IO.Get_Line (Line, Last);

   Open (Input, In_File, Line (1 .. Last));

   Create (Numbered, Out_File, "NUMBERED.");

   while not End_Of_File (Input) loop
      N := N + 1;

      Get_Line (Input, Line, Last);

      Text_IO.Put (Numbered, Integer'Image (N));
      Set_Col (Numbered, 10);
      Text_IO.Put_Line (Numbered, Line (1 .. Last));

   end loop;

   Close (Numbered);

end Number;
