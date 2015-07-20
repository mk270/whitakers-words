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
procedure number is
--   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;

   input : Text_IO.File_Type;
   numbered : Text_IO.File_Type;

   line : String (1 .. 300) := (others => ' ');
   last, n : Integer := 0;

begin

   Put_Line (
     "Takes a text file and produces a NUMBERED. file with line numbers");

   Put_Line ("What file to NUMBER?");
   Text_IO.Get_Line (line, last);

   Open (input, In_File, line (1 .. last));

   Create (numbered, Out_File, "NUMBERED.");

   while not End_Of_File (input) loop
      n := n + 1;

      Get_Line (input, line, last);

      Text_IO.Put (numbered, Integer'Image (n));
      Set_Col (numbered, 10);
      Text_IO.Put_Line (numbered, line (1 .. last));

   end loop;

   Close (numbered);

end number;
