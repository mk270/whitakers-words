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
with Strings_package; use Strings_package;
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
procedure linefile is
   package Integer_IO is new text_io.Integer_IO (Integer);
   use text_io;
   use Dictionary_Entry_IO;
   use dict_io;

   dictfile : dict_io.File_Type;
   output : text_io.File_Type;
   de : Dictionary_Entry;
   d_k : dictionary_kind := general;
   line : String (1 .. 40) := (others => ' ');
   last : Integer := 0;

begin
   Put_Line ("Takes a DICTFILE.D_K and produces a DICTLINE.D_K");
   put ("What dictionary to convert, GENERAL or SPECIAL  (Reply G or S) =>");
   Get_Line (line, last);
   if last > 0  then
      if Trim (line (1 .. last))(1) = 'G'  or else
        Trim (line (1 .. last))(1) = 'g'     then
         d_k := general;
      elsif Trim (line (1 .. last))(1) = 'S'  or else
        Trim (line (1 .. last))(1) = 's'     then
         d_k := special;
      else
         Put_Line ("No such dictionary");
         raise text_io.data_error;
      end if;
   end if;

   Open (dictfile, In_File, add_file_name_extension (dict_file_name,
     dictionary_kind'image (d_k)));

   Create (output, Out_File, add_file_name_extension ("DICT_NEW",
     dictionary_kind'image (d_k)));

   while not End_Of_File (dictfile)  loop
      read (dictfile, de);
      put (output, de);
      text_io.New_Line (output);
   end loop;

end linefile;
