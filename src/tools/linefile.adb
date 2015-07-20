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
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
-- with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with line_stuff; use line_stuff;
procedure linefile is
--   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use Dictionary_Entry_IO;
   use Dict_IO;

   dictfile : Dict_IO.File_Type;
   output : Text_IO.File_Type;
   de : Dictionary_Entry;
   d_k : Dictionary_Kind := general;
   line : String (1 .. 40) := (others => ' ');
   last : Integer := 0;

begin
   Put_Line ("Takes a DICTFILE.D_K and produces a DICTLINE.D_K");
   Put ("What dictionary to convert, GENERAL or SPECIAL  (Reply G or S) =>");
   Get_Line (line, last);
   if last > 0  then
      if Trim (line (1 .. last))(1) = 'G'  or else
        Trim (line (1 .. last))(1) = 'g'
      then
         d_k := general;
      elsif Trim (line (1 .. last))(1) = 'S'  or else
        Trim (line (1 .. last))(1) = 's'
      then
         d_k := special;
      else
         Put_Line ("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

   Open (dictfile, In_File, add_file_name_extension (dict_file_name,
     Dictionary_Kind'Image (d_k)));

   Create (output, Out_File, add_file_name_extension ("DICT_NEW",
     Dictionary_Kind'Image (d_k)));

   while not End_Of_File (dictfile)  loop
      Read (dictfile, de);
      Put (output, de);
      Text_IO.New_Line (output);
   end loop;

end linefile;
