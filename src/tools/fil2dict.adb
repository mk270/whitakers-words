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
procedure fil2dict is
   package Integer_IO is new text_io.Integer_IO (Integer);
   use text_io;
   use stem_key_type_io;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use geo_type_io;
   use frequency_type_io;
   use source_type_io;
   use dict_io;

   d_k : dictionary_kind := xxx;
   de: Dictionary_Entry := null_Dictionary_Entry;

   line : String (1 .. 200) := (others => ' ');
   last : Integer := 0;

   dictfile : dict_io.File_Type;
   dictline : text_io.File_Type;

begin
   Put_Line (
     "Takes a DICTFILE.D_K and reconstructs the DICTLINE.D_K it came from");

   put ("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
   text_io.Get_Line (line, last);
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

   dict_io.Open (dictfile, In_File, add_file_name_extension (dict_file_name,
     dictionary_kind'image (d_k)));

   Create (dictline, Out_File, add_file_name_extension (dict_line_name,
     "NEW"));
   --DICTIONARY_KIND'IMAGE (D_K)));

   while not End_Of_File (dictfile)  loop
      read (dictfile, de);
      put (dictline, de);
   end loop;

end fil2dict;
