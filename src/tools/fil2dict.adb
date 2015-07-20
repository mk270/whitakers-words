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
--with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
procedure fil2dict is
--   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
--   use Stem_Key_Type_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
--   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
--   use Frequency_Type_IO;
   use Source_Type_IO;
   use Dict_IO;

   d_k : Dictionary_Kind := xxx;
   De : Dictionary_Entry := null_Dictionary_Entry;

   line : String (1 .. 200) := (others => ' ');
   last : Integer := 0;

   dictfile : Dict_IO.File_Type;
   dictline : Text_IO.File_Type;

begin
   Put_Line (
     "Takes a DICTFILE.D_K and reconstructs the DICTLINE.D_K it came from");

   put ("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
   Text_IO.Get_Line (line, last);
   if last > 0  then
      if Trim (line (1 .. last))(1) = 'G'  or else
        Trim (line (1 .. last))(1) = 'g'     then
         d_k := general;
      elsif Trim (line (1 .. last))(1) = 'S'  or else
        Trim (line (1 .. last))(1) = 's'     then
         d_k := special;
      else
         Put_Line ("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

   Dict_IO.Open (dictfile, In_File, add_file_name_extension (dict_file_name,
     Dictionary_Kind'Image (d_k)));

   Create (dictline, Out_File, add_file_name_extension (dict_line_name,
     "NEW"));
   --DICTIONARY_KIND'IMAGE (D_K)));

   while not End_Of_File (dictfile)  loop
      read (dictfile, de);
      put (dictline, de);
   end loop;

end fil2dict;
