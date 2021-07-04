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
procedure Fil2dict is
--   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use Dictionary_Entry_IO;
   use Dict_IO;

   D_K : Dictionary_Kind := Xxx;
   De : Dictionary_Entry := Null_Dictionary_Entry;

   Line : String (1 .. 200) := (others => ' ');
   Last : Integer := 0;

   Dictfile : Dict_IO.File_Type;
   Dictline : Text_IO.File_Type;

begin
   Put_Line (
     "Takes a DICTFILE.D_K and reconstructs the DICTLINE.D_K it came from");

   Put ("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
   Text_IO.Get_Line (Line, Last);
   if Last > 0  then
      if Trim (Line (1 .. Last))(1) = 'G'  or else
        Trim (Line (1 .. Last))(1) = 'g'
      then
         D_K := General;
      elsif Trim (Line (1 .. Last))(1) = 'S'  or else
        Trim (Line (1 .. Last))(1) = 's'
      then
         D_K := Special;
      else
         Put_Line ("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

   Dict_IO.Open (Dictfile, In_File, Dict_File_Name & '.' & Ext (D_K));

   Create (Dictline, Out_File, Dict_Line_Name & ".NEW");
   --DICTIONARY_KIND'IMAGE (D_K)));

   while not End_Of_File (Dictfile)  loop
      Read (Dictfile, De);
      Put (Dictline, De);
   end loop;

end Fil2dict;
