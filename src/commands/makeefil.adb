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

with Ada.Text_IO;
with Words_Engine.English_Support_Package;
use Words_Engine.English_Support_Package;

procedure Makeefil is
   use Ada.Text_IO;
   use Ewds_Direct_Io;
   Ewds_List : Ada.Text_IO.File_Type;
   Ewds, New_Ewds : Ewds_Record := Null_Ewds_Record;
begin
   Ada.Text_IO.Open (Ewds_List, Ada.Text_IO.In_File, "EWDSLIST.GEN");
   Create (Ewds_File, Out_File, "EWDSFILE.GEN");

   while not Ada.Text_IO.End_Of_File (Ewds_List)  loop
      Ewds_Record_Io.Get (Ewds_List, New_Ewds);
      Ada.Text_IO.Skip_Line (Ewds_List);

      --  Eliminate doubles    --  If sort is OK
      if Ewds.W = New_Ewds.W  and  --  AUX ????
        Ewds.N = New_Ewds.N
      then
         -- PUT_LINE ("DOUBLES   ");
         -- EWDS_RECORD_IO.PUT (EWDS); NEW_LINE;
         -- EWDS_RECORD_IO.PUT (NEW_EWDS); NEW_LINE;

         if Ewds.Kind > New_Ewds.Kind  then  --  Large KIND = high priority
            null;
         elsif Ewds.Kind < New_Ewds.Kind  then
            Ewds := New_Ewds;
         elsif Ewds.Kind = New_Ewds.Kind  then
            if Ewds.Semi > New_Ewds.Semi  then
               Ewds := New_Ewds;
            end if;
         end if;

      else

         Write (Ewds_File, Ewds);
         Ewds := New_Ewds;
         Number_Of_Ewords := Number_Of_Ewords + 1;
      end if;
      --PUT ('.');
   end loop;
   Close (Ewds_File);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     ("NUMBER_OF_EWORDS = " & Integer'Image (Number_Of_Ewords));
exception
   when others =>
      Close (Ewds_File);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("MAKEEFIL terminated on an exception");
      Ada.Text_IO.Put_Line
        ("NUMBER_OF_EWORDS = " & Integer'Image (Number_Of_Ewords));
end Makeefil;
