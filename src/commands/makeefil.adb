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
with english_support_package; use english_support_package;
procedure makeefil is
   use Ada.Text_IO;
   use ewds_direct_io;
   ewds_list : Ada.Text_IO.File_Type;
   ewds, new_ewds : ewds_record := null_ewds_record;
begin
   Ada.Text_IO.Open (ewds_list, Ada.Text_IO.In_File, "EWDSLIST.GEN");
   Create (ewds_file, Out_File, "EWDSFILE.GEN");

   while not Ada.Text_IO.End_Of_File (ewds_list)  loop
      ewds_record_io.Get (ewds_list, new_ewds);
      Ada.Text_IO.Skip_Line (ewds_list);

      --  Eliminate doubles    --  If sort is OK
      if ewds.w = new_ewds.w  and  --  AUX ????
        ewds.n = new_ewds.n
      then
         -- PUT_LINE ("DOUBLES   ");
         -- EWDS_RECORD_IO.PUT (EWDS); NEW_LINE;
         -- EWDS_RECORD_IO.PUT (NEW_EWDS); NEW_LINE;

         if ewds.kind > new_ewds.kind  then  --  Large KIND = high priority
            null;
         elsif ewds.kind < new_ewds.kind  then
            ewds := new_ewds;
         elsif ewds.kind = new_ewds.kind  then
            if ewds.semi > new_ewds.semi  then
               ewds := new_ewds;
            end if;
         end if;

      else

         Write (ewds_file, ewds);
         ewds := new_ewds;
         number_of_ewords := number_of_ewords + 1;
      end if;
      --PUT ('.');
   end loop;
   Close (ewds_file);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     ("NUMBER_OF_EWORDS = " & Integer'Image (number_of_ewords));
exception
   when others =>
      Close (ewds_file);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("MAKEEFIL terminated on an exception");
      Ada.Text_IO.Put_Line
        ("NUMBER_OF_EWORDS = " & Integer'Image (number_of_ewords));
end makeefil;
