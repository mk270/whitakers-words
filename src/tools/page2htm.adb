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

with Text_IO; use Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
procedure Page2htm is
   Line : String (1 .. 300) := (others => ' ');
   Last : Integer := 0;

   Input, Output : File_Type;

begin
   Put_Line ("DICTPAGE.RAW (sorted) -> DICTPAGE.HTM");
   Put_Line ("For use in preparing a DICTPAGE.HTM after" &
     " running DICTPAGE and sorting.");

   Open (Input, In_File, "DICTPAGE.RAW");
   Create (Output, Out_File, "DICTPAGE.HTM");

   while not End_Of_File (Input)  loop
      Get_Line (Input, Line, Last);
      if Line (1) /= '#'  then
         Put_Line ("BAD LINE   >" & Line (1 .. Last));
      end if;
      for I in 1 .. Last  loop
         if Line (I) = '['  then
            Put (Output, "<B>" & Line (2 .. I - 1) & "</B>  ");
            Put_Line (Output, Trim (Line (I .. I + 6) & "<BR>"));
         end if;
         if Line (I .. I + 1) = "::"  then
            Put_Line (Output, Trim (Line (I + 2 .. Last)) & "<BR>");
            exit;
         end if;
      end loop;  --  On LINE

   end loop;  --  On file

end Page2htm;
