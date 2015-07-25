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
-- with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
-- with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
-- with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with line_stuff; use line_stuff;
procedure Fixord is
   use Text_IO;

   Input, Output : Text_IO.File_Type;

   Blank_Line : constant String (1 .. 400) := (others => ' ');
   S : String (1 .. 400) := (others => ' ');
   Last : Integer := 0;

begin
   Put_Line ("FIXORD.IN -> FIXORD.OUT");
   Put_Line ("Makes a clean (no #) 3 line ED format from LISTORD output");

   Create (Output, Out_File, "FIXORD.OUT");
   Open (Input, In_File, "FIXORD.IN");

   Over_Lines :
   while not End_Of_File (Input) loop
      S := Blank_Line;
      Get_Line (Input, S, Last);
      if Trim (S (1 .. Last)) /= ""  then   --  Rejecting blank lines

         if S (1) /= '#'  then
            Put_Line (Output, S (1 .. Last));
         end if;

      end if;  --  Rejecting blank lines
   end loop Over_Lines;

   Close (Output);
exception
   when Text_IO.Data_Error  =>
      Close (Output);

end Fixord;
