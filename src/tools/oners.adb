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

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Text_IO; use Text_IO;
procedure Oners is

   Line, Old_Line : String (1 .. 250) := (others => ' ');
   Last, Old_Last : Integer := 0;
   N : Integer := 0;

   Input, Output : File_Type;

begin
   Put_Line ("ONERS.IN -> ONERS.OUT");
   Put_Line ("Takes a sorted file to produce a file having just" &
     " one of each identical line.");
   Put_Line ("Puts a count of how many identical lines at the" &
     " beginning of each.");

   Open (Input, In_File, "ONERS.IN");
   Create (Output, Out_File, "ONERS.OUT");

   Get_Line (Input, Old_Line, Old_Last);

   while not End_Of_File (Input)  loop
      Get_Line (Input, Line, Last);
      N := N + 1;
      if Line (1 .. Last) /= Old_Line (1 .. Old_Last)  then
         Put (Output, N);
         Put_Line (Output, "  " & Old_Line (1 .. Old_Last));
         N := 0;
         Old_Last := Last;
         Old_Line (1 .. Old_Last) := Line (1 .. Last);
      end if;
   end loop;

   Close (Output);
end Oners;
