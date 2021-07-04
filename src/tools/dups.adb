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

with Ada.Integer_Text_IO;
with Text_IO;
--with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
procedure Dups is
   use Ada.Integer_Text_IO;
   use Text_IO;

   Input, Output : File_Type;
   S, Blank_Line : constant String (1 .. 400) := (others => ' ');
   Line, Oldline : String (1 .. 400) := (others => ' ');
   Last : Integer := 0;
   Mx, Nx : Natural := 0;

   Line_Number : Integer := 0;
   Number : Integer := 0;

   procedure Get_Entry (Mx, Nx  : out Natural) is
      Ls : Integer := 0;
      Enter_Line : String (1 .. 20);

   begin

      Get_Line (Enter_Line, Ls);
      Get (Enter_Line (1 .. Ls), Mx, Last);
      Get (Enter_Line (Last + 1 .. Ls), Nx, Last);

   end Get_Entry;

begin
   Put_Line ("DUPS.IN -> DUPS.OUT    For sorted files");
   Put_Line ("DUPS  checks for columns MX .. NX being duplicates");
   Get_Entry (Mx, Nx);

   Create (Output, Out_File, "DUPS.OUT");
   Open (Input, In_File, "DUPS.IN");

   while not End_Of_File (Input) loop
      Oldline := Line;
      Line := Blank_Line;
      Get_Line (Input, Line, Last);
      Line_Number := Line_Number + 1;
      if Line (Mx .. Nx) = Oldline (Mx .. Nx)  and then
        (Line (111) /= '|')
      then
         Number := Number + 1;
         Put (Output, Line_Number); Put (Output, "  ");
         Put_Line (Output, Line (1 .. Nx));
      end if;
   end loop;

   Close (Output);

   New_Line;
   Put ("Number of entries = "); Put (Line_Number); New_Line;
   Put ("Number of DUPS    = "); Put (Number); New_Line;
   Put ("Ratio             = 1 :"); Put (Line_Number / Number); New_Line;

exception
   when Name_Error  =>
      Put_Line ("No file to process");
      Close (Output);

   when others =>
      Put ("Exception on LINE"); Put (Line_Number); New_Line;
      Put_Line (S (1 .. Last));
      Close (Output);

end Dups;
