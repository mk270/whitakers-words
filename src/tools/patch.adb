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
with Ada.Text_IO;
procedure Patch is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;

   F1, F2, F3  : File_Type;
   F           : String (1 .. 100);
   Blanks      : constant String (1 .. 250) := (others => ' ');
   S, T        : String (1 .. 250) := Blanks;
   L           : Integer := 0;
   N           : Integer := 0;
   Ls, Lt      : Integer := 0;
begin
   Put_Line ("Takes in two files and produces a third which is the pair");
   Put_Line ("as columns with N blanks between");
   Put_Line ("Does this while there are corresponding lines in both files");

   Put ("What is first file to PATCH from =>");
   Get_Line (F, L);
   Put ("=> ");
   Open (F1, In_File, F (1 .. L));
   Put_Line ("Opened first input file");

   Put ("What is second file to PATCH from =>");
   Get_Line (F, L);
   Put ("=> ");
   Open (F2, In_File, F (1 .. L));
   Put_Line ("Opened second input file");

   Put ("How many blank columns to leave between =>");
   Get (N);
   Skip_Line;
   New_Line;

   Put ("Where to put the resulting PATCHed file =>");
   Get_Line (F, L);
   Put ("=> ");
   Create (F3, Out_File, F (1 .. L));
   Put_Line ("Created PATCHed output file");

   while not End_Of_File (F1) and not End_Of_File (F2) loop
      Get_Line (F1, S, Ls);
      Get_Line (F2, T, Lt);
      Put_Line (F3, S (1 .. Ls) & Blanks (1 .. N) & T (1 .. Lt));
   end loop;
   Close (F1);
   Close (F2);
   Close (F3);
   Put_Line ("Finshed PATCH");

exception
   when others =>
      Put_Line ("Unexpected exception in PATCH");
      Close (F3);
end Patch;
