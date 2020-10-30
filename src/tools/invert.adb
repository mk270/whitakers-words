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
with Text_IO; use Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
procedure Invert is
   Line, Parm : String (1 .. 250);
   L, Last : Integer;
   N1, N2 : Integer;

   Input, Output : File_Type;

   function Invert (S : String) return String is
      T : String (1 .. S'Length);
   begin
      for I in 1 .. T'Length  loop
         T (I) := S (S'Last - I + 1);
      end loop;
      return Head (Trim (T), S'Length);

   end Invert;

begin
   Put_Line ("Inverts/reverses the order of columns N1" &
     " .. N2 of INVERT.IN -> INVERT.OUT");
   Put ("Give an N1 and N2 => ");
   Get_Line (Parm, Last);

   Ada.Integer_Text_IO.Get (Parm (1 .. Last), N1, L);
   Ada.Integer_Text_IO.Get (Parm (L + 1 .. Last), N2, L);

   Create (Output, Out_File, "INVERT.OUT");
   Open (Input, In_File, "INVERT.IN");

   while not End_Of_File (Input)  loop
      Get_Line (Input, Line, Last);

      Line (N1 .. N2)  := Invert (Line (N1 .. N2));
      Put ('.');
      Put_Line (Output, Line (1 .. Last));

   end loop;

   Close (Output);

exception
   when others  =>
      Close (Output);

end Invert;
