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
procedure Invstems is
   Line : String (1 .. 250);
   Ll : Integer;
   subtype Stem is String (1 .. 18);
   Blank_Stem : constant Stem := (others => ' ');
   Sts : array (1 .. 4) of Stem;

   Input, Output : File_Type;

   function Invert (S : String) return String is
      T : String (S'First .. S'Last);
   begin
      if S (S'First) = ' '  then
         return Blank_Stem;
      else
         for I in S'Range  loop
            T (I) := S (S'Last - I + 1);
         end loop;
         return Head (Trim (T), 18);
      end if;
   end Invert;

begin
   Put_Line ("Inverts the 4 stems of a DICTLINE form file " &
     "INVERT_S.IN -> INVERT_S.OUT");

   Create (Output, Out_File, "INVERT_S.OUT");
   Open (Input, In_File, "INVERT_S.IN");

   while not End_Of_File (Input)  loop
      Get_Line (Input, Line, Ll);
      Sts (1) := Line (1 .. 18);
      Sts (2) := Line (20 .. 37);
      Sts (3) := Line (39 .. 56);
      Sts (4) := Line (58 .. 75);
      for I in 1 .. 4  loop
         Sts (I) := Invert (Sts (I));
      end loop;
      Line (1 .. 18)  := Sts (1);
      Line (20 .. 37) := Sts (2);
      Line (39 .. 56) := Sts (3);
      Line (58 .. 75) := Sts (4);
      Put_Line (Output, Line (1 .. Ll));

   end loop;

   Close (Output);

end Invstems;
