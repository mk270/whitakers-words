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
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;

procedure Linedict is
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   De : Dictionary_Entry;

   Dictionary_File : File_Type;
   Output : File_Type;

   St_Line, Pt_Line, Mn_Line : String (1 .. 300) :=  (others => ' ');
   Blank_Line : constant String (1 .. 300) := (others => ' ');
   L, Ll, Last : Integer := 0;
   Number_Of_Dictionary_Entries : Integer := 0;

   procedure Get_Stem (S : in String;
                       Stem : out Stem_Type; Last : out Integer) is
      I  : Integer := 1;
      L  : Integer := S'First;
   begin
      Stem := Null_Stem_Type;
      --  Squeeze left
      while L <= S'Last and then S (L) = ' '  loop
         L := L + 1;
      end loop;
      --  Count until the first blank
      --  Return that String
      while L <= S'Last and then S (L) /= ' '  loop
         Stem (I) := S (L);
         I := I + 1;
         L := L + 1;
      end loop;
      --  Return  last
      Last := L;
   end Get_Stem;

begin
   Put_Line ("LINEDICT.IN (EDIT format - 3 lines)" &
     " -> LINEDICT.OUT (DICTLINE format)");

   Create (Output, Out_File, "LINEDICT.OUT");

   Open (Dictionary_File, In_File, "LINEDICT.IN");
   Put ("Dictionary loading");

   while not End_Of_File (Dictionary_File)  loop
      St_Line := Blank_Line;
      Pt_Line := Blank_Line;
      Mn_Line := Blank_Line;
      Error_Check :
      begin

         Get_Non_Comment_Line (Dictionary_File, St_Line, Last); --  STEMS

         -- really? -- when is "line" supposed to be read?
         -- line := blank_line;
         Get_Non_Comment_Line (Dictionary_File, Pt_Line, L);    --  PART
         Get (Pt_Line (1 .. L), De.Part, Ll);
         --            GET (PT_LINE (LL+1 .. L), DE.PART.POFS, DE.KIND, LL);

         Get (Pt_Line (Ll + 1 .. L), De.Tran.Age, Ll);
         Get (Pt_Line (Ll + 1 .. L), De.Tran.Area, Ll);
         Get (Pt_Line (Ll + 1 .. L), De.Tran.Geo, Ll);
         Get (Pt_Line (Ll + 1 .. L), De.Tran.Freq, Ll);
         Get (Pt_Line (Ll + 1 .. L), De.Tran.Source, Ll);

         De.Stems := Null_Stems_Type;
         Ll := 1;
         --  Extract up to 4 Stems

         for I in 1 .. Number_Of_Stems (De.Part.Pofs)
         loop   --  EXTRACT STEMS
            Get_Stem (St_Line (Ll .. Last), De.Stems (I), Ll);
         end loop;

         -- line := blank_line;
         Get_Non_Comment_Line (Dictionary_File, Mn_Line, L);   --  MEANING

         De.Mean := Head (Trim (Mn_Line (1 .. L)), Max_Meaning_Size);

         Put (Output, De); New_Line (Output);

         Number_Of_Dictionary_Entries := Number_Of_Dictionary_Entries + 1;

      exception
         when others  =>
            Put_Line
              ("-------------------------------------------------------------");
            Put_Line (Head (St_Line, 78));
            Put_Line (Head (Pt_Line, 78));
            Put_Line (Head (Mn_Line, 78));
      end Error_Check;

   end loop;
   Close (Dictionary_File);
   Close (Output);
   Set_Col (33); Put ("--  ");
   Ada.Integer_Text_IO.Put (Number_Of_Dictionary_Entries);
   Put (" entries");    Set_Col (55); Put_Line ("--  Loaded correctly");
end Linedict;
