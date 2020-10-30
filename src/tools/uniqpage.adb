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
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with line_stuff; use line_stuff;
-- with dictionary_form;
procedure Uniqpage is
   use Text_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   Uniques_File, Uniqpage : Text_IO.File_Type;

   S : constant String (1 .. 400) := (others => ' ');
   Line : String (1 .. 400) := (others => ' ');
   Blanks : constant String (1 .. 400) := (others => ' ');
   L, Last : Integer := 0;

   Stem : Stem_Type := Null_Stem_Type;
   Qual : Quality_Record;
   Kind : Kind_Entry;
   Tran : Translation_Record;
   Mean : Meaning_Type;

   procedure Get_Line_Unique
     (Input : in Text_IO.File_Type;
      S     : out String;
      Last  : out Natural)
   is
   begin
      Last := 0;
      Text_IO.Get_Line (Input, S, Last);
      -- FIXME: this if statement was commented out, because it triggered
      -- warning "if statement has no effect". I didn't delete it because quite
      -- possibly author wanted it to do something. Question is what?
      --if Trim (s (s'First .. last)) /= ""  then   --  Rejecting blank lines
      --   null;
      --end if;
   end Get_Line_Unique;

begin
   Put_Line ("UNIQUES.LAT -> UNIQPAGE.PG");
   Put_Line ("Takes UNIQUES form, single lines it, puts # at beginning,");
   Put_Line ("producing a .PG file for sorting to produce paper dictionary");
   Create (Uniqpage, Out_File, "UNIQPAGE.PG");
   Open (Uniques_File, In_File, "UNIQUES.LAT");

   Over_Lines :
   while not End_Of_File (Uniques_File)  loop
      Line := Blanks;
      Get_Line_Unique (Uniques_File, Line, Last);      --  STEM
      Stem := Head (Trim (Line (1 .. Last)), Max_Stem_Size);

      Line := Blanks;
      Get_Line_Unique (Uniques_File, Line, Last);    --  QUAL, KIND, TRAN
      Quality_Record_IO.Get (Line (1 .. Last), Qual, L);
      Get (Line (L + 1 .. Last), Qual.Pofs, Kind, L);
      Age_Type_IO.Get (Line (L + 1 .. Last), Tran.Age, L);
      Area_Type_IO.Get (Line (L + 1 .. Last), Tran.Area, L);
      Geo_Type_IO.Get (Line (L + 1 .. Last), Tran.Geo, L);
      Frequency_Type_IO.Get (Line (L + 1 .. Last), Tran.Freq, L);
      Source_Type_IO.Get (Line (L + 1 .. Last), Tran.Source, L);

      Line := Blanks;
      Get_Line_Unique (Uniques_File, Line, L);         --  MEAN
      Mean := Head (Trim (Line (1 .. L)), Max_Meaning_Size);

      --      while not END_OF_FILE (UNIQUES_FILE) loop
      --         S := BLANK_LINE;
      --         GET_LINE (INPUT, S, LAST);
      --         if TRIM (S (1 .. LAST)) /= ""  then   --  Rejecting blank lines
      --
      --

      Text_IO.Put (Uniqpage, "#" & Stem);

      Quality_Record_IO.Put (Uniqpage, Qual);

      -- PART := (V, (QUAL.V.CON, KIND.V_KIND));

      if (Qual.Pofs = V)  and then  (Kind.V_Kind in Gen .. Perfdef)  then
         Text_IO.Put (Uniqpage, "  " &
           Verb_Kind_Type'Image (Kind.V_Kind) & "  ");
      end if;

      Text_IO.Put (Uniqpage, " [");
      Age_Type_IO.Put (Uniqpage, Tran.Age);
      Area_Type_IO.Put (Uniqpage, Tran.Area);
      Geo_Type_IO.Put (Uniqpage, Tran.Geo);
      Frequency_Type_IO.Put (Uniqpage, Tran.Freq);
      Source_Type_IO.Put (Uniqpage, Tran.Source);
      Text_IO.Put (Uniqpage, "]");

      Put (Uniqpage, " :: ");
      Put_Line (Uniqpage, Mean);

      --end if;  --  Rejecting blank lines
   end loop Over_Lines;

   Close (Uniqpage);
exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (S (1 .. Last));
      Close (Uniqpage);

end Uniqpage;
