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
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
procedure Dictflag is
   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;
   use Dict_IO;

   --be_ve : Verb_Entry := (Con => (5, 1), Kind => To_Be);

   D_K : Dictionary_Kind := Xxx;       --  ######################

   Start_Stem_1  : constant := 1;
   Start_Stem_2  : constant := Start_Stem_1 + Max_Stem_Size + 1;
   Start_Stem_3  : constant := Start_Stem_2 + Max_Stem_Size + 1;
   Start_Stem_4  : constant := Start_Stem_3 + Max_Stem_Size + 1;
   Start_Part    : constant := Start_Stem_4 + Max_Stem_Size + 1;
   --start_tran    : constant Integer :=
   --  start_part +
   --  Integer (Part_Entry_IO.Default_Width + 1);
   --finish_line   : constant Integer :=
   --  start_tran +
   --  Translation_Record_IO.Default_Width - 1;

   Age_Array : array (Age_Type'Range) of Integer := (others => 0);

   Area_Array : array (Area_Type'Range) of Integer := (others => 0);

   Geo_Array : array (Geo_Type'Range) of Integer := (others => 0);

   Freq_Array : array (Frequency_Type'Range) of Integer := (others => 0);

   Source_Array : array (Source_Type'Range) of Integer := (others => 0);

   -- dictfile : Dict_IO.File_Type;
   Input, Output : Text_IO.File_Type;
   De : Dictionary_Entry;

   S, Line : String (1 .. 400) := (others => ' ');
   Blank_Line : constant String (1 .. 400) := (others => ' ');
   L, Last : Integer := 0;
   J : Dict_IO.Count := 0;

   --mean_to_be : constant Meaning_Type :=
   --  Head ("to be, exist; also used to form verb perfect passive tenses" &
   --  " with NOM PERF PPL", Max_Meaning_Size);

   -- mean_to_be unreferenced - perhaps it was not meant to be...

begin
   Put_Line (
     "Takes a DICTLINE.D_K and produces a numeration of FLAGS");
   Put ("What dictionary to list, GENERAL or SPECIAL  =>");
   Get_Line (Line, Last);
   if Last > 0  then
      if Trim (Line (1 .. Last))(1) = 'G'  or else
        Trim (Line (1 .. Last))(1) = 'g'
      then
         D_K := General;
      elsif Trim (Line (1 .. Last))(1) = 'S'  or else
        Trim (Line (1 .. Last))(1) = 's'
      then
         D_K := Special;
      else
         Put_Line ("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

   Open (Input, In_File, Dict_Line_Name & '.' & Ext (D_K));

   Create (Output, Out_File, "FLAGS." & Ext (D_K));

   while not End_Of_File (Input) loop
      S := Blank_Line;
      Get_Line (Input, S, Last);
      if Trim (S (1 .. Last)) /= ""  then
         L := 0;

         Form_De :
         begin

            De.Stems (1) := S (Start_Stem_1 .. Max_Stem_Size);
            --NEW_LINE; PUT (DE.STEMS (1));
            De.Stems (2) :=
              S (Start_Stem_2 .. Start_Stem_2 + Max_Stem_Size - 1);
            De.Stems (3) :=
              S (Start_Stem_3 .. Start_Stem_3 + Max_Stem_Size - 1);
            De.Stems (4) :=
              S (Start_Stem_4 .. Start_Stem_4 + Max_Stem_Size - 1);

            --PUT ('#'); PUT (INTEGER'IMAGE (L)); PUT (INTEGER'IMAGE (LAST));
            --PUT ('@');
            Get (S (Start_Part .. Last), De.Part, L);
            --PUT ('%'); PUT (INTEGER'IMAGE (L)); PUT (INTEGER'IMAGE (LAST));
            --PUT ('&'); PUT (S (L+1 .. LAST)); PUT ('3');
            --   GET (S (L+1 .. LAST), DE.PART.POFS, DE.KIND, L);

            Get (S (L + 1 .. Last), De.Tran.Age, L);
            Age_Array (De.Tran.Age) := Age_Array (De.Tran.Age) + 1;
            Get (S (L + 1 .. Last), De.Tran.Area, L);
            Area_Array (De.Tran.Area) := Area_Array (De.Tran.Area) + 1;
            Get (S (L + 1 .. Last), De.Tran.Geo, L);
            Geo_Array (De.Tran.Geo) := Geo_Array (De.Tran.Geo) + 1;
            Get (S (L + 1 .. Last), De.Tran.Freq, L);
            Freq_Array (De.Tran.Freq) := Freq_Array (De.Tran.Freq) + 1;
            Get (S (L + 1 .. Last), De.Tran.Source, L);
            Source_Array (De.Tran.Source) := Source_Array (De.Tran.Source) + 1;

            De.Mean := Head (S (L + 2 .. Last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING,
            --    not ENUM

         exception
            when others =>
               New_Line;
               Put_Line ("Exception");
               Put_Line (S (1 .. Last));
               Integer_IO.Put (Integer (J)); New_Line;
               Put (De); New_Line;
         end Form_De;

         J := J + 1;

      end if;
   end loop;

   Text_IO.Put (Output, "Number of lines in DICTLINE "  & Ext (D_K) & "  ");
   Integer_IO.Put (Output, Integer (J));
   Text_IO.New_Line (Output);

   Text_IO.New_Line (Output, 4);
   Text_IO.Put_Line (Output, "AGE");
   for I in Age_Type'Range  loop
      Text_IO.Put (Output, Age_Type'Image (I));
      Text_IO.Set_Col (Output, 10);
      Text_IO.Put_Line (Output, Integer'Image (Age_Array (I)));
   end loop;

   Text_IO.New_Line (Output, 4);
   Text_IO.Put_Line (Output, "AREA");
   for I in Area_Type'Range  loop
      Text_IO.Put (Output, Area_Type'Image (I));
      Text_IO.Set_Col (Output, 10);
      Text_IO.Put_Line (Output, Integer'Image (Area_Array (I)));
   end loop;

   Text_IO.New_Line (Output, 4);
   Text_IO.Put_Line (Output, "GEO");
   for I in Geo_Type'Range  loop
      Text_IO.Put (Output, Geo_Type'Image (I));
      Text_IO.Set_Col (Output, 10);
      Text_IO.Put_Line (Output, Integer'Image (Geo_Array (I)));
   end loop;

   Text_IO.New_Line (Output, 4);
   Text_IO.Put_Line (Output, "FREQ");
   for I in Frequency_Type'Range  loop
      Text_IO.Put (Output, Frequency_Type'Image (I));
      Text_IO.Set_Col (Output, 10);
      Text_IO.Put_Line (Output, Integer'Image (Freq_Array (I)));
   end loop;

   Text_IO.New_Line (Output, 4);
   Text_IO.Put_Line (Output, "SOURCE");
   for I in Source_Type'Range  loop
      Text_IO.Put (Output, Source_Type'Image (I));
      Text_IO.Set_Col (Output, 10);
      Text_IO.Put_Line (Output, Integer'Image (Source_Array (I)));
   end loop;

   Close (Output);

exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (S (1 .. Last));
      Integer_IO.Put (Integer (J)); New_Line;
      Close (Output);

end Dictflag;
