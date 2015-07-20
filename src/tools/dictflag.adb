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
procedure dictflag is
   package Integer_IO is new Text_IO.Integer_IO (Integer);
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
   use Dict_IO;

   --be_ve : Verb_Entry := (Con => (5, 1), Kind => To_Be);

   d_k : Dictionary_Kind := xxx;       --  ######################

   Start_Stem_1  : constant := 1;
   Start_Stem_2  : constant := Start_Stem_1 + Max_Stem_Size + 1;
   Start_Stem_3  : constant := Start_Stem_2 + Max_Stem_Size + 1;
   Start_Stem_4  : constant := Start_Stem_3 + Max_Stem_Size + 1;
   start_part    : constant := Start_Stem_4 + Max_Stem_Size + 1;
   --start_tran    : constant Integer :=
   --  start_part +
   --  Integer (Part_Entry_IO.Default_Width + 1);
   --finish_line   : constant Integer :=
   --  start_tran +
   --  Translation_Record_IO.Default_Width - 1;

   age_array : array (Age_Type'Range) of Integer := (others => 0);

   area_array : array (Area_Type'Range) of Integer := (others => 0);

   geo_array : array (Geo_Type'Range) of Integer := (others => 0);

   Freq_Array : array (Frequency_Type'Range) of Integer := (others => 0);

   source_array : array (Source_Type'Range) of Integer := (others => 0);

   -- dictfile : Dict_IO.File_Type;
   input, output : Text_IO.File_Type;
   de : Dictionary_Entry;

   s, line : String (1 .. 400) := (others => ' ');
   blank_line : constant String (1 .. 400) := (others => ' ');
   l, last : Integer := 0;
   j : Dict_IO.Count := 0;

   --mean_to_be : constant Meaning_Type :=
   --  Head ("to be, exist; also used to form verb perfect passive tenses" &
   --  " with NOM PERF PPL", Max_Meaning_Size);

   -- mean_to_be unreferenced - perhaps it was not meant to be...

begin
   Put_Line (
     "Takes a DICTLINE.D_K and produces a numeration of FLAGS");
   Put ("What dictionary to list, GENERAL or SPECIAL  =>");
   Get_Line (line, last);
   if last > 0  then
      if Trim (line (1 .. last))(1) = 'G'  or else
        Trim (line (1 .. last))(1) = 'g'
      then
         d_k := general;
      elsif Trim (line (1 .. last))(1) = 'S'  or else
        Trim (line (1 .. last))(1) = 's'
      then
         d_k := special;
      else
         Put_Line ("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

   Open (input, In_File, add_file_name_extension (dict_line_name,
     Dictionary_Kind'Image (d_k)));

   Create (output, Out_File, add_file_name_extension ("FLAGS",
     Dictionary_Kind'Image (d_k)));

   while not End_Of_File (input) loop
      s := blank_line;
      Get_Line (input, s, last);
      if Trim (s (1 .. last)) /= ""  then
         l := 0;

         Form_De :
         begin

            de.Stems (1) := s (Start_Stem_1 .. Max_Stem_Size);
            --NEW_LINE; PUT (DE.STEMS (1));
            de.Stems (2) :=
              s (Start_Stem_2 .. Start_Stem_2 + Max_Stem_Size - 1);
            de.Stems (3) :=
              s (Start_Stem_3 .. Start_Stem_3 + Max_Stem_Size - 1);
            de.Stems (4) :=
              s (Start_Stem_4 .. Start_Stem_4 + Max_Stem_Size - 1);

            --PUT ('#'); PUT (INTEGER'IMAGE (L)); PUT (INTEGER'IMAGE (LAST));
            --PUT ('@');
            Get (s (start_part .. last), de.Part, l);
            --PUT ('%'); PUT (INTEGER'IMAGE (L)); PUT (INTEGER'IMAGE (LAST));
            --PUT ('&'); PUT (S (L+1 .. LAST)); PUT ('3');
            --   GET (S (L+1 .. LAST), DE.PART.POFS, DE.KIND, L);

            Get (s (l + 1 .. last), de.Tran.Age, l);
            age_array (de.Tran.Age) := age_array (de.Tran.Age) + 1;
            Get (s (l + 1 .. last), de.Tran.Area, l);
            area_array (de.Tran.Area) := area_array (de.Tran.Area) + 1;
            Get (s (l + 1 .. last), de.Tran.Geo, l);
            geo_array (de.Tran.Geo) := geo_array (de.Tran.Geo) + 1;
            Get (s (l + 1 .. last), de.Tran.Freq, l);
            Freq_Array (de.Tran.Freq) := Freq_Array (de.Tran.Freq) + 1;
            Get (s (l + 1 .. last), de.Tran.Source, l);
            source_array (de.Tran.Source) := source_array (de.Tran.Source) + 1;

            de.Mean := Head (s (l + 2 .. last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING,
            --    not ENUM

         exception
            when others =>
               New_Line;
               Put_Line ("Exception");
               Put_Line (s (1 .. last));
               Integer_IO.Put (Integer (j)); New_Line;
               Put (de); New_Line;
         end Form_De;

         j := j + 1;

      end if;
   end loop;

   Text_IO.Put (output, "Number of lines in DICTLINE "  &
     Dictionary_Kind'Image (d_k) & "  ");
   Integer_IO.Put (output, Integer (j));
   Text_IO.New_Line (output);

   Text_IO.New_Line (output, 4);
   Text_IO.Put_Line (output, "AGE");
   for i in Age_Type'Range  loop
      Text_IO.Put (output, Age_Type'Image (i));
      Text_IO.Set_Col (output, 10);
      Text_IO.Put_Line (output, Integer'Image (age_array (i)));
   end loop;

   Text_IO.New_Line (output, 4);
   Text_IO.Put_Line (output, "AREA");
   for i in Area_Type'Range  loop
      Text_IO.Put (output, Area_Type'Image (i));
      Text_IO.Set_Col (output, 10);
      Text_IO.Put_Line (output, Integer'Image (area_array (i)));
   end loop;

   Text_IO.New_Line (output, 4);
   Text_IO.Put_Line (output, "GEO");
   for i in Geo_Type'Range  loop
      Text_IO.Put (output, Geo_Type'Image (i));
      Text_IO.Set_Col (output, 10);
      Text_IO.Put_Line (output, Integer'Image (geo_array (i)));
   end loop;

   Text_IO.New_Line (output, 4);
   Text_IO.Put_Line (output, "FREQ");
   for i in Frequency_Type'Range  loop
      Text_IO.Put (output, Frequency_Type'Image (i));
      Text_IO.Set_Col (output, 10);
      Text_IO.Put_Line (output, Integer'Image (Freq_Array (i)));
   end loop;

   Text_IO.New_Line (output, 4);
   Text_IO.Put_Line (output, "SOURCE");
   for i in Source_Type'Range  loop
      Text_IO.Put (output, Source_Type'Image (i));
      Text_IO.Set_Col (output, 10);
      Text_IO.Put_Line (output, Integer'Image (source_array (i)));
   end loop;

   Close (output);

exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (s (1 .. last));
      Integer_IO.Put (Integer (j)); New_Line;
      Close (output);

end dictflag;
