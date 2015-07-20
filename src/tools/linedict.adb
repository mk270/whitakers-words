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
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;

procedure linedict is
   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   de : Dictionary_Entry;

   dictionary_file : File_Type;
   output : File_Type;

   st_line, pt_line, mn_line : String (1 .. 300) :=  (others => ' ');
   blank_line : constant String (1 .. 300) := (others => ' ');
   l, ll, last : Integer := 0;
   number_of_dictionary_entries : Integer := 0;

   procedure get_stem (s : in String;
                       stem : out Stem_Type; last : out Integer) is
      i  : Integer := 1;
      l  : Integer := s'First;
   begin
      stem := Null_Stem_Type;
      --  Squeeze left
      while l <= s'Last and then s (l) = ' '  loop
         l := l + 1;
      end loop;
      --  Count until the first blank
      --  Return that String
      while l <= s'Last and then s (l) /= ' '  loop
         stem (i) := s (l);
         i := i + 1;
         l := l + 1;
      end loop;
      --  Return  last
      last := l;
   end get_stem;

begin
   Put_Line ("LINEDICT.IN (EDIT format - 3 lines)" &
     " -> LINEDICT.OUT (DICTLINE format)");

   Create (output, Out_File, "LINEDICT.OUT");

   Open (dictionary_file, In_File, "LINEDICT.IN");
   Put ("Dictionary loading");

   while not End_Of_File (dictionary_file)  loop
      st_line := blank_line;
      pt_line := blank_line;
      mn_line := blank_line;
      Error_Check :
      begin

         Get_Non_Comment_Line (dictionary_file, st_line, last); --  STEMS

         -- really? -- when is "line" supposed to be read?
         -- line := blank_line;
         Get_Non_Comment_Line (dictionary_file, pt_line, l);    --  PART
         Get (pt_line (1 .. l), de.Part, ll);
         --            GET (PT_LINE (LL+1 .. L), DE.PART.POFS, DE.KIND, LL);

         Get (pt_line (ll + 1 .. l), de.Tran.Age, ll);
         Get (pt_line (ll + 1 .. l), de.Tran.Area, ll);
         Get (pt_line (ll + 1 .. l), de.Tran.Geo, ll);
         Get (pt_line (ll + 1 .. l), de.Tran.Freq, ll);
         Get (pt_line (ll + 1 .. l), de.Tran.Source, ll);

         de.Stems := Null_Stems_Type;
         ll := 1;
         --  Extract up to 4 Stems

         for i in 1 .. Number_Of_Stems (de.Part.pofs)
         loop   --  EXTRACT STEMS
            get_stem (st_line (ll .. last), de.Stems (i), ll);
         end loop;

         -- line := blank_line;
         Get_Non_Comment_Line (dictionary_file, mn_line, l);   --  MEANING

         de.Mean := Head (Trim (mn_line (1 .. l)), Max_Meaning_Size);

         Put (output, de); New_Line (output);

         number_of_dictionary_entries := number_of_dictionary_entries + 1;

      exception
         when others  =>
            Put_Line
              ("-------------------------------------------------------------");
            Put_Line (Head (st_line, 78));
            Put_Line (Head (pt_line, 78));
            Put_Line (Head (mn_line, 78));
      end Error_Check;

   end loop;
   Close (dictionary_file);
   Close (output);
   Set_Col (33); Put ("--  "); Integer_IO.Put (number_of_dictionary_entries);
   Put (" entries");    Set_Col (55); Put_Line ("--  Loaded correctly");
end linedict;
