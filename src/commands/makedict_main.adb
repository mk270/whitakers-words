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

with Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Latin_Utils.General;
procedure makedict_main(porting : Boolean) is
   package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);
   use Ada.Text_IO;
   use Stem_Key_Type_IO;
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

   be_ve : constant Verb_Entry := (Con => (5, 1), Kind => to_be);

   d_k : Dictionary_Kind := xxx;       --  ######################

   start_stem_1  : constant := 1;
   start_stem_2  : constant := start_stem_1 + Max_Stem_Size + 1;
   start_stem_3  : constant := start_stem_2 + Max_Stem_Size + 1;
   start_stem_4  : constant := start_stem_3 + Max_Stem_Size + 1;
   start_part    : constant := start_stem_4 + Max_Stem_Size + 1;

   dictfile : Dict_IO.File_Type;
   Input, stemlist : Ada.Text_IO.File_Type;
   de : Dictionary_Entry;

   blank_line : constant String(1..400) := (others => ' ');
   s, line : String(1..400) := (others => ' ');
   l, last : Integer := 0;
   j : Dict_IO.Count := 0;
   mean_to_be : constant Meaning_Type :=
     Head ("be; exist; (also used to form verb perfect passive tenses)" &
            " with NOM PERF PPL", Max_Meaning_Size);

begin
   Put_Line("Takes a DICTLINE.D_K and produces a STEMLIST.D_K and DICTFILE.D_K");
   Put_Line("This version inserts ESSE when D_K = GEN");
   Latin_Utils.General.Load_Dictionary (line, last, d_k);

   Open(Input, In_File, add_file_name_extension(dict_line_name,
                                                Dictionary_Kind'Image(d_k)));

   if not porting  then
      Create(stemlist, Out_File, add_file_name_extension(stem_list_name,
                                                         Dictionary_Kind'Image(d_k)));
   end if;

   Create(dictfile, Out_File, add_file_name_extension(dict_file_name,
                                                      Dictionary_Kind'Image(d_k)));
   over_lines:
   while not End_Of_File(Input) loop
      s := blank_line;
      Get_Line(Input, s, last);
      if Trim (s(1..last)) /= ""  then
         l := 0;

         form_de:
         begin
            de.Stems(1) := s(start_stem_1..Max_Stem_Size);
            de.Stems(2) := s(start_stem_2..start_stem_2+Max_Stem_Size-1);
            de.Stems(3) := s(start_stem_3..start_stem_3+Max_Stem_Size-1);
            de.Stems(4) := s(start_stem_4..start_stem_4+Max_Stem_Size-1);
            Get(s(start_part..last), de.Part, l);
            -- FIXME: Why not Translation_Record_IO.Get ?
            Get(s(l+1..last), de.Tran.Age, l);
            Get(s(l+1..last), de.Tran.Area, l);
            Get(s(l+1..last), de.Tran.Geo, l);
            Get(s(l+1..last), de.Tran.Freq, l);
            Get(s(l+1..last), de.Tran.Source, l);
            de.Mean := Head (s(l+2..last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING, not ENUM
         exception
            when others =>
               New_Line;
               Put_Line("Exception");
               Put_Line(s(1..last));
               Integer_IO.Put(Integer(j)); New_Line;
               Put(de); New_Line;
         end form_de;

         j := j + 1;
         Write(dictfile, de, j);

         if not porting  then
            if de.Part.pofs = N    and then
               de.Stems(1) = de.Stems(2)     and then
               de.Stems(1) /= ZZZ_Stem
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 0, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Adj  and then
               de.Stems(1) = de.Stems(2)     and then
               de.Stems(1) /= ZZZ_Stem
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 0, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               if de.Stems(3) /= Null_Stem_Type  and de.Stems(3) /= ZZZ_Stem  then
                  Put(stemlist, de.Stems(3)); Put(stemlist, ' ');
                  Put(stemlist, de.Part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;
               if de.Stems(4) /= Null_Stem_Type  and de.Stems(4) /= ZZZ_Stem  then
                  Put(stemlist, de.Stems(4)); Put(stemlist, ' ');
                  Put(stemlist, de.Part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;
            elsif de.Part.pofs = Adj  and then
               --  POS taken care of by position
               de.Part.Adj.Co = Comp
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Adj  and then
               de.Part.Adj.Co = Super
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Adv  and then
               --  POS taken care of by position
               de.Part.Adv.Co = Comp
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 2, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Adv  and then
               de.Part.Adv.Co = Super
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = V    and then
               de.Stems(1) = de.Stems(2)     and then
               de.Stems(1) /= ZZZ_Stem
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 0, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               if de.Stems(3) /= Null_Stem_Type  and de.Stems(3) /= ZZZ_Stem  then
                  Put(stemlist, de.Stems(3)); Put(stemlist, ' ');
                  Put(stemlist, de.Part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;

               if de.Stems(4) /= Null_Stem_Type  and de.Stems(4) /= ZZZ_Stem  then
                  Put(stemlist, de.Stems(4)); Put(stemlist, ' ');
                  Put(stemlist, de.Part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;
            elsif de.Part.pofs = Num  and then
               de.Part.Num.Sort = Card
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 1, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Num  and then
               de.Part.Num.Sort = Ord
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 2, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Num  and then
               de.Part.Num.Sort = Dist
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.Part.pofs = Num  and then
               de.Part.Num.Sort = Adverb
            then
               Put(stemlist, de.Stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.Part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            else
               for i in Stem_Key_Type range 1..4  loop
                  if de.Stems(i) /= ZZZ_Stem  and
                     de.Stems(i) /= Null_Stem_Type
                  then
                     Put(stemlist, de.Stems(i)); Put(stemlist, ' ');
                     Put(stemlist, de.Part); Put(stemlist, ' ');
                     Set_Col(stemlist, 45);
                     Put(stemlist, i, 2); Put(stemlist, ' ');
                     Set_Col(stemlist, 50);
                     Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
                  end if;
               end loop;
            end if;
         end if;   --  PORTING
      end if;
   end loop over_lines;

   if d_k = general  then
      j := j + 1;

      --  First construct ESSE
      de.Stems(1) := "s                 ";
      de.Stems(2) := "                  ";
      de.Stems(3) := "fu                ";
      de.Stems(4) := "fut               ";
      --DE.PART := (PART => V,  CON => (5, 10));
      --DE.PART := (V, ((5, 1)));
      de.Part := (V, be_ve);
      --DE.KIND := (V, TO_BE);
      de.Tran := (x, x, x, a, x);
      de.Mean := mean_to_be;

      if not porting  then
         --  Load ESSE
         for i in Stem_Key_Type range 1..4  loop
            Put(stemlist, de.Stems(i)); Put(stemlist, ' ');
            Put(stemlist, de.Part); Put(stemlist, ' ');
            Set_Col(stemlist, 45);
            Put(stemlist, i, 2); Put(stemlist, ' ');
            Set_Col(stemlist, 50);
            Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
         end loop;
      end if;

      Write(dictfile, de, j);
   end if;

   if not porting  then
      Close(stemlist);
   end if;

exception
   when Ada.Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line(s(1..last));
      Integer_IO.Put(Integer(j)); New_Line;
      Close(stemlist);
end makedict_main;
