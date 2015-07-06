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
with Strings_Package; use Strings_Package;
with Latin_File_Names; use Latin_File_Names;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
procedure makedict_main(porting : Boolean) is
   package Integer_IO is new Text_IO.Integer_IO(Integer);
   use Text_IO;
   use Stem_Key_Type_IO;
   use dictionary_entry_io;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use translation_record_io;
   use age_type_io;
   use area_type_io;
   use geo_type_io;
   use frequency_type_io;
   use source_type_io;
   use Dict_IO;

   be_ve : constant Verb_Entry := (Con => (5, 1), Kind => to_be);

   d_k : Dictionary_Kind := xxx;       --  ######################

   start_stem_1  : constant := 1;
   start_stem_2  : constant := start_stem_1 + Max_Stem_Size + 1;
   start_stem_3  : constant := start_stem_2 + Max_Stem_Size + 1;
   start_stem_4  : constant := start_stem_3 + Max_Stem_Size + 1;
   start_part    : constant := start_stem_4 + Max_Stem_Size + 1;

   dictfile : Dict_IO.File_Type;
   Input, stemlist : Text_IO.File_Type;
   de : dictionary_entry;

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
   Put("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
   Get_Line(line, last);
   if last > 0  then
      if Trim (line(1..last))(1) = 'G' or else
         Trim (line(1..last))(1) = 'g'
      then
         d_k := general;
      elsif Trim (line(1..last))(1) = 'S' or else
         Trim (line(1..last))(1) = 's'
      then
         d_k := special;
      else
         Put_Line("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

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
            de.stems(1) := s(start_stem_1..Max_Stem_Size);
            de.stems(2) := s(start_stem_2..start_stem_2+Max_Stem_Size-1);
            de.stems(3) := s(start_stem_3..start_stem_3+Max_Stem_Size-1);
            de.stems(4) := s(start_stem_4..start_stem_4+Max_Stem_Size-1);
            Get(s(start_part..last), de.part, l);
            Get(s(l+1..last), de.tran.age, l);
            Get(s(l+1..last), de.tran.area, l);
            Get(s(l+1..last), de.tran.geo, l);
            Get(s(l+1..last), de.tran.freq, l);
            Get(s(l+1..last), de.tran.source, l);
            de.mean := Head (s(l+2..last), Max_Meaning_Size);
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
            if de.part.pofs = n    and then
               de.stems(1) = de.stems(2)     and then
               de.stems(1) /= zzz_stem
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 0, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = adj  and then
               de.stems(1) = de.stems(2)     and then
               de.stems(1) /= zzz_stem
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 0, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               if de.stems(3) /= Null_Stem_Type  and de.stems(3) /= zzz_stem  then
                  Put(stemlist, de.stems(3)); Put(stemlist, ' ');
                  Put(stemlist, de.part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;
               if de.stems(4) /= Null_Stem_Type  and de.stems(4) /= zzz_stem  then
                  Put(stemlist, de.stems(4)); Put(stemlist, ' ');
                  Put(stemlist, de.part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;
            elsif de.part.pofs = adj  and then
               --  POS taken care of by position
               de.part.adj.Co = comp
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = adj  and then
               de.part.adj.Co = super
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = adv  and then
               --  POS taken care of by position
               de.part.adv.Co = comp
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 2, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = adv  and then
               de.part.adv.Co = super
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = v    and then
               de.stems(1) = de.stems(2)     and then
               de.stems(1) /= zzz_stem
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 0, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               if de.stems(3) /= Null_Stem_Type  and de.stems(3) /= zzz_stem  then
                  Put(stemlist, de.stems(3)); Put(stemlist, ' ');
                  Put(stemlist, de.part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;

               if de.stems(4) /= Null_Stem_Type  and de.stems(4) /= zzz_stem  then
                  Put(stemlist, de.stems(4)); Put(stemlist, ' ');
                  Put(stemlist, de.part); Put(stemlist, ' ');
                  Set_Col(stemlist, 45);
                  Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
                  Set_Col(stemlist, 50);
                  Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
               end if;
            elsif de.part.pofs = num  and then
               de.part.num.Sort = card
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 1, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = num  and then
               de.part.num.Sort = ord
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 2, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = num  and then
               de.part.num.Sort = dist
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 3, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            elsif de.part.pofs = num  and then
               de.part.num.Sort = adverb
            then
               Put(stemlist, de.stems(1)); Put(stemlist, ' ');
               Put(stemlist, de.part); Put(stemlist, ' ');
               Set_Col(stemlist, 45);
               Integer_IO.Put(stemlist, 4, 2); Put(stemlist, ' ');
               Set_Col(stemlist, 50);
               Integer_IO.Put(stemlist, Integer(j), 6); New_Line(stemlist);
            else
               for i in Stem_Key_Type range 1..4  loop
                  if de.stems(i) /= zzz_stem  and
                     de.stems(i) /= Null_Stem_Type
                  then
                     Put(stemlist, de.stems(i)); Put(stemlist, ' ');
                     Put(stemlist, de.part); Put(stemlist, ' ');
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
      de.stems(1) := "s                 ";
      de.stems(2) := "                  ";
      de.stems(3) := "fu                ";
      de.stems(4) := "fut               ";
      --DE.PART := (PART => V,  CON => (5, 10));
      --DE.PART := (V, ((5, 1)));
      de.part := (v, be_ve);
      --DE.KIND := (V, TO_BE);
      de.tran := (x, x, x, a, x);
      de.mean := mean_to_be;

      if not porting  then
         --  Load ESSE
         for i in Stem_Key_Type range 1..4  loop
            Put(stemlist, de.stems(i)); Put(stemlist, ' ');
            Put(stemlist, de.part); Put(stemlist, ' ');
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
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line(s(1..last));
      Integer_IO.Put(Integer(j)); New_Line;
      Close(stemlist);
end makedict_main;
