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
with Latin_Utils.Config;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Latin_Utils.General;
procedure Makedict_Main (Porting : Boolean) is
   package Integer_IO renames Ada.Integer_Text_IO;
   use Ada.Text_IO;
   use Stem_Key_Type_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;
   use Dict_IO;

   Be_Ve : constant Verb_Entry := (Con => (5, 1), Kind => To_Be);

   D_K : Dictionary_Kind := Xxx;       --  ######################

   Start_Stem_1  : constant := 1;
   Start_Stem_2  : constant := Start_Stem_1 + Max_Stem_Size + 1;
   Start_Stem_3  : constant := Start_Stem_2 + Max_Stem_Size + 1;
   Start_Stem_4  : constant := Start_Stem_3 + Max_Stem_Size + 1;
   Start_Part    : constant := Start_Stem_4 + Max_Stem_Size + 1;

   Dictfile : Dict_IO.File_Type;
   Input, Stemlist : Ada.Text_IO.File_Type;
   De : Dictionary_Entry;

   Blank_Line : constant String (1 .. 400) := (others => ' ');
   S, Line : String (1 .. 400) := (others => ' ');
   L, Last : Integer := 0;
   J : Dict_IO.Count := 0;
   Mean_To_Be : constant Meaning_Type :=
     Head ("be; exist; (also used to form verb perfect passive tenses)" &
     " with NOM PERF PPL", Max_Meaning_Size);

begin
   Put_Line
     ("Takes a DICTLINE.D_K and produces a STEMLIST.D_K and DICTFILE.D_K");
   Put_Line ("This version inserts ESSE when D_K = GEN");
   Latin_Utils.General.Load_Dictionary (Line, Last, D_K);

   Open (Input, In_File,
         Latin_Utils.Config.Path (Dict_Line_Name & '.' & Ext (D_K)));

   if not Porting  then
      Create (Stemlist, Out_File, Stem_List_Name & '.' & Ext (D_K));
   end if;

   Create (Dictfile, Out_File, Dict_File_Name & '.' & Ext (D_K));
   Over_Lines :
   while not End_Of_File (Input) loop
      S := Blank_Line;
      Get_Line (Input, S, Last);
      if Trim (S (1 .. Last)) /= ""  then
         L := 0;

         Form_De :
         begin
            De.Stems (1) := S (Start_Stem_1 .. Max_Stem_Size);
            De.Stems (2) := S (Start_Stem_2 .. Start_Stem_2 +
              Max_Stem_Size - 1);
            De.Stems (3) := S (Start_Stem_3 .. Start_Stem_3 +
              Max_Stem_Size - 1);
            De.Stems (4) := S (Start_Stem_4 .. Start_Stem_4 +
              Max_Stem_Size - 1);
            Get (S (Start_Part .. Last), De.Part, L);
            -- FIXME: Why not Translation_Record_IO.Get ?
            Get (S (L + 1 .. Last), De.Tran.Age, L);
            Get (S (L + 1 .. Last), De.Tran.Area, L);
            Get (S (L + 1 .. Last), De.Tran.Geo, L);
            Get (S (L + 1 .. Last), De.Tran.Freq, L);
            Get (S (L + 1 .. Last), De.Tran.Source, L);
            De.Mean := Head (S (L + 2 .. Last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is
            --  STRING, not ENUM
         exception
            when others =>
               New_Line;
               Put_Line ("Exception");
               Put_Line (S (1 .. Last));
               Integer_IO.Put (Integer (J)); New_Line;
               Put (De); New_Line;
         end Form_De;

         J := J + 1;
         Write (Dictfile, De, J);

         if not Porting  then
            if De.Part.Pofs = N    and then
              De.Stems (1) = De.Stems (2)     and then
              De.Stems (1) /= ZZZ_Stem
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 0, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Adj  and then
              De.Stems (1) = De.Stems (2)     and then
              De.Stems (1) /= ZZZ_Stem
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 0, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
               if De.Stems (3) /= Null_Stem_Type
                 and De.Stems (3) /= ZZZ_Stem
               then
                  Put (Stemlist, De.Stems (3)); Put (Stemlist, ' ');
                  Put (Stemlist, De.Part); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 45);
                  Integer_IO.Put (Stemlist, 3, 2); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 50);
                  Integer_IO.Put (Stemlist, Integer (J), 6);
                  New_Line (Stemlist);
               end if;
               if De.Stems (4) /= Null_Stem_Type
                 and De.Stems (4) /= ZZZ_Stem
               then
                  Put (Stemlist, De.Stems (4)); Put (Stemlist, ' ');
                  Put (Stemlist, De.Part); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 45);
                  Integer_IO.Put (Stemlist, 4, 2); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 50);
                  Integer_IO.Put (Stemlist, Integer (J), 6);
                  New_Line (Stemlist);
               end if;
            elsif De.Part.Pofs = Adj  and then
               --  POS taken care of by position
               De.Part.Adj.Co = Comp
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 3, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Adj  and then
               De.Part.Adj.Co = Super
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 4, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Adv  and then
               --  POS taken care of by position
               De.Part.Adv.Co = Comp
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 2, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Adv  and then
               De.Part.Adv.Co = Super
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 3, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = V    and then
              De.Stems (1) = De.Stems (2)     and then
              De.Stems (1) /= ZZZ_Stem
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 0, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
               if De.Stems (3) /= Null_Stem_Type
                 and De.Stems (3) /= ZZZ_Stem
               then
                  Put (Stemlist, De.Stems (3)); Put (Stemlist, ' ');
                  Put (Stemlist, De.Part); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 45);
                  Integer_IO.Put (Stemlist, 3, 2); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 50);
                  Integer_IO.Put (Stemlist, Integer (J), 6);
                  New_Line (Stemlist);
               end if;

               if De.Stems (4) /= Null_Stem_Type
                 and De.Stems (4) /= ZZZ_Stem
               then
                  Put (Stemlist, De.Stems (4)); Put (Stemlist, ' ');
                  Put (Stemlist, De.Part); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 45);
                  Integer_IO.Put (Stemlist, 4, 2); Put (Stemlist, ' ');
                  Set_Col (Stemlist, 50);
                  Integer_IO.Put (Stemlist, Integer (J), 6);
                  New_Line (Stemlist);
               end if;
            elsif De.Part.Pofs = Num  and then
               De.Part.Num.Sort = Card
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 1, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Num  and then
               De.Part.Num.Sort = Ord
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 2, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Num  and then
               De.Part.Num.Sort = Dist
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 3, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            elsif De.Part.Pofs = Num  and then
               De.Part.Num.Sort = Adverb
            then
               Put (Stemlist, De.Stems (1)); Put (Stemlist, ' ');
               Put (Stemlist, De.Part); Put (Stemlist, ' ');
               Set_Col (Stemlist, 45);
               Integer_IO.Put (Stemlist, 4, 2); Put (Stemlist, ' ');
               Set_Col (Stemlist, 50);
               Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
            else
               for I in Stem_Key_Type range 1 .. 4  loop
                  if De.Stems (I) /= ZZZ_Stem  and
                    De.Stems (I) /= Null_Stem_Type
                  then
                     Put (Stemlist, De.Stems (I)); Put (Stemlist, ' ');
                     Put (Stemlist, De.Part); Put (Stemlist, ' ');
                     Set_Col (Stemlist, 45);
                     Put (Stemlist, I, 2); Put (Stemlist, ' ');
                     Set_Col (Stemlist, 50);
                     Integer_IO.Put (Stemlist, Integer (J), 6);
                     New_Line (Stemlist);
                  end if;
               end loop;
            end if;
         end if;   --  PORTING
      end if;
   end loop Over_Lines;

   if D_K = General  then
      J := J + 1;

      --  First construct ESSE
      De.Stems (1) := "s                 ";
      De.Stems (2) := "                  ";
      De.Stems (3) := "fu                ";
      De.Stems (4) := "fut               ";
      --DE.PART := (PART => V,  CON => (5, 10));
      --DE.PART := (V, ((5, 1)));
      De.Part := (V, Be_Ve);
      --DE.KIND := (V, TO_BE);
      De.Tran := (X, X, X, A, X);
      De.Mean := Mean_To_Be;

      if not Porting  then
         --  Load ESSE
         for I in Stem_Key_Type range 1 .. 4  loop
            Put (Stemlist, De.Stems (I)); Put (Stemlist, ' ');
            Put (Stemlist, De.Part); Put (Stemlist, ' ');
            Set_Col (Stemlist, 45);
            Put (Stemlist, I, 2); Put (Stemlist, ' ');
            Set_Col (Stemlist, 50);
            Integer_IO.Put (Stemlist, Integer (J), 6); New_Line (Stemlist);
         end loop;
      end if;

      Write (Dictfile, De, J);
   end if;

   if not Porting  then
      Close (Stemlist);
   end if;

exception
   when Ada.Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (S (1 .. Last));
      Integer_IO.Put (Integer (J)); New_Line;
      Close (Stemlist);
end Makedict_Main;
