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

with Ada.Text_IO; use Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Config;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Words_Engine.Word_Package; use Words_Engine.Word_Package;
with Words_Engine.English_Support_Package;
use Words_Engine.English_Support_Package;
with Support_Utils.Dictionary_Form;
use Latin_Utils;

procedure Words_Engine.Search_English
  (Input_English_Word : String;
   Pofs : Part_Of_Speech_Type := X)
is
   use Ewds_Direct_Io;
   Input_Word : Eword := Lower_Case (Head (Input_English_Word, Eword_Size));
   Input_Pofs : constant Part_Of_Speech_Type := Pofs;

   Output_Array : Ewds_Array (1 .. 500) := (others => Null_Ewds_Record);
   Number_Of_Hits : Integer := 0;
   J1, J2, J : Ewds_Direct_Io.Count := 0;

   D_K : constant Dictionary_Kind := General;    --  For the moment

   Ewds : Ewds_Record := Null_Ewds_Record;

   First_Try, Second_Try : Boolean := True;

   procedure Load_Output_Array (Ewds : in Ewds_Record) is
   begin
      if Ewds.Pofs <= Input_Pofs  then
         Number_Of_Hits := Number_Of_Hits + 1;
         Output_Array (Number_Of_Hits) := Ewds;
      end if;
   end Load_Output_Array;

   --procedure TRIM_OUTPUT_ARRAY is
   procedure Sort_Output_Array is
      Hits : Integer := 0;
   begin

      --  Bubble sort
      Hit_Loop :
      loop
         Hits := 0;

         Switch :
         declare
            Dw : Ewds_Record := Null_Ewds_Record;
         begin
            Inner_Loop :    --  Order by RANK, FREQ, SEMI
            for I in 1 .. Number_Of_Hits - 1  loop
               if Output_Array (I + 1).Rank  >  Output_Array (I).Rank or else
                 (Output_Array (I + 1).Rank  =  Output_Array (I).Rank and then
                 Output_Array (I + 1).Freq  <  Output_Array (I).Freq) or else
                 (Output_Array (I + 1).Rank  =  Output_Array (I).Rank and then
                 Output_Array (I + 1).Freq  =  Output_Array (I).Freq and then
                 Output_Array (I + 1).Semi  <  Output_Array (I).Semi)
               then
                  Dw := Output_Array (I);
                  Output_Array (I) := Output_Array (I + 1);
                  Output_Array (I + 1) := Dw;
                  Hits := Hits + 1;
                  --PUT_LINE ("HITS    " & INTEGER'IMAGE (HITS));
               end if;
            end loop Inner_Loop;
         end Switch;
         exit Hit_Loop when Hits = 0;
      end loop Hit_Loop;
   end Sort_Output_Array;

   procedure Dump_Output_Array (Output : in Ada.Text_IO.File_Type) is
      De : Dictionary_Entry := Null_Dictionary_Entry;
      Number_To_Show : Integer := Number_Of_Hits;
      One_Screen : constant Integer := 6;
   begin
      --TEXT_IO.PUT_LINE ("DUMP_OUTPUT");
      if Number_Of_Hits = 0  then
         Ada.Text_IO.Put_Line (Output, "No Match");
      else
         Sort_Output_Array;

         --TEXT_IO.PUT_LINE ("DUMP_OUTPUT SORTED");

         Trimmed := False;
         if Words_Mode (Trim_Output)  then
            if Number_Of_Hits > One_Screen  then
               Number_To_Show := One_Screen;
               Trimmed := True;
            else
               Number_To_Show := Number_Of_Hits;
            end if;
         end if;

         for I in 1 .. Number_To_Show  loop
            Ada.Text_IO.New_Line (Output);

            Do_Pause :
            begin
               if Integer (Ada.Text_IO.Line (Output)) >
                 Scroll_Line_Number + Config.Output_Screen_Size
               then
                  Pause (Output);
                  Scroll_Line_Number := Integer (Ada.Text_IO.Line (Output));
               end if;
            end Do_Pause;

            Dict_IO.Read (Dict_File (General),
              De, Dict_IO.Count (Output_Array (I).N));
            Put (Output, Support_Utils.Dictionary_Form (De));
            Ada.Text_IO.Put (Output, "   ");

            if De.Part.Pofs = N  then
               Ada.Text_IO.Put (Output, "  ");
               Decn_Record_IO.Put (Output, De.Part.N.Decl);
               Ada.Text_IO.Put (Output, "  " &
                 Gender_Type'Image (De.Part.N.Gender) & "  ");
            end if;
            if De.Part.Pofs = V then
               Ada.Text_IO.Put (Output, "  ");
               Decn_Record_IO.Put (Output, De.Part.V.Con);
            end if;
            if (De.Part.Pofs = V)  and then
              (De.Part.V.Kind in Gen .. Perfdef)
            then
               Ada.Text_IO.Put (Output, "  " &
                 Verb_Kind_Type'Image (De.Part.V.Kind) & "  ");
            end if;

            if Words_Mdev (Show_Dictionary_Codes)    then
               Ada.Text_IO.Put (Output, " [");
               -- FIXME: Why noy Translation_Record_IO.Put ?
               Age_Type_IO.Put (Output, De.Tran.Age);
               Area_Type_IO.Put (Output, De.Tran.Area);
               Geo_Type_IO.Put (Output, De.Tran.Geo);
               Frequency_Type_IO.Put (Output, De.Tran.Freq);
               Source_Type_IO.Put (Output, De.Tran.Source);
               Ada.Text_IO.Put (Output, "]  ");
            end if;

            if Words_Mdev (Show_Dictionary) then
               Ada.Text_IO.Put (Output, Ext (D_K) & ">");
            end if;
            --TEXT_IO.PUT_LINE ("DUMP_OUTPUT SHOW");

            if Words_Mdev (Show_Dictionary_Line)  then
               Ada.Text_IO.Put (Output, "("
                 & Trim (Integer'Image (Output_Array (I).N)) & ")");
            end if;

            Ada.Text_IO.New_Line (Output);

            --TEXT_IO.PUT_LINE ("DUMP_OUTPUT MEAN");

            Ada.Text_IO.Put (Output, Trim (De.Mean));
            Ada.Text_IO.New_Line (Output);

         end loop;
         --TEXT_IO.PUT_LINE ("DUMP_OUTPUT TRIMMED");

         if Trimmed then
            Put_Line (Output, "*");
         end if;

      end if;    --  On HITS = 0

   exception
      when others =>
         null;   --  If N not in DICT_FILE
   end Dump_Output_Array;

begin

   J1 := 1;
   J2 := Size (Ewds_File);

   First_Try := True;

   Second_Try := True;

   J := (J1 + J2) / 2;

   Binary_Search :
   loop
      --   TEXT_IO.PUT_LINE ("J = " & INTEGER'IMAGE (INTEGER (J)));

      if (J1 = J2 - 1) or (J1 = J2) then
         if First_Try  then
            J := J1;
            First_Try := False;
         elsif Second_Try  then
            J := J2;
            Second_Try := False;
         else
            exit Binary_Search;
         end if;
      end if;

      --  Should D_K
      Set_Index (Ewds_File, J);
      Read (Ewds_File, Ewds);
      if  "<"(Lower_Case (Ewds.W), Input_Word)  then  --  Not LTU, not u=v
         J1 := J;
         J := (J1 + J2) / 2;
      elsif  ">"(Lower_Case (Ewds.W), Input_Word)  then
         J2 := J;
         J := (J1 + J2) / 2;
      else
         for I in reverse J1 .. J  loop
            Set_Index (Ewds_File, Ewds_Direct_Io.Count (I));
            Read (Ewds_File, Ewds);    --  Reads and advances index!!

            if "="(Lower_Case (Ewds.W), Input_Word)  then
               Load_Output_Array (Ewds);
            else
               exit;
            end if;
         end loop;

         for I in J + 1 .. J2  loop
            Set_Index (Ewds_File, Ewds_Direct_Io.Count (I));
            Read (Ewds_File, Ewds);

            if "="(Lower_Case (Ewds.W), Input_Word)  then
               Load_Output_Array (Ewds);
            else
               exit Binary_Search;
            end if;
         end loop;

         exit Binary_Search;
      end if;
   end loop Binary_Search;

   if  Words_Mode (Write_Output_To_File)      then
      Dump_Output_Array (Output);
   else
      Dump_Output_Array (Current_Output);
   end if;
exception
   when others  =>
      Ada.Text_IO.Put_Line ("exception SEARCH NUMBER_OF_HITS = " &
        Integer'Image (Number_Of_Hits));
      raise;
end Words_Engine.Search_English;
