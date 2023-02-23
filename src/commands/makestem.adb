-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936-2010)
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

with Ada.Strings.Maps; use all type Ada.Strings.Maps.Character_Mapping;
with Ada.Text_IO;
with Latin_Utils.Config;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Support_Utils.Char_Utils;
with Latin_Utils.General;

procedure Makestem is
   use Stem_Key_Type_IO;
   use Count_Io;
   use Ada.Text_IO;
   use Stem_Io;
   use MNPC_IO;
   use Part_Entry_IO;

   D_K : Dictionary_Kind := Xxx;   --  ######################

   I : Stem_Io.Count := 0;
   Line : String (1 .. 200) := (others => ' ');
   Blanks : constant String (1 .. 200) := (others => ' ');
   Last, Ll : Integer := 0;
   Ds : Dictionary_Stem;
   Fc, Ofc : Character := ' ';
   Sc, Osc : Character := ' ';

   procedure Put_Indices (Ch : String;
                          D_K : Dictionary_Kind) is
      Wd : constant String (1 .. 2) := Ch (Ch'First .. Ch'First + 1);
   begin
      --Put_Line ("Put_Indices");
      if Ch = "  "  then
         if (Bblf (Ch (Ch'First), Ch (Ch'First + 1), D_K) > 0) and then
           (Bbll (Ch (Ch'First), Ch (Ch'First + 1), D_K) >=
            Bblf (Ch (Ch'First), Ch (Ch'First + 1), D_K))
         then
            Put ("CH = ("); Put (Ch); Put (") index is of range  ");
            Put (Bblf (Ch (Ch'First), Ch (Ch'First + 1), D_K));
            Put (" .. "); Put (Bbll (Ch (Ch'First), Ch (Ch'First + 1), D_K));
            Put ("    number ");
            Put
              (Bbll (Ch (Ch'First), Ch (Ch'First + 1), D_K) -
               Bblf (Ch (Ch'First), Ch (Ch'First + 1), D_K) + 1);
            New_Line;
         end if;
      elsif Ch (Ch'First + 1) = ' '  then
         if (Bdlf (Ch (Ch'First), Ch (Ch'First + 1), D_K) > 0) and then
           (Bdll (Ch (Ch'First), Ch (Ch'First + 1), D_K) >=
            Bdlf (Ch (Ch'First), Ch (Ch'First + 1), D_K))
         then
            Put ("CH = ("); Put (Ch); Put (") index is of range  ");
            Put (Bdlf (Ch (Ch'First), Ch (Ch'First + 1), D_K));
            Put (" .. "); Put (Bdll (Ch (Ch'First), Ch (Ch'First + 1), D_K));
            Put ("    number ");
            Put (Bdll (Ch (Ch'First), Ch (Ch'First + 1), D_K) -
                 Bdlf (Ch (Ch'First), Ch (Ch'First + 1), D_K) + 1);
            New_Line;
         end if;
      else
         if (First_Index (Wd, D_K) > 0) and then
           (Last_Index (Wd, D_K) >= First_Index (Wd, D_K))
         then
            Put ("CH = ("); Put (Wd); Put (") index is of range  ");
            Put (First_Index (Wd, D_K));
            Put (" .. "); Put (Last_Index (Wd, D_K));
            Put ("    number ");
            Put (Last_Index (Wd, D_K) - First_Index (Wd, D_K) + 1);
            New_Line;
         end if;
      end if;
   end Put_Indices;

begin
   Put_Line ("Creates STEMFILE.D_K and INDXFILE.D_K from STEMLIST.D_K");
   Latin_Utils.General.Load_Dictionary (Line, Last, D_K);

   Open (Stem_List (D_K), In_File,
         Latin_Utils.Config.Path (Stem_List_Name & '.' & Ext (D_K)));

   Create (Stem_File (D_K), Inout_File, Stem_File_Name & '.' & Ext (D_K));

   Create (Indx_File (D_K), Out_File, Indx_File_Name & '.' & Ext (D_K));

   ------------------------------------------------------------------

   --  This section assumes the blank ESSE stem is first - D_K GENERAL
   if D_K = General  then
      I := I + 1;
      Bblf (' ', ' ', General) := I;
      Bbll (' ', ' ', General) := 0;
      Line := Blanks;
      Get_Line (Stem_List (D_K), Line, Last);
      Put_Line (Line (1 .. Last));

      Fc := Line (1);
      Sc := Line (2);
      Ds.Stem := Line (1 .. Max_Stem_Size);

      Get (Line (Max_Stem_Size + 1 .. Last), Ds.Part, Ll);

      Get (Line (Ll + 1 .. Last), Ds.Key, Ll);

      Get (Line (Ll + 1 .. Last), Ds.MNPC, Ll);

      Write (Stem_File (D_K), Ds);
      Bbll (Fc, Sc, General) := I;          --  1

      Put (Indx_File (D_K), "  ");
      Put (Indx_File (D_K), ' ');
      Put (Indx_File (D_K), Bblf (' ', ' ', General));
      Put (Indx_File (D_K), ' ');
      Put (Indx_File (D_K), Bbll (' ', ' ', General));
      Put (Indx_File (D_K), ' ');
      New_Line (Indx_File (D_K));

      Put_Indices ("  ", General);

   end if;
   ------------------------------------------------------------------

   Fc  := 'a';
   Ofc := 'a';
   Sc  := ' ';
   Osc := ' ';
   Bdlf (Ofc, ' ', D_K) := I + 1;
   --DEBUG.PUT (" bf1 BDLF ("); DEBUG.PUT (OFC);
   --DEBUG.PUT (' '); DEBUG.PUT (")  "); DEBUG.PUT (BDLF (OFC, ' ', D_K));
   --DEBUG.NEW_LINE;

   First_Character_Loop : while not End_Of_File (Stem_List (D_K))  loop
      Osc := Sc;

      Second_Character_Loop : while not End_Of_File (Stem_List (D_K))  loop

         Inner_Loop : while not End_Of_File (Stem_List (D_K))  loop
            Line := Blanks;
            Get_Line (Stem_List (D_K), Line, Last);
            --Put_Line ("* " & Line (1 .. Last));

            if Trim (Line (1 .. Last)) = "" then
               Put_Line ("Trim (Line (1 .. Last)) BLANK");
            end if;
            exit First_Character_Loop when Trim (Line (1 .. Last)) = "";
            Fc := Value (Support_Utils.Char_Utils.Normalize, Line (1));
            Sc := Value (Support_Utils.Char_Utils.Normalize, Line (2));

            --------------------------------------------------------------------
            I := I + 1;

            if Sc = ' '  then
               --Put ("BDL    I -> "); Put (I       ); New_Line;
               if Fc /= Ofc  then
                  Bdlf (Fc, ' ', D_K) := I;
                  --Put (" bf2 BDLF ("); Put (Fc);Put (' '); Put (")  ");
                  --Put (Bdlf (Fc, ' ', D_K)); New_Line;
               end if;
            else
               null;
               --Put ("I        -> "); Put (I); New_Line;
            end if;

            Ds.Stem := Line (1 .. Max_Stem_Size);
            Get (Line (Max_Stem_Size + 1 .. Last), Ds.Part, Ll);
            Get (Line (Ll + 1 .. Last), Ds.Key, Ll);
            Get (Line (Ll + 1 .. Last), Ds.MNPC, Ll);
            -- FIXME: code above is duplicated in another file
            Write (Stem_File (D_K), Ds);
            --Put_Line ("Wrote STEMfile");

            if Fc /= Ofc   then
               --  Jumped FC, effectively must have jumped a SC
               --Put_Line ("Jumped FC");
               if Osc = ' '  then
                  Bdll (Ofc, Osc, D_K) := I - 1;
               else
                  Ddll (Ofc, Osc, D_K) := I - 1;
               end if;

               if Sc = ' '  then
                  --Put ("BDLF  "); Put (Bdlf (Fc, Sc, D_K)); New_Line;
                  Bdlf (Fc, Sc, D_K) := I;
               else
                  Ddlf (Fc, Sc, D_K) := I;
               end if;
               --Put_Line ("if Sc done");
               --Put ("Ofc = '"); Put (Ofc);
               Put ("'   Osc = '"); Put (Osc); Put_Line ("'");
               Put_Indices (Ofc & Osc, D_K);
               Ofc := Fc;
               Osc := Sc;
               --Put_Line ("exit Second_Character_Loop");

               exit Second_Character_Loop;
            else
               if Sc /= Osc  then        --  Jumped a SC, but not a FC
                  if Osc = ' '  then     --  Jumped a SC from ' ' to something
                     Bdll (Fc, Osc, D_K) := I - 1;            --  So set BDLL
                     Ddlf (Fc, Sc, D_K) := I;

                     Put_Indices (Fc & Osc, D_K);
                     Osc := Sc;

                     exit Inner_Loop;
                  else                 --  Jumped a SL from something, not ' '
                     Ddll (Fc, Osc, D_K) := I - 1;    --  So set DDLL
                     Ddlf (Fc, Sc, D_K) := I;
                     Put_Indices (Fc & Osc, D_K);
                     Osc := Sc;

                     exit Inner_Loop;
                  end if;
               end if;
            end if;

         end loop Inner_Loop;
         --Put_Line ("Exitted Inner_Loop");

      end loop Second_Character_Loop;
      --Put_Line ("Exitted Second_Character_Loop");

   end loop First_Character_Loop;
   --Put_Line ("Exitted First_Character_Loop");
   Ddll (Ofc, Osc, D_K) := I;

   --  To reprint correctly the last letter information
   --Put_Line ("--  To reprint correctly the last letter information");
   Put_Indices (Ofc & Osc, D_K);
   Close (Stem_File (D_K));

   for I in Character'('a') .. Character'('z')  loop
      for J in Character'(' ') .. Character'(' ')  loop
         Ada.Text_IO.Put (Indx_File (D_K), (I, J));
         Put (Indx_File (D_K), ' ');
         Put (Indx_File (D_K), Bdlf (I, J, D_K));
         Put (Indx_File (D_K), ' ');
         Put (Indx_File (D_K), Bdll (I, J, D_K));
         Put (Indx_File (D_K), ' ');
         New_Line (Indx_File (D_K));
      end loop;
   end loop;

   for I in Character'('a') .. Character'('z')  loop
      for J in Character'('a') .. Character'('z')  loop
         Ada.Text_IO.Put (Indx_File (D_K), (I, J));
         Put (Indx_File (D_K), ' ');
         Put (Indx_File (D_K), Ddlf (I, J, D_K));
         Put (Indx_File (D_K), ' ');
         Put (Indx_File (D_K), Ddll (I, J, D_K));
         Put (Indx_File (D_K), ' ');
         New_Line (Indx_File (D_K));
      end loop;
   end loop;
   Close (Indx_File (D_K));
end Makestem;
