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
with word_support_package; use word_support_package;
with Char_Utils;

procedure makestem is
   use Stem_Key_Type_IO;
   use Count_io;
   use Text_IO;
   use stem_io;
   use MNPC_IO;
   use Part_Entry_IO;

   d_k : Dictionary_Kind := xxx;   --  ######################

   i : stem_io.Count := 0;
   line : String(1..200) := (others => ' ');
   blanks : constant String(1..200) := (others => ' ');
   last, ll : Integer := 0;
   ds : dictionary_stem;
   fc, ofc : Character := ' ';
   sc, osc : Character := ' ';

   procedure Put_indices(ch : String;
                         d_k : Dictionary_Kind) is
      wd : constant String(1..2) := ch(ch'First..ch'First+1);
   begin
      --Put_Line("Put_Indices");
      if ch = "  "  then
         if (bblf(ch(ch'First), ch(ch'First + 1), d_k) > 0)                    and then
            (bbll(ch(ch'First), ch(ch'First + 1), d_k) >= bblf(ch(ch'First), ch(ch'First + 1), d_k))
         then
            Put("CH = ("); Put(ch); Put(") index is of range  ");
            Put(bblf(ch(ch'First), ch(ch'First + 1), d_k)); Put(".."); Put(bbll(ch(ch'First), ch(ch'First + 1), d_k));
            Put("    number ");
            Put(bbll(ch(ch'First), ch(ch'First + 1), d_k) - bblf(ch(ch'First), ch(ch'First + 1), d_k) + 1);
            New_Line;
         end if;
      elsif ch(ch'First + 1) = ' '  then
         if (bdlf(ch(ch'First), ch(ch'First + 1), d_k) > 0) and then
            (bdll(ch(ch'First), ch(ch'First + 1), d_k) >= bdlf(ch(ch'First), ch(ch'First + 1), d_k))
         then
            Put("CH = ("); Put(ch); Put(") index is of range  ");
            Put(bdlf(ch(ch'First), ch(ch'First + 1), d_k)); Put(".."); Put(bdll(ch(ch'First), ch(ch'First + 1), d_k));
            Put("    number ");
            Put(bdll(ch(ch'First), ch(ch'First + 1), d_k) - bdlf(ch(ch'First), ch(ch'First + 1), d_k) + 1);
            New_Line;
         end if;
      else
         if (first_index(wd, d_k) > 0) and then
            (last_index(wd, d_k) >= first_index(wd, d_k))
         then
            Put("CH = ("); Put(wd); Put(") index is of range  ");
            Put(first_index(wd, d_k)); Put(".."); Put(last_index(wd, d_k));
            Put("    number ");
            Put(last_index(wd, d_k) - first_index(wd, d_k) + 1);
            New_Line;
         end if;
      end if;
   end Put_indices;

begin
   Put_Line("Creates STEMFILE.D_K and INDXFILE.D_K from STEMLIST.D_K");

   Put("What dictionary to load, GENERAL or SPECIAL  =>");
   Get_Line(line, last);
   if last > 0  then
      if Trim (line(1..last))(1) = 'G'  or else
         Trim (line(1..last))(1) = 'g'
      then
         d_k := general;
      elsif Trim (line(1..last))(1) = 'S'  or else
         Trim (line(1..last))(1) = 's'
      then
         d_k := special;
      else
         Put_Line("No such dictionary");
         raise Text_IO.Data_Error;
      end if;
   end if;

   Open(   stem_list(d_k), In_File,
           add_file_name_extension(stem_list_name,
                                   Dictionary_Kind'Image(d_k)));

   Create(stem_file(d_k), Inout_File,
          add_file_name_extension(stem_file_name,
                                  Dictionary_Kind'Image(d_k)));

   Create(indx_file(d_k), Out_File,
          add_file_name_extension(indx_file_name,
                                  Dictionary_Kind'Image(d_k)));

   ------------------------------------------------------------------

   --  This section assumes the blank ESSE stem is first - D_K GENERAL
   if d_k = general  then
      i := i + 1;
      bblf(' ', ' ', general) := i;
      bbll(' ', ' ', general) := 0;
      line := blanks;
      Get_Line(stem_list(d_k), line, last);
      Put_Line(line(1..last));

      fc := line(1);
      sc := line(2);
      ds.stem := line(1..Max_Stem_Size);
      --PUT_LINE("#" & LINE(MAX_STEM_SIZE+1..LAST));
      Get(line(Max_Stem_Size+1..last), ds.part, ll);
      --PUT(DS.PART); NEW_LINE;
      --PUT_LINE("#" & LINE(LL+1..LAST));
      Get(line(ll+1..last), ds.key , ll);
      --PUT(DS.KEY ); NEW_LINE;
      --PUT_LINE("#" & LINE(LL+1..LAST));
      Get(line(ll+1..last), ds.MNPC , ll);
      --PUT(DS.AAMNPC); NEW_LINE;
      Write(stem_file(d_k), ds);
      bbll(fc, sc, general) := i;          --  1

      Put(indx_file(d_k), "  ");
      Put(indx_file(d_k), ' ');
      Put(indx_file(d_k), bblf(' ', ' ', general));
      Put(indx_file(d_k), ' ');
      Put(indx_file(d_k), bbll(' ', ' ', general));
      Put(indx_file(d_k), ' ');
      New_Line(indx_file(d_k));

      Put_indices("  ", general);

   end if;
   ------------------------------------------------------------------

   fc  := 'a';
   ofc := 'a';
   sc  := ' ';
   osc := ' ';
   bdlf(ofc, ' ', d_k) := i + 1;
   --DEBUG.PUT(" bf1 BDLF("); DEBUG.PUT(OFC);
   --DEBUG.PUT(' '); DEBUG.PUT(")  "); DEBUG.PUT(BDLF(OFC, ' ', D_K));
   --DEBUG.NEW_LINE;

   first_Character_loop:
   while not End_Of_File(stem_list(d_k))  loop
      --OSC := ' ';
      osc := sc;

      second_Character_loop:
      while not End_Of_File(stem_list(d_k))  loop

         inner_loop:
         while not End_Of_File(stem_list(d_k))  loop
            line := blanks;
            Get_Line(stem_list(d_k), line, last);
            --Put_Line("* " & Line(1..Last));

            if Trim (line(1..last)) = "" then
               Put_Line("Trim(Line(1..Last)) BLANK");
            end if;
            exit first_Character_loop when Trim (line(1..last)) = "";
            fc := Lower_Case (line(1));
            sc := Lower_Case (line(2));
            --------------------------------------------------------------------
            Char_Utils.V_To_U_And_J_To_I (fc);
            Char_Utils.V_To_U_And_J_To_I (sc);
            --------------------------------------------------------------------
            i := i + 1;

            if sc = ' '  then
               --Put("BDL    I -> "); Put(I       ); New_Line;
               if fc /= ofc  then
                  bdlf(fc, ' ', d_k) := i;
                  --Put(" bf2 BDLF("); Put(Fc);Put(' '); Put(")  ");
                  --Put(Bdlf(Fc, ' ', D_K)); New_Line;
               end if;
            else
               null;
               --Put("I        -> "); Put(I); New_Line;
            end if;

            ds.stem := line(1..Max_Stem_Size);
            Get(line(Max_Stem_Size+1..last), ds.part, ll);
            Get(line(ll+1..last), ds.key , ll);
            Get(line(ll+1..last), ds.MNPC , ll);
            Write(stem_file(d_k), ds);
            --Put_Line("Wrote STEMfile");

            if fc /= ofc   then  --  Jumped FC, effectively must have jumped a SC
               --Put_Line("Jumped FC");
               if osc = ' '  then
                  bdll(ofc, osc, d_k) := i - 1;
               else
                  ddll(ofc, osc, d_k) := i - 1;
               end if;

               if sc = ' '  then
                  --Put("BDLF  "); Put(Bdlf(Fc, Sc, D_K)); New_Line;
                  bdlf(fc, sc, d_k) := i;
               else
                  ddlf(fc, sc, d_k) := i;
               end if;
               --Put_Line("if Sc done");
               --Put("Ofc = '"); Put(Ofc); Put("'   Osc = '"); Put(Osc); Put_Line("'");
               Put_indices(ofc & osc, d_k);
               ofc := fc;
               osc := sc;
               --Put_Line("exit Second_Character_Loop");

               exit second_Character_loop;
            else
               if sc /= osc  then          --  Jumped a SC, but not a FC
                  if osc = ' '  then        --  Jumped a SC from ' ' to something
                     bdll(fc, osc, d_k) := i - 1;            --  So set BDLL
                     --DEBUG.PUT(" bl1 BDLL("); DEBUG.PUT(FC); DEBUG.PUT(OSC); DEBUG.PUT(")  ");
                     --DEBUG.PUT(BDLL(FC, OSC, D_K)); DEBUG.NEW_LINE;
                     ddlf(fc, sc, d_k) := i;
                     --DEBUG.PUT(" df1 DDLF("); DEBUG.PUT( FC); DEBUG.PUT( SC); DEBUG.PUT(")  ");
                     --DEBUG.PUT(DDLF( FC,  SC, D_K)); DEBUG.NEW_LINE;
                     Put_indices(fc & osc, d_k);
                     osc := sc;

                     exit inner_loop;
                  else                      --  Jumped a SL from something, not ' '
                     ddll(fc, osc, d_k) := i - 1;    --  So set DDLL
                     --DEBUG.PUT(" dl2 DDLL("); DEBUG.PUT(FC); DEBUG.PUT(OSC); DEBUG.PUT(")  ");
                     --DEBUG.PUT(DDLL(FC, OSC, D_K)); DEBUG.NEW_LINE;
                     ddlf(fc, sc, d_k) := i;
                     --DEBUG.PUT(" df2 DDLF("); DEBUG.PUT( FC); DEBUG.PUT( SC); DEBUG.PUT(")  ");
                     --DEBUG.PUT(DDLF( FC,  SC, D_K)); DEBUG.NEW_LINE;
                     Put_indices(fc & osc, d_k);
                     osc := sc;

                     exit inner_loop;
                  end if;
               end if;
            end if;

         end loop inner_loop;
         --Put_Line("Exitted Inner_Loop");

      end loop second_Character_loop;
      --Put_Line("Exitted Second_Character_Loop");

   end loop first_Character_loop;
   --Put_Line("Exitted First_Character_Loop");
   ddll(ofc, osc, d_k) := i;

   --  To reprint correctly the last letter information
   --Put_Line("--  To reprint correctly the last letter information");
   Put_indices(ofc & osc, d_k);
   Close(stem_file(d_k));

   for i in Character'('a')..Character'('z')  loop
      for j in Character'(' ')..Character'(' ')  loop
         Text_IO.Put(indx_file(d_k), (i, j));
         Put(indx_file(d_k), ' ');
         Put(indx_file(d_k), bdlf(i, j, d_k));
         Put(indx_file(d_k), ' ');
         Put(indx_file(d_k), bdll(i, j, d_k));
         Put(indx_file(d_k), ' ');
         New_Line(indx_file(d_k));
      end loop;
   end loop;

   for i in Character'('a')..Character'('z')  loop
      for j in Character'('a')..Character'('z')  loop
         Text_IO.Put(indx_file(d_k), (i, j));
         Put(indx_file(d_k), ' ');
         Put(indx_file(d_k), ddlf(i, j, d_k));
         Put(indx_file(d_k), ' ');
         Put(indx_file(d_k), ddll(i, j, d_k));
         Put(indx_file(d_k), ' ');
         New_Line(indx_file(d_k));
      end loop;
   end loop;
   Close(indx_file(d_k));
end makestem;
