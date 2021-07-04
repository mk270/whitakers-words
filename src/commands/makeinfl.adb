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
with IO_Exceptions;
with Ada.Exceptions;

procedure Makeinfl is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Stem_Key_Type_IO;
   use Inflection_Record_IO;
   use Quality_Record_IO;
   use Ending_Record_IO;
   use Age_Type_IO;
   use Frequency_Type_IO;
   use Lel_Section_Io;

   Porting : constant Boolean := True;    --FALSE for WAKEINFL;

   M : Integer := 0;
   N1, N2, N3, N4, N5 : Integer := 0;

   Output : Ada.Text_IO.File_Type;
   Inflections_Sections_File : Lel_Section_Io.File_Type;

   procedure File_Inflections_Sections is
      --  Reads the INFLECTS. file and prepares an inflections list
      --  Then it Writes that list into an array
      --  Loads the inflection array into a file for later retrieval
      Inflections_File : Ada.Text_IO.File_Type;
      Inflections_Sections_File : Lel_Section_Io.File_Type;
      Ir : Inflection_Record;
      Line : String (1 .. 100) := (others => ' ');
      Last, L : Integer := 0;
      Sn : Ending_Size_Type := Ending_Size_Type'First;
      Sx : Character := ' ';

      type Inflection_Item;
      type Inflection_List is access Inflection_Item;

      type Inflection_Item is
         record
            Ir   : Inflection_Record;
            Succ : Inflection_List;
         end record;

      type Latin_Inflections is array (Integer range 0 .. Max_Ending_Size,
        Character  range ' ' .. 'z') of Inflection_List;
      Null_Latin_Inflections : constant Latin_Inflections :=
        (others => (others => null));

      L_I : Latin_Inflections := Null_Latin_Inflections;

      Lel : Lel_Section := (others => Null_Inflection_Record);
      J1, J2, J3, J4, J5 : Integer := 0;

      procedure Null_Lel is
      begin
         for I in Lel'Range loop
            Lel (I) := Null_Inflection_Record;
         end loop;
      end Null_Lel;

      procedure Load_Inflections_List is
         --  Takes the INFLECT. file and populates the L_I list of inflections
         --  indexed on ending size and last letter of ending
      begin
         Put_Line ("Begin  LOAD_INFLECTIONS_LIST");
         Number_Of_Inflections := 0;

         L_I := Null_Latin_Inflections;
         Open (Inflections_File, In_File,
               Latin_Utils.Config.Path (Inflections_Full_Name));
         Ada.Text_IO.Put ("INFLECTIONS file loading");
         while not End_Of_File (Inflections_File)  loop
            -- read_a_line :
            begin
               Get_Non_Comment_Line (Inflections_File, Line, Last);

               if Last > 0  then
                  Get (Line (1 .. Last), Ir, L);
                  Sn := Ir.Ending.Size;
                  if Sn = 0  then
                     Sx := ' ';
                  else
                     Sx := Ir.Ending.Suf (Sn);
                  end if;
                  L_I (Sn, Sx) := new Inflection_Item'(Ir, L_I (Sn, Sx));
                  Number_Of_Inflections := Number_Of_Inflections + 1;
               end if;
            exception
               when E : Constraint_Error | IO_Exceptions.Data_Error  =>
                  Put_Line ("");
                  Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                    Line (1 .. Last));
                  raise;
            end;

         end loop;
         Close (Inflections_File);
         Put_Line ("INFLECTIONS_LIST LOADED   "
           & Integer'Image (Number_Of_Inflections));
      end Load_Inflections_List;

      procedure List_To_Lel_File  is
         --  From ILC (=L_I) list of inflections,
         --  prepares the LEL inflections array
         Ilc : Latin_Inflections := L_I;
      begin
         Create (Inflections_Sections_File, Out_File,
           Inflections_Sections_Name);

         Null_Lel;
         Ilc := L_I;                      --  Resetting the list to start over
         while Ilc (0, ' ') /= null  loop
            J5 := J5 + 1;
            Lel (J5) := Ilc (0, ' ').Ir;
            Ilc (0, ' ') := Ilc (0, ' ').Succ;
         end loop;
         Write (Inflections_Sections_File, Lel, 5);
         N5 := J5;

         Null_Lel;
         Ilc := L_I;                      --  Resetting the list to start over
         for Ch in Character range 'a' .. 'z'  loop
            for N in reverse 1 .. Max_Ending_Size  loop
               while Ilc (N, Ch) /= null  loop
                  if   not
                    (Ilc (N, Ch).Ir.Qual.Pofs = Pron  and then
                    (Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 1  or
                    Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 2))
                  then
                     if Ch in Inflections_Section_1  then
                        J1 := J1 + 1;
                        Lel (J1) := Ilc (N, Ch).Ir;
                     end if;
                  end if;
                  Ilc (N, Ch) := Ilc (N, Ch).Succ;
               end loop;
            end loop;
         end loop;
         Write (Inflections_Sections_File, Lel, 1);
         N1 := J1;

         Null_Lel;
         Ilc := L_I;                       --  Resetting the list to start over
         for Ch in Character range 'a' .. 'z'  loop
            for N in reverse 1 .. Max_Ending_Size  loop
               while Ilc (N, Ch) /= null  loop
                  if   not
                    (Ilc (N, Ch).Ir.Qual.Pofs = Pron  and then
                    (Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 1  or
                    Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 2))
                  then
                     if Ch in Inflections_Section_2  then
                        J2 := J2 + 1;
                        Lel (J2) := Ilc (N, Ch).Ir;
                     end if;
                  end if;
                  Ilc (N, Ch) := Ilc (N, Ch).Succ;
               end loop;
            end loop;
         end loop;
         Write (Inflections_Sections_File, Lel, 2);
         N2 := J2;

         Null_Lel;
         Ilc := L_I;                      --  Resetting the list to start over
         for Ch in Character range 'a' .. 'z'  loop
            for N in reverse 1 .. Max_Ending_Size  loop
               while Ilc (N, Ch) /= null  loop
                  if   not
                    (Ilc (N, Ch).Ir.Qual.Pofs = Pron  and then
                    (Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 1  or
                    Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 2))
                  then
                     if Ch in Inflections_Section_3  then
                        J3 := J3 + 1;
                        Lel (J3) := Ilc (N, Ch).Ir;
                     end if;
                  end if;
                  Ilc (N, Ch) := Ilc (N, Ch).Succ;
               end loop;
            end loop;
         end loop;
         Write (Inflections_Sections_File, Lel, 3);
         N3 := J3;

         Null_Lel;
         Ilc := L_I;                      --  Resetting the list to start over
         for Ch in Character range 'a' .. 'z'  loop
            for N in reverse 1 .. Max_Ending_Size  loop
               while Ilc (N, Ch) /= null  loop
                  if   not
                    (Ilc (N, Ch).Ir.Qual.Pofs = Pron  and then
                    (Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 1  or
                    Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 2))
                  then
                     if Ch in Inflections_Section_4 then
                        J4 := J4 + 1;
                        Lel (J4) := Ilc (N, Ch).Ir;
                     end if;
                  end if;
                  Ilc (N, Ch) := Ilc (N, Ch).Succ;
               end loop;
            end loop;
         end loop;

         --  Now Put the PACK in 4       --  Maybe it should be in 5 ????
         Ilc := L_I;                     --  Resetting the list to start over
         for Ch in Character range 'a' .. 'z'  loop
            for N in reverse 1 .. Max_Ending_Size  loop
               while Ilc (N, Ch) /= null  loop
                  if Ilc (N, Ch).Ir.Qual.Pofs = Pron  and then
                    (Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 1  or
                    Ilc (N, Ch).Ir.Qual.Pron.Decl.Which = 2)
                  then  --  2 no longer PACK
                     J4 := J4 + 1;
                     Lel (J4) := Ilc (N, Ch).Ir;
                  end if;
                  Ilc (N, Ch) := Ilc (N, Ch).Succ;
               end loop;
            end loop;
         end loop;
         Write (Inflections_Sections_File, Lel, 4);
         N4 := J4;

         Close (Inflections_Sections_File);
      end List_To_Lel_File;

   begin
      Load_Inflections_List;

      Ada.Text_IO.Set_Col (33);
      Ada.Text_IO.Put ("--  ");
      Put (Number_Of_Inflections);
      Ada.Text_IO.Put_Line (" entries    --  Loaded correctly");

      List_To_Lel_File;                     --  Load arrays to file
      Ada.Text_IO.Put_Line ("File INFLECTS.SEC  --  Loaded");

   exception
      when others =>
         Ada.Text_IO.Put_Line ("Exception in FILE_INFLECTIONS_SECTIONS");
   end File_Inflections_Sections;

begin

   Put_Line ("Produces INFLECTS.SEC file from INFLECTS.");

   File_Inflections_Sections;

   if not Porting  then
      Put_Line
        ("using FILE_INFLECTIONS_SECTIONS, also produces INFLECTS.LIN file");

      Create (Output, Out_File, "INFLECTS.LIN");
   end if;

   Establish_Inflections_Section;

   Lel_Section_Io.Open (Inflections_Sections_File, In_File,
     Latin_Utils.Config.Path
     (Inflections_Sections_Name));

   if not Porting then
      for I in Bel'Range loop                     --  Blank endings
         if  Bel (I) /= Null_Inflection_Record  then
            M := M + 1;
            Put (Output, Bel (I).Qual);
            Set_Col (Output, 50);
            Put (Output, Bel (I).Key, 1);
            Set_Col (Output, 52);
            Put (Output, Bel (I).Ending);
            Set_Col (Output, 62);
            Put (Output, Bel (I).Age);
            Set_Col (Output, 64);
            Put (Output, Bel (I).Freq);
            New_Line (Output);
         end if;
      end loop;
   end if;

   for N in 1 .. 4  loop
      Read (Inflections_Sections_File, Lel, Lel_Section_Io.Positive_Count (N));

      if not Porting then
         for I in Lel'Range loop                     --  Non-blank endings
            if  Lel (I) /= Null_Inflection_Record  then
               M := M + 1;
               Put (Output, Lel (I).Qual);
               Set_Col (Output, 50);
               Put (Output, Lel (I).Key, 1);
               Set_Col (Output, 52);
               Put (Output, Lel (I).Ending);
               Set_Col (Output, 62);
               Put (Output, Lel (I).Age);
               Set_Col (Output, 64);
               Put (Output, Lel (I).Freq);
               New_Line (Output);
            end if;
         end loop;
      end if;

   end loop;

   New_Line;
   Put ("LINE_INFLECTIONS finds "); Put (M); Put_Line (" inflections");
   New_Line;

   for I in Character range ' ' .. ' '  loop
      Ada.Integer_Text_IO.Put (0); Put ("    "); Put (I); Put ("    ");
      Put (Belf (0, I));
      Put ("  ");   Put (Bell (0, I));
      Put ("    "); Put (Bell (0, I) - Belf (0, I) + 1); New_Line;
   end loop;
   New_Line;

   for I in Character range 'a' .. 'z'  loop
      for N in reverse 1 .. Max_Ending_Size  loop
         if (Lell (N, I) > 0)  and then (Lelf (N, I) <= Lell (N, I))  then
            Put (N); Put ("    "); Put (I); Put ("    "); Put (Lelf (N, I));
            Put ("  ");   Put (Lell (N, I));
            Put ("    "); Put (Lell (N, I) - Lelf (N, I) + 1); New_Line;
         end if;
      end loop;
   end loop;
   New_Line;

   for I in Character range 'a' .. 'z'  loop
      for N in reverse 1 .. Max_Ending_Size  loop
         if (Pell (N, I) > 0)  and then (Pelf (N, I) <= Pell (N, I))  then
            Put (N); Put ("    "); Put (I); Put ("    "); Put (Pelf (N, I));
            Put ("  ");   Put (Pell (N, I));
            Put ("    "); Put (Pell (N, I) - Pelf (N, I) + 1); New_Line;
         end if;
      end loop;
   end loop;
   New_Line;

   New_Line;
   Put (N5);  Put ("    ");
   Put (N1);  Put ("    ");
   Put (N2);  Put ("    ");
   Put (N3);  Put ("    ");
   Put (N4);  Put ("    ");
   New_Line;

end Makeinfl;
