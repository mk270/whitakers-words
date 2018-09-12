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

with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Config;
with Latin_Utils.Preface;
use Latin_Utils;
package body Support_Utils.Word_Support_Package is

   function Len (S : String) return Integer is
   begin
      return Trim (S)'Length;
   end Len;

   function Eff_Part (Part : Part_Of_Speech_Type) return Part_Of_Speech_Type is
   begin
      if Part = Vpar   then
         return V;
      elsif Part = Supine  then
         return V;
      else
         return Part;
      end if;
   end Eff_Part;

   function Adj_Comp_From_Key (Key : Stem_Key_Type) return Comparison_Type is
   begin
      case Key is
         when 0 | 1 | 2  => return Pos;
         when 3          => return Comp;
         when 4          => return Super;
         when others     => return X;
      end case;
   end Adj_Comp_From_Key;

   function Adv_Comp_From_Key (Key : Stem_Key_Type) return Comparison_Type is
   begin
      case Key is
         when 1  => return Pos;
         when 2  => return Comp;
         when 3  => return Super;
         when others  => return X;
      end case;
   end Adv_Comp_From_Key;

   function Num_Sort_From_Key (Key : Stem_Key_Type) return Numeral_Sort_Type is
   begin
      case Key is
         when 1  => return Card;
         when 2  => return Ord;
         when 3  => return Dist;
         when 4  => return Adverb;
         when others  => return X;
      end case;
   end Num_Sort_From_Key;

   function First_Index
     (Input_Word : String;
      D_K : Dictionary_File_Kind := Default_Dictionary_File_Kind)
     return Stem_Io.Count is
      Wd : constant String := Trim (Input_Word);  --  String may not start at 1
   begin
      if D_K = Local  then
         return Ddlf (Wd (Wd'First), 'a', D_K);
      elsif Wd'Length < 2 then
         return   0; --  BDLF (WD (WD'FIRST), ' ', D_K);
      else
         return Ddlf (Wd (Wd'First), Wd (Wd'First + 1), D_K);
      end if;
   end First_Index;

   function  Last_Index
     (Input_Word : String;
      D_K : Dictionary_File_Kind := Default_Dictionary_File_Kind)
     return Stem_Io.Count is
      Wd : constant String := Trim (Input_Word);
   begin        --  remember the String may not start at 1
      if D_K = Local  then
         return Ddll (Wd (Wd'First), 'a', D_K);
      elsif Wd'Length < 2 then
         return   0; --  BDLL (WD (WD'FIRST), ' ', D_K);
      else
         return Ddll (Wd (Wd'First), Wd (Wd'First + 1), D_K);
      end if;
   end  Last_Index;

   procedure Load_Bdl_From_Disk is
      use Stem_Io;
      Ds : Dictionary_Stem;
      Index_First,
      Index_Last  : Stem_Io.Count := 0;
      K : Integer := 0;
   begin

      if Dictionary_Available (General)  then
         --  The blanks are on the GENERAL dictionary
         Loading_Bdl_From_Disk :
         declare
            D_K : constant Dictionary_Kind := General;
         begin
            if not Is_Open (Stem_File (D_K))  then
               Open (Stem_File (D_K), Stem_Io.In_File,
                 Add_File_Name_Extension (Stem_File_Name,
                 Dictionary_Kind'Image (D_K)));
            end if;
            Index_First := Bblf (' ', ' ', D_K);
            Index_Last  := Bbll (' ', ' ', D_K);

            Set_Index (Stem_File (D_K), Stem_Io.Positive_Count (Index_First));
            for J in Index_First .. Index_Last  loop
               Read (Stem_File (D_K), Ds);
               K := K + 1;
               Bdl (K) := Ds;
            end loop;
            Close (Stem_File (D_K));
         exception
            when Name_Error =>
               Ada.Text_IO.Put_Line
                 ("LOADING BDL FROM DISK had NAME_ERROR on " &
                 Add_File_Name_Extension (Stem_File_Name,
                 Dictionary_Kind'Image (D_K)));
               Ada.Text_IO.Put_Line ("The will be no blank stems loaded");
            when Use_Error =>
               Ada.Text_IO.Put_Line ("LOADING BDL FROM DISK had USE_ERROR on " &
                 Add_File_Name_Extension (Stem_File_Name,
                 Dictionary_Kind'Image (D_K)));
               Ada.Text_IO.Put_Line ("There will be no blank stems loaded");
         end Loading_Bdl_From_Disk;
      end if;

      --  Now load the stems of just one letter
      for D_K in General .. Dictionary_Kind'Last loop
         if Dictionary_Available (D_K)  then
            exit when D_K = Local;
            --TEXT_IO.PUT_LINE ("OPENING BDL STEMFILE " & EXT (D_K));
            if not Is_Open (Stem_File (D_K))  then
               --PUT_LINE ("LOADING_BDL is going to OPEN " &
               --ADD_FILE_NAME_EXTENSION (STEM_FILE_NAME,
               --DICTIONARY_KIND'IMAGE (D_K)));
               Open (Stem_File (D_K), Stem_Io.In_File,
                 Add_File_Name_Extension (Stem_File_Name,
                 Dictionary_Kind'Image (D_K)));
               --STEMFILE." & EXT (D_K));
               --PUT_LINE ("OPENing was successful");
            end if;
            for I in Character range 'a' .. 'z'  loop
               Index_First := Bdlf (I, ' ', D_K);
               Index_Last  := Bdll (I, ' ', D_K);
               if Index_First > 0  then
                  Set_Index (Stem_File (D_K),
                    Stem_Io.Positive_Count (Index_First));
                  for J in Index_First .. Index_Last  loop
                     Read (Stem_File (D_K), Ds);
                     K := K + 1;
                     Bdl (K) := Ds;
                  end loop;
               end if;
            end loop;
            Close (Stem_File (D_K));
         end if;

      end loop;
      Bdl_Last := K;

      --TEXT_IO.PUT ("FINISHED LOADING BDL FROM DISK     BDL_LAST = ");
      --TEXT_IO.PUT (INTEGER'IMAGE (BDL_LAST));
      --TEXT_IO.NEW_LINE;

   end Load_Bdl_From_Disk;

   procedure Load_Indices_From_Indx_File (D_K : Dictionary_Kind) is
      use Ada.Text_IO;
      use Stem_Io;
      use Count_Io;
      Ch : String (1 .. 2);
      M, N : Stem_Io.Count;
      Number_Of_Blank_Stems,
      Number_Of_Non_Blank_Stems : Stem_Io.Count := 0;
      S : String (1 .. 100) := (others => ' ');
      Last, L : Integer := 0;

   begin
      Open (Indx_File (D_K), Ada.Text_IO.In_File,
        Add_File_Name_Extension (Indx_File_Name,
        Dictionary_Kind'Image (D_K)));
      --"INDXFILE." & EXT (D_K)); --  $$$$$$$$$$$$

      Preface.Put (Dictionary_Kind'Image (D_K));
      Preface.Put (" Dictionary loading");

      if D_K = General  then
         Get_Line (Indx_File (D_K), S, Last);
         Ch := S (1 .. 2);
         Get (S (4 .. Last), M, L);
         Bblf (Ch (1), Ch (2), D_K) := M;
         Get (S (L + 1  .. Last), N, L);
         Bbll (Ch (1), Ch (2), D_K) := N;
         Number_Of_Blank_Stems := Stem_Io.Count'Max (Number_Of_Blank_Stems, N);
      end if;

      while not End_Of_File (Indx_File (D_K))  loop
         Get_Line (Indx_File (D_K), S, Last);
         exit when Last = 0;
         Ch := S (1 .. 2);
         Get (S (4 .. Last), M, L);
         if Ch (2) = ' '  then
            Bdlf (Ch (1), Ch (2), D_K) := M;
         else
            Ddlf (Ch (1), Ch (2), D_K) := M;
         end if;
         Get (S (L + 1 .. Last), N, L);
         if Ch (2) = ' '  then
            Bdll (Ch (1), Ch (2), D_K) := N;
            Number_Of_Blank_Stems := Stem_Io.Count'Max
               (Number_Of_Blank_Stems, N);
         else
            Ddll (Ch (1), Ch (2), D_K) := N;
            Number_Of_Non_Blank_Stems := Stem_Io.Count'Max
               (Number_Of_Non_Blank_Stems, N);
         end if;
      end loop;
      Close (Indx_File (D_K));
      Preface.Set_Col (33); Preface.Put ("--  ");
      if not Config.Suppress_Preface  then
         Put (Number_Of_Non_Blank_Stems, 6);
      end if;   --  Kludge for when TEXT_IO.COUNT too small
      Preface.Put (" stems");
      Preface.Set_Col (55); Preface.Put_Line ("--  Loaded correctly");
   end Load_Indices_From_Indx_File;

end Support_Utils.Word_Support_Package;
