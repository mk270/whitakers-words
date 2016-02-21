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
with Ada.Direct_IO;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
package Support_Utils.Word_Support_Package is

   Followed_By_Period, Capitalized, All_Caps : Boolean := False;

   type Dictionary_Stem is
      record
         Stem : Stem_Type := Null_Stem_Type;
         Part : Part_Entry := Null_Part_Entry;
         Key  : Stem_Key_Type := 0;
         MNPC : Dict_IO.Count := Null_MNPC;
      end record;

   package Stem_Io is new Ada.Direct_IO (Dictionary_Stem);
   package Count_Io is new Ada.Text_IO.Integer_IO (Stem_Io.Count);

   subtype Dictionary_File_Kind is Dictionary_Kind range General .. Local;
   Default_Dictionary_File_Kind : Dictionary_File_Kind := General;

   Stem_File : array (Dictionary_File_Kind) of Stem_Io.File_Type;

   Stem_List : array (Dictionary_File_Kind) of Ada.Text_IO.File_Type;
   Indx_File : array (Dictionary_File_Kind) of Ada.Text_IO.File_Type;

   type Dict_Array is array (Positive range <>) of Dictionary_Stem;
   Bdl : Dict_Array (1 .. 100);
   Bdl_Last : Integer := 0;
   --SIZE_OF_DICTIONARY_ARRAY : constant INTEGER := 120;
   --DDL : DICT_ARRAY (1 .. SIZE_OF_DICTIONARY_ARRAY);
   type Dict_Array_Index is array (Character range <>,
     Character range <>,
     Dictionary_File_Kind range <>) of Stem_Io.Count;

   Bblf, Bbll :
     Dict_Array_Index (' ' .. ' ', ' ' .. ' ', Dictionary_File_Kind) :=
     (others => (others => (others => 0)));

   Bdlf, Bdll :
     Dict_Array_Index ('a' .. 'z', ' ' .. ' ', Dictionary_File_Kind) :=
     (others => (others => (others => 0)));

   Ddlf, Ddll :
     Dict_Array_Index ('a' .. 'z', 'a' .. 'z', Dictionary_File_Kind) :=
     (others => (others => (others => 0)));

   function Adj_Comp_From_Key (Key : Stem_Key_Type) return Comparison_Type;

   function Adv_Comp_From_Key (Key : Stem_Key_Type) return Comparison_Type;

   function Num_Sort_From_Key (Key : Stem_Key_Type) return Numeral_Sort_Type;

   function Eff_Part (Part : Part_Of_Speech_Type) return Part_Of_Speech_Type;

   function Len (S : String) return Integer;

   function First_Index
     (Input_Word : String;
      D_K : Dictionary_File_Kind := Default_Dictionary_File_Kind)
     return Stem_Io.Count;

   function  Last_Index
     (Input_Word : String;
      D_K : Dictionary_File_Kind := Default_Dictionary_File_Kind)
     return Stem_Io.Count;

   procedure Load_Indices_From_Indx_File (D_K : Dictionary_Kind);

   procedure Load_Bdl_From_Disk;

end Support_Utils.Word_Support_Package;
