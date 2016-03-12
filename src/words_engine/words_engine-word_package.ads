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
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
use Latin_Utils;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
package Words_Engine.Word_Package is

   Line_Number, Word_Number : Integer := 0;

   type Stem_Array_Type is array (Integer range <>) of Stem_Type;
   subtype Stem_Array is Stem_Array_Type (0 .. Max_Stem_Size);

   Not_A_Stem : constant Stem_Type := (others => 'x');
   Not_A_Stem_Array : Stem_Array  := (others => Not_A_Stem);

   Sa, Ssa : Stem_Array := Not_A_Stem_Array;
   Ssa_Max : Integer := 0;

   type Pruned_Dictionary_Item is
      record
         Ds   : Dictionary_Stem;
         D_K  : Dictionary_Kind := Default_Dictionary_Kind;
      end record;
   Null_Pruned_Dictionary_Item : Pruned_Dictionary_Item;
   type Pruned_Dictionary_List is array (1 .. 80) of Pruned_Dictionary_Item;
   --  Aug 96   QU_PRON max 42, PACK max 54
   --  Jan 97   QU_PRON max 42, PACK max 74  --  Might reduce

   Pdl : Pruned_Dictionary_List := (others => Null_Pruned_Dictionary_Item);
   Pdl_Index : Integer := 0;

   subtype Sal is Parse_Array (1 .. 250);

   type Dict_Restriction is (X, Regular, Qu_Pron_Only, Pack_Only);

   Scroll_Line_Number : Integer := 0;
   Output_Scroll_Count : Integer := 0;

   procedure Pause (Output : Ada.Text_IO.File_Type);

   function Ltu (C, D : Character) return Boolean;

   function Equ (C, D : Character) return Boolean;

   function Gtu (C, D : Character) return Boolean;

   function Ltu (S, T : String) return Boolean;

   function Gtu (S, T : String) return Boolean;

   function Equ (S, T : String) return Boolean;

   procedure Run_Inflections
     (S : in String;
      Sl : in out Sal;
      Restriction : Dict_Restriction := Regular);

   procedure Search_Dictionaries (Ssa : in Stem_Array_Type;
                                  Restriction : Dict_Restriction := Regular);

   procedure Word (Raw_Word : in String;
                   Pa : in out Parse_Array; Pa_Last : in out Integer);

   procedure Change_Language (C : Character);

   procedure Initialize_Word_Package;

end Words_Engine.Word_Package;
