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
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Latin_Utils.Config; use Latin_Utils.Config;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Words_Engine.Explanation_Package; use Words_Engine.Explanation_Package;

package Words_Engine.List_Package is

   --  SCROLL_LINE_NUMBER : INTEGER := 0;
   --  OUTPUT_SCROLL_COUNT : INTEGER := 0;
   --

   type Word_Analysis is private;

   function Analyse_Word (Pa       : Parse_Array;
                          Pa_Last  : Integer;
                          Raw_Word : String;
                          Xp       : Explanations)
                         return Word_Analysis;

   procedure List_Stems (Configuration : Configuration_Type;
                         Output        : Ada.Text_IO.File_Type;
                         WA            : Word_Analysis;
                         Input_Line    : String);

   procedure Unknown_Search (Unknown       :  in String;
                             Unknown_Count : out Dict_IO.Count);

   procedure List_Neighborhood
     (Output     : Ada.Text_IO.File_Type;
      Input_Word : String);

private
   type Stem_Inflection_Record is
      record
         Stem : Stem_Type          := Null_Stem_Type;
         Ir   : Inflection_Record  := Null_Inflection_Record;
      end record;

   Stem_Inflection_Array_Size       : constant := 12;
   Stem_Inflection_Array_Array_Size : constant := 40;

   type Stem_Inflection_Array is
     array (Integer range <>) of Stem_Inflection_Record;
   type Stem_Inflection_Array_Array is array (Integer range <>)
     of Stem_Inflection_Array (1 .. Stem_Inflection_Array_Size);

   type Dictionary_MNPC_Record is record
      D_K  : Dictionary_Kind := Default_Dictionary_Kind;
      MNPC : MNPC_Type := Null_MNPC;
      De   : Dictionary_Entry := Null_Dictionary_Entry;
   end record;

   Dictionary_MNPC_Array_Size : constant := 40;

   type Dictionary_MNPC_Array is array (1 .. Dictionary_MNPC_Array_Size)
     of Dictionary_MNPC_Record;

   type Word_Analysis is
      record
         Stem_IAA : Stem_Inflection_Array_Array
           (1 .. Stem_Inflection_Array_Array_Size);
         Dict : Dictionary_MNPC_Array;
         I_Is_Pa_Last : Boolean;
         Unknowns : Boolean;
         The_Word : Unbounded_String;
         Was_Trimmed : Boolean;
         Xp : Explanations;
      end record;

end Words_Engine.List_Package;
