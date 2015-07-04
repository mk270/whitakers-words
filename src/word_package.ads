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
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
with word_support_package; use word_support_package;
package word_package is

   line_number, word_number : Integer := 0;

   type stem_array_type is array (Integer range <>) of Stem_Type;
   subtype stem_array is stem_array_type(0..Max_Stem_Size);

   not_a_stem : constant Stem_Type := (others => 'x');
   not_a_stem_array : stem_array  := (others => not_a_stem);

   sa, ssa : stem_array := not_a_stem_array;
   ssa_max : Integer := 0;

   type pruned_dictionary_item is
      record
         ds   : dictionary_stem;
         d_k  : Dictionary_Kind := Default_Dictionary_Kind;
      end record;
   null_pruned_dictionary_item : pruned_dictionary_item;
   type pruned_dictionary_list is array (1..80) of pruned_dictionary_item;
   --  Aug 96   QU_PRON max 42, PACK max 54
   --  Jan 97   QU_PRON max 42, PACK max 74  --  Might reduce

   pdl : pruned_dictionary_list := (others => null_pruned_dictionary_item);
   pdl_index : Integer := 0;

   subtype sal is Parse_Array(1..250);

   type dict_restriction is (x, regular, qu_pron_only, pack_only);

   xxx_meaning : Meaning_Type := Null_Meaning_Type;  --  For TRICKS
   yyy_meaning : Meaning_Type := Null_Meaning_Type;  --  For SYNCOPE
   nnn_meaning : Meaning_Type := Null_Meaning_Type;  --  For Names
   rrr_meaning : Meaning_Type := Null_Meaning_Type;  --  For Roman Numerals
   ppp_meaning : Meaning_Type := Null_Meaning_Type;  --  For COMPOUNDED

   scroll_line_number : Integer := 0;
   Output_scroll_count : Integer := 0;

   procedure pause(Output : Text_IO.File_Type);

   function min(a, b : Integer) return Integer;

   function ltu(c, d : Character) return Boolean;

   function equ(c, d : Character) return Boolean;

   function gtu(c, d : Character) return Boolean;

   function ltu(s, t : String) return Boolean;

   function gtu(s, t : String) return Boolean;

   function equ(s, t : String) return Boolean;

   procedure run_inflections(s : in String; sl : in out sal;
                                            restriction : dict_restriction := regular);

   procedure search_dictionaries(ssa : in stem_array_type;
                                                       restriction : dict_restriction := regular);

   procedure word(raw_word : in String;
                  pa : in out Parse_Array; pa_last : in out Integer);

   procedure change_language(c : Character);

   procedure initialize_word_package;

end word_package;
