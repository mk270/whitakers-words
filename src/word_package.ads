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
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with word_support_package; use word_support_package;
package word_package is

   line_number, word_number : Integer := 0;

   type stem_array_type is array (Integer range <>) of stem_type;
   subtype stem_array is stem_array_type(0..max_stem_size);

   not_a_stem : constant stem_type := (others => 'x');
   not_a_stem_array : stem_array  := (others => not_a_stem);

   sa, ssa : stem_array := not_a_stem_array;
   ssa_max : Integer := 0;

   type pruned_dictionary_item is
      record
         ds   : dictionary_stem;
         d_k  : dictionary_kind := default_dictionary_kind;
      end record;
   null_pruned_dictionary_item : pruned_dictionary_item;
   type pruned_dictionary_list is array (1..80) of pruned_dictionary_item;
   --  Aug 96   QU_PRON max 42, PACK max 54
   --  Jan 97   QU_PRON max 42, PACK max 74  --  Might reduce

   pdl : pruned_dictionary_list := (others => null_pruned_dictionary_item);
   pdl_index : Integer := 0;

   subtype sal is parse_array(1..250);

   type dict_restriction is (x, regular, qu_pron_only, pack_only);

   xxx_meaning : meaning_type := null_meaning_type;  --  For TRICKS
   yyy_meaning : meaning_type := null_meaning_type;  --  For SYNCOPE
   nnn_meaning : meaning_type := null_meaning_type;  --  For Names
   rrr_meaning : meaning_type := null_meaning_type;  --  For Roman Numerals
   ppp_meaning : meaning_type := null_meaning_type;  --  For COMPOUNDED

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
                  pa : in out parse_array; pa_last : in out Integer);

   procedure change_language(c : Character);

   procedure initialize_word_package;

end word_package;
