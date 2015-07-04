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
with Direct_IO;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
package word_support_package is

   followed_by_period, follows_period, capitalized, all_caps :
     Boolean := False;

   type dictionary_stem is
      record
         stem : Stem_Type := Null_Stem_Type;
         part : part_entry := null_part_entry;
         key  : stem_key_type := 0;
         MNPC : Dict_IO.Count := Null_MNPC;
      end record;

   package stem_io is new direct_io(dictionary_stem);
   package Count_io is new Text_IO.Integer_IO(stem_io.Count);

   subtype dictionary_file_kind is Dictionary_Kind range general..local;
   default_dictionary_file_kind : dictionary_file_kind := general;

   stem_file : array (dictionary_file_kind) of stem_io.File_Type;

   stem_list : array (dictionary_file_kind) of Text_IO.File_Type;
   indx_file : array (dictionary_file_kind) of Text_IO.File_Type;

   type dict_array is array (Positive range <>) of dictionary_stem;
   bdl : dict_array(1..100);
   bdl_last : Integer := 0;
   --SIZE_OF_DICTIONARY_ARRAY : constant INTEGER := 120;    --  ###################
   --DDL : DICT_ARRAY(1..SIZE_OF_DICTIONARY_ARRAY);
   type dict_array_index is array (Character range <>,
                                   Character range <>,
                                   dictionary_file_kind range <>) of stem_io.Count;

   bblf, bbll : dict_array_index(' '..' ', ' '..' ', dictionary_file_kind) :=
     (others => (others => (others => 0)));
   bdlf, bdll : dict_array_index('a'..'z', ' '..' ', dictionary_file_kind) :=
     (others => (others => (others => 0)));
   ddlf, ddll : dict_array_index('a'..'z', 'a'..'z', dictionary_file_kind) :=
     (others => (others => (others => 0)));

   function adj_comp_from_key(key : stem_key_type) return Comparison_Type;

   function adv_comp_from_key(key : stem_key_type) return Comparison_Type;

   function num_sort_from_key(key : stem_key_type) return Numeral_Sort_Type;

   function eff_part(part : part_of_speech_type) return part_of_speech_type;

   function len(s : String) return Integer;

   function first_index(Input_word : String;
                        d_k : dictionary_file_kind := default_dictionary_file_kind)
                       return stem_io.Count;

   function  last_index(Input_word : String;
                        d_k : dictionary_file_kind := default_dictionary_file_kind)
                       return stem_io.Count;

   procedure load_indices_from_indx_file(d_k : Dictionary_Kind);

   procedure load_bdl_from_disk;

end word_support_package;
