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

with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
package uniques_package is

   type unique_item;
   type unique_list is access unique_item;

   type unique_item is
      record
         stem : stem_type      := null_stem_type;
         qual : quality_record := null_quality_record;
         kind : kind_entry     := null_kind_entry;
         mnpc : dict_io.count  := null_mnpc;
         succ : unique_list;
      end record;

   type latin_uniques is array (character range 'a'..'z') of unique_list;
   null_latin_uniques : latin_uniques := (others => null);

   unq : latin_uniques := null_latin_uniques;

   type uniques_de_array is array (dict_io.positive_count range <>) of dictionary_entry;
   uniques_de : uniques_de_array(1..100) := (others => null_dictionary_entry);

end uniques_package;
