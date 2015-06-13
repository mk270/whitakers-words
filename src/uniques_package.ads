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
