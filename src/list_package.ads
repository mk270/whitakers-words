with text_io;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
package list_package is

   --  SCROLL_LINE_NUMBER : INTEGER := 0;
   --  OUTPUT_SCROLL_COUNT : INTEGER := 0;
   --

   procedure list_stems(output   : text_io.file_type;
						raw_word : string;
						input_line : string;
						pa       : in out parse_array;
						pa_last  : in out integer);

   procedure list_entry(output   : text_io.file_type;
						d_k      : dictionary_kind;
						mn       : dict_io.count);

   procedure unknown_search(unknown       :  in string;
							unknown_count : out dict_io.count);

   procedure list_neighborhood(output : text_io.file_type; input_word : string);

end list_package;
