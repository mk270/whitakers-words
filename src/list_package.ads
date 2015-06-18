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

with text_io;
with dictionary_package; use dictionary_package;
with config; use config;
package list_package is

   --  SCROLL_LINE_NUMBER : INTEGER := 0;
   --  OUTPUT_SCROLL_COUNT : INTEGER := 0;
   --

   procedure list_stems(configuration : configuration_type;
                        output   : text_io.file_type;
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
