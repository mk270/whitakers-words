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
package developer_parameters is

   --  These are a few strange declarations to be used in diagnostics;
   sra_max, sraa_max, dma_max : integer := 0;
   pa_last_max, final_pa_last_max : integer := 0;

   --  This package defines a number of parameters that areused in the program
   --  The default values are set in the body, so that they may be changed easily

   --  These files are used by the program if requested, but not necessary
   --  They are all text files and human readable

   --  DEVELOPER MODE_FILE is used by the program to remember values
   mdev_file : text_io.file_type;
   mdev_full_name : constant string := "WORD.MDV";

   --  Debug not currently in use
   --  --  DBG collects debug output for one entry at a time
   --  DBG : TEXT_IO.FILE_TYPE;
   --  DEBUG_FULL_NAME : constant STRING := "WORD.DBG";

   --  STATS collects statistics on the program, stems used, inflections, etc.
   stats : text_io.file_type;
   stats_full_name : constant string := "WORD.STA";

   type mdev_type is (
     --               HAVE_DEBUG_FILE,      --  No longer in use
     --               WRITE_DEBUG_FILE,

     have_statistics_file,
     write_statistics_file,

     show_dictionary,
     show_dictionary_line,
     show_dictionary_codes,
     do_pearse_codes,

     do_only_initial_word,
     for_word_list_check,

     do_only_fixes,
     do_fixes_anyway,
     use_prefixes,
     use_suffixes,
     use_tackons,

     do_medieval_tricks,

     do_syncope,
     do_two_words,
     include_unknown_context,
     no_meanings,

     omit_archaic,
     omit_medieval,
     omit_uncommon,

     do_i_for_j,
     do_u_for_v,

     pause_in_screen_output,
     no_screen_activity,

     update_local_dictionary,
     update_meanings,

     minimize_output         );

   package mdev_type_io is new text_io.enumeration_io(mdev_type);

   type mdev_array is array (mdev_type) of boolean;

   words_mdev : mdev_array;        --  Initialized in body

   start_file_character               : character := '@';
   change_developer_modes_character   : character := '!';

   procedure change_developer_modes;

   procedure update_local_dictionary_file;

   procedure initialize_developer_parameters;

end developer_parameters;
