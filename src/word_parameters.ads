with text_io;
package word_parameters is
   --  This package defines a number of parameters that areused in the program
   --  The default values are set in the body, so that they may be changed easily

   change_parameters_character        : character := '#';
   change_language_character          : character := '~';
   help_character                     : character := '?';

   --  These files are used by the program if requested, but not necessary
   --  They are all text files and human readable

   --  MODE_FILE is used by the program to remember MODE values between runs
   mode_file : text_io.file_type;

   --  OUTPUT is used to write out and save the results of a run
   output : text_io.file_type;
   input  : text_io.file_type;
   --  UNKNOWNS is used to record the words that the program fails to find
   unknowns : text_io.file_type;

   --  This is a flag to tell if there has been trimming for this word
   trimmed : boolean := false;

   type mode_type is (
							trim_output,

							have_output_file,
							write_output_to_file,

							do_unknowns_only,
							write_unknowns_to_file,

							ignore_unknown_names,
							ignore_unknown_caps,
							do_compounds,
							do_fixes,
							do_tricks,

							do_dictionary_forms,
							show_age,
							show_frequency,

							do_examples,
							do_only_meanings,
							do_stems_for_unknown       );

   package mode_type_io is new text_io.enumeration_io(mode_type);

   type mode_array is array (mode_type) of boolean;

   words_mode : mode_array;        --  Initialized in body

   procedure change_parameters;

   procedure initialize_word_parameters;

end word_parameters;
