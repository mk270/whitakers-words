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

with text_io; use text_io;
with strings_package; use strings_package;
with latin_file_names; use latin_file_names;   --  Omit when put name here
with word_parameters; use word_parameters;
with dictionary_package; use dictionary_package;
with preface;
with line_stuff; use line_stuff;
pragma elaborate(preface);
package body developer_parameters is

   type help_type is array (natural range <>) of string(1..70);
   blank_help_line : constant string(1..70) := (others => ' ');
   no_help : constant help_type := (2..1 => blank_help_line);

   type reply_type is (n, y);
   package reply_type_io is new text_io.enumeration_io(reply_type);
   reply : constant array (boolean) of reply_type := (n, y);
   mdev_of_reply : constant array (reply_type) of boolean := (false, true);

   blank_input : exception;

   --  The default MDEVs are set in the body so that they can be changed
   --  with only this being recompiled, not the rest of the with'ing system
   default_mdev_array : constant mdev_array := (

     --               HAVE_DEBUG_FILE             => FALSE,
     --               WRITE_DEBUG_FILE            => FALSE,

     have_statistics_file        => false,
     write_statistics_file       => false,

     show_dictionary             => false,
     show_dictionary_line        => false,
     show_dictionary_codes       => true,
     do_pearse_codes             => false,

     do_only_initial_word        => false,
     for_word_list_check         => false,

     do_only_fixes               => false,
     do_fixes_anyway             => false,
     use_prefixes                => true,
     use_suffixes                => true,
     use_tackons                 => true,

     do_medieval_tricks          => true,

     do_syncope                  => true,
     do_two_words                => true,
     include_unknown_context     => true,
     no_meanings                 => false,

     omit_archaic                => true,
     omit_medieval               => false,
     omit_uncommon               => true,

     do_i_for_j                  => false,
     do_u_for_v                  => false,

     pause_in_screen_output      => true,
     no_screen_activity          => false,

     update_local_dictionary     => false,
     update_meanings             => false,

     minimize_output             => true    );

   bad_mdev_file : exception;

   --HAVE_DEBUG_FILE_HELP : constant HELP_TYPE :=  (
   --   "This option instructs the program to create a file which can hold     ",
   --   "certain internal information about the current search.  The file is   ",
   --   "overwritten for every word in order to prevent it from growing out of ",
   --   "hand, so information about the last word searched is saved in case of ",
   --   "failure.  The debug output file is named " & DEBUG_FULL_NAME
   --                                   & (42+DEBUG_FULL_NAME'LENGTH..70 => ' '),
   --   "Use of this option, along with the WRITE_DEBUG_FILE option may slow   ",
   --   "the program significantly.  This information is usually only useful   ",
   --   "to the developer, so the default is N(o).                             " );
   --
   --WRITE_DEBUG_FILE_HELP : constant HELP_TYPE :=  (
   --   "This option instructs the program, when HAVE_DEBUG_FILE is on, to put ",
   --   "some debug data to a file named " & DEBUG_FULL_NAME
   --                                   & (33+DEBUG_FULL_NAME'LENGTH..70 => ' '),
   --   "This option may be turned on and off while running of the program,    ",
   --   "thereby capturing only certain desired results.  The file is reset and",
   --   "restarted after each word parsed, so that it does not get too big.    ",
   --   "If the option HAVE_DEBUG_FILE is off, the user will not be given a    ",
   --   "chance to turn this one on.                  Default is N(o).         " );
   --

   have_statistics_file_help : constant help_type :=  (
     "This option instructs the program to create a file which can hold     ",
     "certain statistical information about the process.  The file is       ",
     "overwritten for new invocation of the program, so old data must be    ",
     "explicitly saved if it is to be retained.  The statistics are in TEXT ",
     "format.     The statistics file is named " & stats_full_name
     & (42+stats_full_name'length..70 => ' '),
     "This information is only of development use, so the default is N(o).  " );

   write_statistics_file_help : constant help_type :=  (
     "This option instructs the program, with HAVE_STATISTICS_FILE, to put  ",
     "derived statistics in a file named " & stats_full_name
     & (36+stats_full_name'length..70 => ' '),
     "This option may be turned on and off while running of the program,    ",
     "thereby capturing only certain desired results.  The file is reset at ",
     "each invocation of the program, if the HAVE_STATISTICS_FILE is set.   ",
     "If the option HAVE_STATISTICS_FILE is off, the user will not be given ",
     "a chance to turn this one on.                Default is N(o).         " );

   show_dictionary_help : constant help_type :=  (
     "This option causes a flag, like 'GEN>' to be put before the meaning   ",
     "in the output.  While this is useful for certain development purposes,",
     "it forces off a few characters from the meaning, and is really of no  ",
     "interest to most users.                                               ",
     "The default choice is N(o), but it can be turned on with a Y(es).     " );

   show_dictionary_line_help : constant help_type :=  (
     "This option causes the number of the dictionary line for the current  ",
     "meaning to be output.  This is of use to no one but the dictionary    ",
     "maintainer.  The default choice is N(o).  It is activated by Y(es).   ");

   show_dictionary_codes_help : constant help_type :=  (
     "This option causes the codes for the dictionary entry for the current ",
     "meaning to be output.  This may not be useful to any but the most     ",
     "involved user.  The default choice is N(o).  It is activated by Y(es).");

   do_pearse_codes_help : constant help_type :=  (
     "This option causes special codes to be output flagging the different  ",
     "kinds of output lines.  01 for forms, 02 for dictionary forms, and    ",
     "03 for meaning. The default choice is N(o).  It is activated by Y(es).",
     "There are no Pearse codes in English mode.                            ");

   do_only_initial_word_help : constant help_type :=  (
     "This option instructs the program to only analyze the initial word on ",
     "each line submitted.  This is a tool for checking and integrating new ",
     "dictionary input, and will be of no interest to the general user.     ",
     "The default choice is N(o), but it can be turned on with a Y(es).     " );

   for_word_list_check_help : constant help_type :=  (
     "This option works in conjunction with DO_ONLY_INITIAL_WORD to allow   ",
     "the processing of scanned dictionarys or text word lists.  It accepts ",
     "only the forms common in dictionary entries, like NOM S for N or ADJ, ",
     "or PRES ACTIVE IND 1 S for V.  It is be used only with DO_INITIAL_WORD",
     "The default choice is N(o), but it can be turned on with a Y(es).     " );

   do_only_fixes_help : constant help_type :=  (
     "This option instructs the program to ignore the normal dictionary     ",
     "search and to go direct to attach various prefixes and suffixes before",
     "processing. This is a pure research tool.  It allows one to examine   ",
     "the coverage of pure stems and dictionary primary compositions.       ",
     "This option is only available if DO_FIXES is turned on.               ",
     "This is entirely a development and research tool, not to be used in   ",
     "conventional translation situations, so the default choice is N(o).   ",
     "This processing can be turned on with the choice of Y(es).            " );

   do_fixes_anyway_help : constant help_type :=  (
     "This option instructs the program to do both the normal dictionary    ",
     "search and then process for the various prefixes and suffixes too.    ",
     "This is a pure research tool allowing one to consider the possibility ",
     "of strange constructions, even in the presence of conventional        ",
     "results, e.g., alte => deeply (ADV), but al+t+e => wing+ed (ADJ VOC)  ",
     "(If multiple suffixes were supported this could also be wing+ed+ly.)  ",
     "This option is only available if DO_FIXES is turned on.               ",
     "This is entirely a development and research tool, not to be used in   ",
     "conventional translation situations, so the default choice is N(o).   ",
     "This processing can be turned on with the choice of Y(es).            ",
     "      ------    PRESENTLY NOT IMPLEMENTED    ------                   " );

   use_prefixes_help : constant help_type :=  (
     "This option instructs the program to implement prefixes from ADDONS   ",
     "whenever and wherever FIXES are called for.  The purpose of this      ",
     "option is to allow some flexibility while the program in running to   ",
     "select various combinations of fixes, to turn them on and off,        ",
     "individually as well as collectively.  This is an option usually      ",
     "employed by the developer while experimenting with the ADDONS file.   ",
     "This option is only effective in connection with DO_FIXES.            ",
     "This is primarily a development tool, so the conventional user should ",
     "probably maintain the default  choice of Y(es).                       " );

   use_suffixes_help : constant help_type :=  (
     "This option instructs the program to implement suffixes from ADDONS   ",
     "whenever and wherever FIXES are called for.  The purpose of this      ",
     "option is to allow some flexibility while the program in running to   ",
     "select various combinations of fixes, to turn them on and off,        ",
     "individually as well as collectively.  This is an option usually      ",
     "employed by the developer while experimenting with the ADDONS file.   ",
     "This option is only effective in connection with DO_FIXES.            ",
     "This is primarily a development tool, so the conventional user should ",
     "probably maintain the default  choice of Y(es).                       " );

   use_tackons_help : constant help_type :=  (
     "This option instructs the program to implement TACKONS from ADDONS    ",
     "whenever and wherever FIXES are called for.  The purpose of this      ",
     "option is to allow some flexibility while the program in running to   ",
     "select various combinations of fixes, to turn them on and off,        ",
     "individually as well as collectively.  This is an option usually      ",
     "employed by the developer while experimenting with the ADDONS file.   ",
     "This option is only effective in connection with DO_FIXES.            ",
     "This is primarily a development tool, so the conventional user should ",
     "probably maintain the default  choice of Y(es).                       " );

   do_medieval_tricks_help : constant help_type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, and after various prefixes and suffixes, and ",
     "tring every Classical Latin trick it can think of, to go to a few that",
     "are usually only found in medieval Latin, replacements of caul -> col,",
     "st -> est, z -> di, ix -> is, nct -> nt.  It also tries some things   ",
     "like replacing doubled consonants in classical with a single one.     ",
     "Together these tricks are useful, but may give false positives (>20%).",
     "This option is only available if the general DO_TRICKS is chosen.     ",
     "If the text is late or medieval, this option is much more useful than ",
     "tricks for classical.  The dictionary can never contain all spelling  ",
     "variations found in medieval Latin, but some constructs are common.   ",
     "The default choice is N(o), since the results are iffy, medieval only,",
     "and expensive.  This processing is turned on with the choice of Y(es)." );

   do_syncope_help : constant help_type :=  (
     "This option instructs the program to postulate that syncope of        ",
     "perfect stem verbs may have occured (e.g, aver -> ar in the perfect), ",
     "and to try various possibilities for the insertion of a removed 'v'.  ",
     "To do this it has to fully process the modified candidates, which can ",
     "have a consderable impact on the speed of processind a large file.    ",
     "However, this trick seldom producesa false positive, and syncope is   ",
     "very common in Latin (first year texts excepted).  Default is Y(es).  ",
     "This processing is turned off with the choice of N(o).                " );

   do_two_words_help : constant help_type :=  (
     "There are some few common Lain expressions that combine two inflected ",
     "words (e.g. respublica, paterfamilias).  There are numerous examples  ",
     "of numbers composed of two words combined together.                   ",
     "Sometimes a text or inscription will have words run together.         ",
     "When WORDS is unable to reach a satisfactory solution with all other  ",
     "tricks, as a last stab it will try to break the input into two words. ",
     "This most often fails.  Even if mechnically successful, the result is ",
     "usually false and must be examined by the user.  If the result is     ",
     "correct, it is probably clear to the user.  Otherwise,  beware.  .    ",
     "Since this is a last chanceand infrequent, the default is Y(es);      ",
     "This processing is turned off with the choice of N(o).                " );

   include_unknown_context_help : constant help_type :=  (
     "This option instructs the program, when writing to an UNKNOWNS file,  ",
     "to put out the whole context of the UNKNOWN (the whole input line on  ",
     "which the UNKNOWN was found).  This is appropriate for processing     ",
     "large text files in which it is expected that there will be relatively",
     "few UNKNOWNS.    The main use at the moment is to provide display     ",
     "of the input line on the output file in the case of UNKNOWNS_ONLY.    ");

   no_meanings_help : constant help_type :=  (
     "This option instructs the program to omit putting out meanings.       ",
     "This is only useful for certain dictionary maintenance procedures.    ",
     "The combination not DO_DICTIONARY_FORMS, MEANINGS_ONLY, NO_MEANINGS   ",
     "results in no visible output, except spacing lines.    Default is N)o.");

   omit_archaic_help : constant help_type :=  (
     "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
     "This option instructs the program to omit inflections and dictionary  ",
     "entries with an AGE code of A (Archaic).  Archaic results are rarely  ",
     "of interest in general use.  If there is no other possible form, then ",
     "the Archaic (roughly defined) will be reported.  The default is Y(es)." );

   omit_medieval_help : constant help_type :=  (
     "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
     "This option instructs the program to omit inflections and dictionary  ",
     "entries with AGE codes of E or later, those not in use in Roman times.",
     "While later forms and words are a significant application, most users ",
     "will not want them.  If there is no other possible form, then the     ",
     "Medieval (roughly defined) will be reported.   The default is Y(es).  " );

   omit_uncommon_help : constant help_type :=  (
     "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
     "This option instructs the program to omit inflections and dictionary  ",
     "entries with FREQ codes indicating that the selection is uncommon.    ",
     "While these forms area significant feature of the program, many users ",
     "will not want them.  If there is no other possible form, then the     ",
     "uncommon (roughly defined) will be reported.   The default is Y(es).  " );

   do_i_for_j_help : constant help_type :=  (
     "This option instructs the program to modify the output so that the j/J",
     "is represented as i/I.  The consonant i was writen as j in cursive in ",
     "Imperial times and called i longa, and often rendered as j in medieval",
     "times.  The capital is usually rendered as I, as in inscriptions.     ",
     "If this is NO/FALSE, the output will have the same character as input.",
     "The program default, and the dictionary convention is to retain the j.",
     "Reset if this ia unsuitable for your application. The default is N(o)." );

   do_u_for_v_help : constant help_type :=  (
     "This option instructs the program to modify the output so that the u  ",
     "is represented as v.  The consonant u was writen sometimes as uu.     ",
     "The pronounciation was as current w, and important for poetic meter.  ",
     "With the printing press came the practice of distinguishing consonant ",
     "u with the character v, and was common for centuries.  The practice of",
     "using only u has been adopted in some 20th century publications (OLD),",
     " but it is confusing to many modern readers.  The capital is commonly ",
     "V in any case, as it was and is in inscriptions (easier to chisel).   ",
     "If this is NO/FALSE, the output will have the same character as input.",
     "The program default, and the dictionary convention is to retain the v.",
     "Reset If this ia unsuitable for your application. The default is N(o)." );

   pause_in_screen_output_help : constant help_type :=  (
     "This option instructs the program to pause in output on the screen    ",
     "after about 16 lines so that the user can read the output, otherwise  ",
     "it would just scroll off the top.  A RETURN/ENTER gives another page. ",
     "If the program is waiting for a return, it cannot take other input.   ",
     "This option is active only for keyboard entry or command line input,  ",
     "and only when there is no output file.  It is moot if only single word",
     "input or brief output.                 The default is Y(es).          " );

   no_screen_activity_help : constant help_type :=  (
     "This option instructs the program not to keep a running screen of the ",
     "input.  This is probably only to be used by the developer to calibrate",
     "run times for large text file input, removing the time necessary to   ",
     "write to screen.                       The default is N(o).           ");

   update_local_dictionary_help : constant help_type :=  (
     "This option instructs the program to invite the user to input a new   ",
     "word to the local dictionary on the fly.  This is only active if the  ",
     "program is not using an (@) input file!  If an UNKNOWN is discovered, ",
     "the program asks for STEM, PART, and MEAN, the basic elements of a    ",
     "dictionary entry.  These are put into the local dictionary right then,",
     "and are available for the rest of the session, and all later sessions.",
     "The use of this option requires a detailed knowledge of the structure ",
     "of dictionary entries, and is not for the average user.  If the entry ",
     "is not valid, reloading the dictionary will raise and exception, and  ",
     "the invalid entry will be rejected, but the program will continue     ",
     "without that word.  Any invalid entries can be corrected or deleted   ",
     "off-line with a text editor on the local dictionary file.  If one does",
     "not want to enter a word when this option is on, a simple RETURN at   ",
     "the STEM=> prompt will ignore and continue the program.  This option  ",
     "is only for very experienced users and should normally be off.        ",
     "                                          The default is N(o).        ",
     "      ------    NOT AVAILABLE IN THIS VERSION   -------               " );

   update_meanings_help : constant help_type :=  (
     "This option instructs the program to invite the user to modify the    ",
     "meaning displayed on a word translation.  This is only active if the  ",
     "program is not using an (@) input file!  These changes are put into   ",
     "the dictionary right then and permenently, and are available from     ",
     "then on, in this session, and all later sessions.   Unfortunately,    ",
     "these changes will not survive the replacement of the dictionary by a ",
     "new version from the developer.  Changes can only be recovered by     ",
     "considerable prcessing by the deneloper, and should be left there.    ",
     "This option is only for experienced users and should remain off.      ",
     "                                          The default is N(o).        ",
     "      ------    NOT AVAILABLE IN THIS VERSION   -------               " );

   minimize_output_help : constant help_type :=  (
     "This option instructs the program to minimize the output.  This is a  ",
     "somewhat flexible term, but the use of this option will probably lead ",
     "to less output.                        The default is Y(es).          " );

   save_parameters_help : constant help_type :=  (
     "This option instructs the program, to save the current parameters, as ",
     "just established by the user, in a file WORD.MDV.  If such a file     ",
     "exists, the program will load those parameters at the start.  If no   ",
     "such file can be found in the current subdirectory, the program will  ",
     "start with a default set of parameters.  Since this parameter file is ",
     "human-readable ASCII, it may also be created with a text editor.  If  ",
     "the file found has been improperly created, is in the wrong format, or",
     "otherwise uninterpretable by the program, it will be ignored and the  ",
     "default parameters used, until a proper parameter file in written by  ",
     "the program.  Since one may want to make temporary changes during a   ",
     "run, but revert to the usual set, the default is N(o).                " );

   procedure put(help : help_type) is
   begin
      new_line;
      for i in help'first..help'last  loop
         put_line(help(i));
      end loop;
      new_line;
   end put;

   procedure update_local_dictionary_file is
      blank_line : constant string(1..80) := (others => ' ');
      line, stem_line, part_line, mean_line : string(1..80) := blank_line;
      l, sl, pl, ml : integer := 0;    --  SL BAD NAME !!!!!!!!!!!
                                       --DICT_LOC : DICTIONARY;   --  Def in LINE_STUFF
      dict_loc_file : file_type;
      dummy : file_type;
      --  Omit when put name here
      dict_loc_name : constant string :=
        add_file_name_extension(dictionary_file_name, "LOCAL");

      procedure ready_dict_loc_file is
         --  Effectively goes to the end of DICT_LOC to ready for appending
         --  Does this by making a new file and writing the old DICT_LOC into it
         --  If there is not already a DICT_LOC, it creates one
      begin
         open(dict_loc_file, in_file, dict_loc_name);
         create(dummy, out_file);
         while not end_of_file(dict_loc_file)  loop
            get_line(dict_loc_file, line, l);
            put_line(dummy, line(1..l));
         end loop;
         reset(dummy, in_file);
         delete(dict_loc_file);     --  Might RESET, but environment might not support
         create(dict_loc_file, out_file, dict_loc_name);
         while not end_of_file(dummy)  loop
            get_line(dummy, line, l);
            put_line(dict_loc_file, line(1..l));
         end loop;
         delete(dummy);
      exception
         when name_error  =>
            create(dict_loc_file, out_file, dict_loc_name);
      end ready_dict_loc_file;

      procedure append_to_dict_loc_file is
         --  This just appends the 3 lines of a dictionary entry to DICT_LOC
         --  It prepares the file to write at the end, writes, then closes it
      begin
         ready_dict_loc_file;
         put_line(dict_loc_file, stem_line(1..sl));   --  SL bad name
         put(dict_loc_file, part_line(1..pl));
         put_line(dict_loc_file, " X X X X X ");
         put_line(dict_loc_file, mean_line(1..ml));

         close(dict_loc_file);

      end append_to_dict_loc_file;

   begin
      loop

         text_io.put("STEMS =>");
         get_line(stem_line, sl);
         if sl > 0  then  --  if no input for stems, then just skip the entry
            text_io.put("PART  =>");
            get_line(part_line, pl);
            text_io.put("MEAN =>");
            get_line(mean_line, ml);
         else
            exit;       --  on no entry, just CR
         end if;

         begin
            append_to_dict_loc_file;

            dict_loc := null_dictionary;
            load_dictionary(dict_loc,
              add_file_name_extension(dictionary_file_name, "LOCAL"));
            --  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
            load_stem_file(local);
            dictionary_available(local) := true;
            exit;       --  If everything OK, otherwise loop back and try again
         end;

      end loop;

   end update_local_dictionary_file;

   procedure put_mdevs is
      use mdev_type_io;
      use reply_type_io;
   begin
      if is_open(mdev_file)  then
         close(mdev_file);
      end if;
      create(mdev_file, out_file, mdev_full_name);
      for i in words_mdev'range  loop
         put(mdev_file, i);
         set_col(mdev_file, 35);
         put(mdev_file, reply(words_mdev(i)));
         new_line(mdev_file);
      end loop;
      put(mdev_file, "START_FILE_CHARACTER             '" &
        start_file_character &"'"); new_line(mdev_file);
        put(mdev_file, "CHANGE_PARAMETERS_CHARACTER      '" &
          change_parameters_character &"'"); new_line(mdev_file);
          put(mdev_file, "CHANGE_DEVELOPER_MODES_CHARACTER '" &
            change_developer_modes_character &"'"); new_line(mdev_file);
            close(mdev_file);
   end put_mdevs;

   procedure get_mdevs is
      use mdev_type_io;
      use reply_type_io;
      mo : mdev_type;
      rep : reply_type;
      line : string(1..100) := (others => ' ');
      last : integer := 0;
   begin
      open(mdev_file, in_file, mdev_full_name);
      for i in words_mdev'range  loop
         get(mdev_file, mo);
         get(mdev_file, rep);
         words_mdev(mo) := mdev_of_reply(rep);
      end loop;
      skip_line(mdev_file);

      get_line(mdev_file, line, last);
      if line(1..20) = "START_FILE_CHARACTER"  then
         if ((line(35) in '!'..'/')  or
           (line(35) in ':'..'@')  or
           (line(35) in '['..'`')  or
           (line(35) in '{'..'~'))  and
           (line(35) /= change_parameters_character)  and
           (line(35) /= change_developer_modes_character)  then
            start_file_character := line(35);
         else
            put_line("Not an acceptable START_FILE_CHARACTER, may conflict");
            put_line("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      else
         raise bad_mdev_file;
      end if;

      get_line(mdev_file, line, last);
      if line(1..27) = "CHANGE_PARAMETERS_CHARACTER"  then
         if ((line(35) in '!'..'/')  or
           (line(35) in ':'..'@')  or
           (line(35) in '['..'`')  or
           (line(35) in '{'..'~'))  and
           (line(35) /= start_file_character)  and
           (line(35) /= change_developer_modes_character)  then
            change_parameters_character := line(35);
         else
            put_line("Not an acceptable CHANGE_PARAMETERS_CHARACTER, may conflict");
            put_line("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      else
         raise bad_mdev_file;
      end if;

      get_line(mdev_file, line, last);
      if line(1..32) = "CHANGE_DEVELOPER_MODES_CHARACTER"  then
         if ((line(35) in '!'..'/')  or
           (line(35) in ':'..'@')  or
           (line(35) in '['..'`')  or
           (line(35) in '{'..'~'))  and
           (line(35) /= start_file_character)  and
           (line(35) /= change_parameters_character)  then
            change_developer_modes_character := line(35);
         else
            put_line("Not an acceptable CHANGE_DEVELOPER_MODES_CHARACTER, may conflict");
            put_line("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      else
         raise bad_mdev_file;
      end if;
      close(mdev_file);

   exception
      when name_error  =>
         raise;
      when others =>
         raise bad_mdev_file;
   end get_mdevs;

   procedure inquire(mo : mdev_type; help : in help_type := no_help) is
      use mdev_type_io;
      use reply_type_io;
      l1 : string(1..100);
      ll : natural;
      r  : reply_type;
   begin
      put(mo);
      put(" ?  "); set_col(45); put("(Currently  ");
      put(reply(words_mdev(mo))); put(" =>");
      get_line(l1, ll);
      if ll /= 0  then
         if trim(l1(1..ll)) = ""  then
            put_line("Blank input, skipping the rest of CHANGE_DEVELOPER_MODES");
            raise blank_input;
         elsif l1(1) = '?'  then
            put(help);
            inquire(mo, help);
         else
            get(l1(1..ll), r, ll);
            words_mdev(mo) := mdev_of_reply(r);
         end if;
      end if;
      new_line;
   end inquire;

   procedure change_developer_modes is
      l1 : string(1..100);
      ll : natural;
      r  : reply_type;

   begin

      put_line("To set developer modes reply Y/y or N/n.  Return accepts current value.");
      put_line("A '?' reply gives infomation/help on that parameter.  A space skips the rest.");
      put_line("Developer modes are only for special requirements and may not all be operable.");
      new_line;

      --  Interactive MDEV - lets you do things on unknown words

      --  You can say it is a noun and then look at the endings
      --  Or look all the endings and guess what part of speech

      --  You can look at the dictionary items that are close to the word
      --  There may be cases in which the stem is found but is not of right part
      --  So maybe the word list is deficient and that root goes also to a ADJ
      --  even if it is listed only for a N.
      --  One can also look for ADV here with ending 'e', etc.

      --  You can look up the word in a paper dictionary (with the help of ending)
      --  And then enter the word into DICT.LOC, so it will hit next time

      --  All unknowns could be recorded in a file for later reference

      --  A '?' gives information (help) about the item in question

      --  One can change the symbol that the main program uses for change and file

      --  One can save the new parameters or let them revert to previous
      --  There should be a basic set of parameters that one can always go to

      --  There should be moods of translation, maybe to switch dictionaries

      --  Maybe to turn on or off pre/suffix
      --  Maybe to allow the user to look at just all the prefixes that match

      --    INQUIRE(HAVE_DEBUG_FILE, HAVE_DEBUG_FILE_HELP);
      --    if IS_OPEN(DBG)  and then not WORDS_MDEV(HAVE_DEBUG_FILE)  then
      --      DELETE(DBG);
      --      WORDS_MDEV(WRITE_DEBUG_FILE) := FALSE;
      --    end if;
      --    if not IS_OPEN(DBG) and then WORDS_MDEV(HAVE_DEBUG_FILE)  then
      --      begin
      --        CREATE(DBG, OUT_FILE, DEBUG_FULL_NAME);
      --      exception
      --        when others =>
      --          PUT_LINE("Cannot CREATE WORD.DBG - Check if it is in use elsewhere");
      --      end;
      --    end if;
      --
      --    if WORDS_MDEV(HAVE_DEBUG_FILE)  then
      --      INQUIRE(WRITE_DEBUG_FILE, WRITE_DEBUG_FILE_HELP);
      --    end if;

      inquire(have_statistics_file, have_statistics_file_help);
      if is_open(stats)  and then not words_mdev(have_statistics_file)  then
         delete(stats);
         words_mdev(write_statistics_file) := false;
      end if;
      if not is_open(stats) and then words_mdev(have_statistics_file)  then
         begin
            create(stats, out_file, stats_full_name);
         exception
            when others =>
               put_line("Cannot CREATE WORD.STA - Check if it is in use elsewhere");
         end;
      end if;

      if words_mdev(have_statistics_file)  then
         inquire(write_statistics_file, write_statistics_file_help);
      end if;

      inquire(do_only_initial_word, do_only_initial_word_help);
      if words_mdev(do_only_initial_word)  then
         inquire(for_word_list_check, for_word_list_check_help);
      else
         words_mdev(for_word_list_check) := false;
      end if;

      inquire(show_dictionary, show_dictionary_help);

      inquire(show_dictionary_line, show_dictionary_line_help);

      inquire(show_dictionary_codes, show_dictionary_codes_help);

      inquire(do_pearse_codes, do_pearse_codes_help);

      if words_mode(do_fixes) then
         inquire(do_only_fixes, do_only_fixes_help);
         inquire(do_fixes_anyway, do_fixes_anyway_help);
      end if;

      inquire(use_prefixes, use_prefixes_help);

      inquire(use_suffixes, use_suffixes_help);

      inquire(use_tackons, use_tackons_help);

      if words_mode(do_tricks) then
         inquire(do_medieval_tricks, do_medieval_tricks_help);
      end if;

      inquire(do_syncope, do_syncope_help);

      inquire(do_two_words, do_two_words_help);

      inquire(include_unknown_context, include_unknown_context_help);

      inquire(no_meanings, no_meanings_help);

      inquire(omit_archaic, omit_archaic_help);

      inquire(omit_medieval, omit_medieval_help);

      inquire(omit_uncommon, omit_uncommon_help);

      inquire(do_i_for_j, do_i_for_j_help);

      inquire(do_u_for_v, do_u_for_v_help);

      inquire(pause_in_screen_output, pause_in_screen_output_help);

      inquire(no_screen_activity, no_screen_activity_help);

      inquire(update_local_dictionary, update_local_dictionary_help);

      inquire(update_meanings, update_meanings_help);

      inquire(minimize_output, minimize_output_help);

      put("START_FILE_CHARACTER ?  "); set_col(45); put("(Currently  '");
      put(start_file_character); put("'");
      put(" =>");
      get_line(l1, ll);
      if ll /= 0  then
         if ((l1(1) in '!'..'/')  or
           (l1(1) in ':'..'@')  or
           (l1(1) in '['..'`')  or
           (l1(1) in '{'..'~'))  and
           (l1(1) /= change_parameters_character)  and
           (l1(1) /= change_developer_modes_character)  then
            start_file_character := l1(1);
         else
            put_line("Not an acceptable character, may conflict with other input");
            put_line("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      end if;
      new_line;

      put("CHANGE_PARAMETERS_CHARACTER ?  "); set_col(45); put("(Currently  '");
      put(change_parameters_character); put("'");
      put(" =>");
      get_line(l1, ll);
      if ll /= 0  then
         if ((l1(1) in '!'..'/')  or
           (l1(1) in ':'..'@')  or
           (l1(1) in '['..'`')  or
           (l1(1) in '{'..'~'))  and
           (l1(1) /= start_file_character)  and
           (l1(1) /= change_developer_modes_character)  then
            change_parameters_character := l1(1);
         else
            put_line("Not an acceptable character, may conflict with other input");
            put_line("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      end if;
      new_line;

      put("CHANGE_DEVELOPER_MODES_CHARACTER ?  ");
      set_col(45); put("(Currently  '");
      put(change_developer_modes_character); put("'");
      put(" =>");
      get_line(l1, ll);
      if ll /= 0  then
         if ((l1(1) in '!'..'/')  or
           (l1(1) in ':'..'@')  or
           (l1(1) in '['..'`')  or
           (l1(1) in '{'..'~'))  and
           (l1(1) /= start_file_character)  and
           (l1(1) /= change_language_character)  and
           (l1(1) /= change_parameters_character)  then
            change_developer_modes_character := l1(1);
         else
            put_line("Not an acceptable character, may conflict with other input");
            put_line("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      end if;
      new_line;

      put("Do you wish to save this set of parameters? Y or N (Default) ");
      put(" =>");
      get_line(l1, ll);
      if ll /= 0  then
         if l1(1) = '?'  then
            put(save_parameters_help);
            put("Do you wish to save this set of parameters? Y or N (Default) ");
            put(" =>");
            get_line(l1, ll);
         end if;
         reply_type_io.get(l1(1..ll), r, ll);
         if mdev_of_reply(r)  then
            put_mdevs;
            put_line("MDEV_ARRAY saved in file " & mdev_full_name);
         end if;
      end if;
      new_line;

   exception
      when blank_input  =>
         null;
      when others =>
         put_line("Bad input - terminating CHANGE_DEVELOPER_PARAMETERS");

   end change_developer_modes;

   procedure initialize_developer_parameters is
   begin

  do_mdev_file:
      begin
         --  Read the MDEV file
         get_mdevs;
         preface.put_line("MDEV_FILE found - Using those MDEVs and parameters");
      exception
         --  If there is any problem
         --  Put that the MDEV file is corrupted and the options are:
         --  to proceed with default parameters
         --  to set parameters with a CHANGE (SET) PARAMETERS and save
         --  to examine the MDEV file with a text editor and try to repair it
         when name_error  =>
            words_mdev := default_mdev_array;
         when bad_mdev_file  =>
            preface.put_line("MDEV_FILE exists, but empty or corupted - Default MDEVs used");
            preface.put_line("You can set new parameters with CHANGE PARAMETERS and save.");
            words_mdev := default_mdev_array;
      end do_mdev_file;

      --  if not IS_OPEN(DBG) and then WORDS_MDEV(HAVE_DEBUG_FILE)  then
      --    CREATE(DBG, OUT_FILE, DEBUG_FULL_NAME);
      --    PREFACE.PUT_LINE("WORD.DBG Created at Initialization");
      --  end if;
      if not is_open(stats) and then words_mdev(have_statistics_file)  then
         create(stats, out_file, stats_full_name);
         preface.put_line("WORD.STA Created at Initialization");
      end if;

   end initialize_developer_parameters;

end developer_parameters;
