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

with strings_package; use strings_package;
with latin_file_names; use latin_file_names;
with config; use config;
with preface;
pragma elaborate(preface);
package body word_parameters is
   use text_io;

   type help_type is array (natural range <>) of string(1..70);
   blank_help_line : constant string(1..70) := (others => ' ');
   no_help : constant help_type := (2..1 => blank_help_line);

   type reply_type is (n, y);
   package reply_type_io is new text_io.enumeration_io(reply_type);
   reply : constant array (boolean) of reply_type := (n, y);
   mode_of_reply : constant array (reply_type) of boolean := (false, true);

   blank_input : exception;

   --  The default modes are set in the body so that they can be changed
   --  with only this being recompiled, not the rest of the with'ing system
   default_mode_array : constant mode_array := (
     trim_output                 => true,
     
     have_output_file            => false,
     write_output_to_file        => false,

     do_unknowns_only            => false,
     write_unknowns_to_file      => false,
     
     ignore_unknown_names        => true,
     ignore_unknown_caps         => true,
     do_compounds                => true,
     do_fixes                    => true,
     do_tricks                   => true,
     
     do_dictionary_forms         => true,
     show_age                    => false,
     show_frequency              => false,
     
     do_examples                 => false,
     do_only_meanings            => false,
     do_stems_for_unknown        => false    );
   
   bad_mode_file : exception;

   trim_output_help : constant help_type :=  (
     "This option instructs the program to remove from the output list of   ",
     "possible constructs those which are least likely.  There is now a fair",
     "amount of trimming, killing LOC and VOC plus removing Uncommon and    ",
     "non-classical (Archaic/Medieval) when more common results are found   ",
     "and this action is requested (turn it off in MDV (!) parameters).     ",
     "When a TRIM has been done, output is usually followed by asterix (*). ",
     "The asterix may be missing depending on where the TRIM is done.       ",
     "There certainly is no absolute assurence that the items removed are   ",
     "not correct, just that they are statistically less likely.            ",
     "Note that poets are likely to employ unusual words and inflections for",
     "various reasons.  These may be trimmed out if this parameter in on.   ",
     "When in English mode, trim just reduces the output to the top six     ",
     "results, if there are that many.  Asterix means there are more        ",
     "                                                The default is Y(es)  " );
   
   have_output_file_help : constant help_type :=  (
     "This option instructs the program to create a file which can hold the ",
     "output for later study, otherwise the results are just displayed on   ",
     "the screen.  The output file is named " & output_full_name
     & (39+output_full_name'length..70 => ' '),
     "This means that one run will necessarily overwrite a previous run,    ",
     "unless the previous results are renamed or copied to a file of another",
     "name.  This is available if the METHOD is INTERACTIVE, no parameters. ",
     "The default is N(o), since this prevents the program from overwriting ",
     "previous work unintentionally.  Y(es) creates the output file.        " );
   
   write_output_to_file_help : constant help_type :=  (
     "This option instructs the program, when HAVE_OUTPUT_FILE is on, to    ",
     "write results to the file " & output_full_name
     & (27+output_full_name'length..70 => ' '),
     "This option may be turned on and off during running of the program,   ",
     "thereby capturing only certain desired results.  If the option        ",
     "HAVE_OUTPUT_FILE is off, the user will not be given a chance to turn  ",
     "this one on.  Only for INTERACTIVE running.         Default is N(o).  ",
     "This works in English mode, but output in somewhat diffeent so far.   " );
   
   do_unknowns_only_help : constant help_type :=  (
     "This option instructs the program to only output those words that it  ",
     "cannot resolve.  Of course, it has to do processing on all words, but ",
     "those that are found (with prefix/suffix, if that option in on) will  ",
     "be ignored.  The purpose of this option is t allow a quick look to    ",
     "determine if the dictionary and process is going to do an acceptable  ",
     "job on the current text.  It also allows the user to assemble a list  ",
     "of unknown words to look up manually, and perhaps augment the system  ",
     "dictionary.  For those purposes, the system is usually run with the   ",
     "MINIMIZE_OUTPUT option, just producing a list.  Another use is to run ",
     "without MINIMIZE to an output file.  This gives a list of the input   ",
     "text with the unknown words, by line.  This functions as a spelling   ",
     "checker for Latin texts.  The default is N(o).                        ",
     "This does not work in English mode, but may in the future.            " );

   write_unknowns_to_file_help : constant help_type :=  (
     "This option instructs the program to write all unresolved words to a  ",
     "UNKNOWNS file named " & unknowns_full_name
     & (21+unknowns_full_name'length..70 => ' '),
     "With this option on , the file of unknowns is written, even though    ",
     "the main output contains both known and unknown (unresolved) words.   ",
     "One may wish to save the unknowns for later analysis, testing, or to  ",
     "form the basis for dictionary additions.  When this option is turned  ",
     "on, the UNKNOWNS file is written, destroying any file from a previous ",
     "run.  However, the write may be turned on and off during a single run ",
     "without destroying the information written in that run.               ",
     "This option is for specialized use, so its default is N(o).           ",
     "This does not work in English mode, but may in the future.            " );
   
   ignore_unknown_names_help : constant help_type :=  (
     "This option instructs the program to assume that any capitalized word ",
     "longer than three letters is a proper name.  As no dictionary can be  ",
     "expected to account for many proper names, many such occur that would ",
     "be called UNKNOWN.  This contaminates the output in most cases, and   ",
     "it is often convenient to ignore these sperious UNKNOWN hits.  This   ",
     "option implements that mode, and calls such words proper names.       ",
     "Any proper names that are in the dictionary are handled in the normal ",
     "manner.                                The default is Y(es).          " );
   
   ignore_unknown_caps_help : constant help_type :=  (
     "This option instructs the program to assume that any all caps word    ",
     "is a proper name or similar designation.  This convention is often    ",
     "used to designate speakers in a discussion or play.  No dictionary can",
     "claim to be exaustive on proper names, so many such occur that would  ",
     "be called UNKNOWN.  This contaminates the output in most cases, and   ",
     "it is often convenient to ignore these sperious UNKNOWN hits.  This   ",
     "option implements that mode, and calls such words names.  Any similar ",
     "designations that are in the dictionary are handled in the normal     ",
     "manner, as are normal words in all caps.    The default is Y(es).     " );
   
   do_compounds_help : constant help_type :=  (
     "This option instructs the program to look ahead for the verb TO_BE (or",
     "iri) when it finds a verb participle, with the expectation of finding ",
     "a compound perfect tense or periphastic.  This option can also be a   ",
     "trimming of the output, in that VPAR that do not fit (not NOM) will be",
     "excluded, possible interpretations are lost.  Default choice is Y(es).",
     "This processing is turned off with the choice of N(o).                " );
   
   do_fixes_help : constant help_type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, to attach various prefixes and suffixes and  ",
     "try again.  This effort is successful in about a quarter of the cases ",
     "which would otherwise give UNKNOWN results, or so it seems in limited ",
     "tests.  For those cases in which a result is produced, about half give",
     "easily interpreted output; many of the rest are etymologically true,  ",
     "but not necessarily obvious; about a tenth give entirely spurious     ",
     "derivations.  The user must proceed with caution.                     ",
     "The default choice is Y(es), since the results are generally useful.  ",
     "This processing can be turned off with the choice of N(o).            " );
   
   do_tricks_help : constant help_type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, and after various prefixes and suffixes, to  ",
     "try every dirty Latin trick it can think of, mainly common letter     ",
     "replacements like cl -> cul, vul -> vol, ads -> ass, inp -> imp, etc. ",
     "Together these tricks are useful, but may give false positives (>10%).",
     "They provide for recognized varients in classical spelling.  Most of  ",
     "the texts with which this program will be used have been well edited  ",
     "and standardized in spelling.  Now, moreover,  the dictionary is being",
     "populated to such a state that the hit rate on tricks has fallen to a ",
     "low level.  It is very seldom productive, and it is always expensive. ",
     "The only excuse for keeping it as default is that now the dictionary  ",
     "is quite extensive and misses are rare.         Default is now Y(es). ") ;
   
   do_dictionary_forms_help : constant help_type :=  (
     "This option instructs the program to output a line with the forms     ",
     "normally associated with a dictionary entry (NOM and GEN of a noun,   ",
     "the four principal parts of a verb, M-F-N NOM of an adjective, ...).  ",
     "This occurs when there is other output (i.e., not with UNKNOWNS_ONLY).",
     "The default choice is N(o), but it can be turned on with a Y(es).     " );

   show_age_help : constant help_type :=  (
     "This option causes a flag, like '<Late>' to appear for inflection or  ",
     "form in the output.  The AGE indicates when this word/inflection was  ",
     "in use, at least from indications is dictionary citations.  It is     ",
     "just an indication, not controlling, useful when there are choices.   ",
     "No indication means that it is common throughout all periods.         ",
     "The default choice is Y(es), but it can be turned off with a N(o).    " );

   show_frequency_help : constant help_type :=  (
     "This option causes a flag, like '<rare>' to appear for inflection or  ",
     "form in the output.  The FREQ is indicates the relative usage of the  ",
     "word or inflection, from indications is dictionary citations.  It is  ",
     "just an indication, not controlling, useful when there are choices.   ",
     "No indication means that it is common throughout all periods.         ",
     "The default choice is Y(es), but it can be turned off with a N(o).    " );

   do_examples_help : constant help_type :=  (
     "This option instructs the program to provide examples of usage of the ",
     "cases/tenses/etc. that were constructed.  The default choice is N(o). ",
     "This produces lengthly output and is turned on with the choice Y(es). " );

   do_only_meanings_help : constant help_type :=  (
     "This option instructs the program to only output the MEANING for a    ",
     "word, and omit the inflection details.  This is primarily used in     ",
     "analyzing new dictionary material, comparing with the existing.       ",
     "However it may be of use for the translator who knows most all of     ",
     "the words and just needs a little reminder for a few.                 ",
     "The default choice is N(o), but it can be turned on with a Y(es).     " );

   do_stems_for_unknown_help : constant help_type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, and after various prefixes and suffixes, to  ",
     "list the dictionary entries around the unknown.  This will likely     ",
     "catch a substantive for which only the ADJ stem appears in dictionary,",
     "an ADJ for which there is only a N stem, etc.  This option should     ",
     "probably only be used with individual UNKNOWN words, and off-line     ",
     "from full translations, therefore the default choice is N(o).         ",
     "This processing can be turned on with the choice of Y(es).            " );

   save_parameters_help : constant help_type :=  (
     "This option instructs the program, to save the current parameters, as ",
     "just established by the user, in a file WORD.MOD.  If such a file     ",
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

   procedure put_modes is
      use mode_type_io;
      use reply_type_io;
   begin
      if is_open(mode_file)  then
         close(mode_file);
      end if;
      create(mode_file, out_file, mode_full_name);
      for i in words_mode'range  loop
         put(mode_file, i);
         set_col(mode_file, 35);
         put(mode_file, reply(words_mode(i)));
         new_line(mode_file);
      end loop;
      close(mode_file);
   end put_modes;

   procedure get_modes is --(M : out MODE_ARRAY) is
      use mode_type_io;
      use reply_type_io;
      mo : mode_type;
      rep : reply_type;
   begin
      open(mode_file, in_file, mode_full_name);
      while not end_of_file(mode_file)  loop
         get(mode_file, mo);
         get(mode_file, rep);
         words_mode(mo) := mode_of_reply(rep);
      end loop;
      close(mode_file);

   exception
      when name_error  =>
         raise;
      when others =>
         raise bad_mode_file;
   end get_modes;

   procedure inquire(mo : mode_type; help : in help_type := no_help) is
      use mode_type_io;
      use reply_type_io;
      l1 : string(1..100) := (others => ' ');
      ll : natural;
      r  : reply_type;
   begin
      put(mo);
      put(" ?  "); set_col(45); put("(Currently  ");
      put(reply(words_mode(mo))); put(" =>");
      get_line(l1, ll);
      if ll /= 0  then
         if trim(l1(1..ll)) = ""  then
            put_line("Blank input, skipping the rest of CHANGE_PARAMETERS");
            raise blank_input;
         elsif l1(1) = '?'  then
            put(help);
            inquire(mo, help);
         else
            get(l1(1..ll), r, ll);
            words_mode(mo) := mode_of_reply(r);
         end if;
      end if;
      new_line;
   end inquire;

   procedure change_parameters is
      l1 : string(1..100) := (others => ' ');
      ll : natural;
      r  : reply_type;

   begin

      put_line("To set/change parameters reply Y/y or N/n.  Return accepts current value.");
      put_line("A '?' reply gives infomation/help on that parameter.  A space skips the rest.");
      new_line;

      --  Interactive mode - lets you do things on unknown words

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

      inquire(trim_output, trim_output_help);

      inquire(have_output_file, have_output_file_help);

      if is_open(output)  and then not words_mode(have_output_file)  then
         close(output);
         words_mode(write_output_to_file) := false;
      end if;
      if not is_open(output) and then words_mode(have_output_file)  then
         begin
            create(output, out_file, output_full_name);
         exception
            when others =>
               put_line("Cannot CREATE WORD.OUT - Check if it is in use elsewhere");
         end;
      end if;

      if words_mode(have_output_file)  then
         inquire(write_output_to_file, write_output_to_file_help);
      end if;

      inquire(do_unknowns_only, do_unknowns_only_help);

      inquire(write_unknowns_to_file, write_unknowns_to_file_help);
      --  If there is an open file then OK
      --  If not open and you now want to start writing to UNKNOWNS, the CREATE
      if not is_open(unknowns) and then words_mode(write_unknowns_to_file)  then
         begin
            create(unknowns, out_file, unknowns_full_name);
         exception
            when others =>
               put_line("Cannot CREATE WORD.UNK - Check if it is in use elsewhere");
         end;
      end if;

      inquire(ignore_unknown_names, ignore_unknown_names_help);

      inquire(ignore_unknown_caps, ignore_unknown_caps_help);

      inquire(do_compounds, do_compounds_help);

      inquire(do_fixes, do_fixes_help);

      inquire(do_tricks, do_tricks_help);

      inquire(do_dictionary_forms, do_dictionary_forms_help);

      inquire(show_age, show_age_help);

      inquire(show_frequency, show_frequency_help);

      inquire(do_examples, do_examples_help);

      inquire(do_only_meanings, do_only_meanings_help);

      inquire(do_stems_for_unknown, do_stems_for_unknown_help);

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
         if mode_of_reply(r)  then
            put_modes;
            put_line("MODE_ARRAY saved in file " & mode_full_name);
         end if;
      end if;
      new_line;

   exception
      when blank_input  =>
         null;
      when others =>
         put_line("Bad input - terminating CHANGE_PARAMETERS");

   end change_parameters;

   procedure initialize_word_parameters is
   begin
      words_mode := default_mode_array;
      --TEXT_IO.PUT_LINE("Initializing WORD_PARAMETERS");

  do_mode_file:
      begin
         --  Read the mode file
         get_modes; --(WORDS_MODE);
         preface.put_line("MODE_FILE found - Using those modes and parameters");
      exception
         --  If there is any problem
         --  Put that the mode file is corrupted and the options are:
         --  to proceed with default parameters
         --  to set parameters with a CHANGE (SET) PARAMETERS and save
         --  to examine the mode file with a text editor and try to repair it
         when name_error  =>
            words_mode := default_mode_array;
         when bad_mode_file  =>
            put_line("MODE_FILE exists, but empty or corupted - Default modes used");
            put_line("You can set new parameters with CHANGE PARAMETERS and save.");
            words_mode := default_mode_array;
         when others  =>
            put_line("MODE_FILE  others ERROR");
            words_mode := default_mode_array;
      end do_mode_file;

      if ((method = interactive) or (method = command_line_input)) and then
        (not text_io.is_open(output)) and then
        (words_mode(have_output_file))  then
         text_io.create(output, text_io.out_file, output_full_name);
         --TEXT_IO.PUT_LINE("WORD.OUT Created at Initialization");
         preface.put_line("WORD.OUT Created at Initialization");
      end if;
      if not text_io.is_open(unknowns) and then words_mode(write_unknowns_to_file)  then
         text_io.create(unknowns, text_io.out_file, unknowns_full_name);
         preface.put_line("WORD.UNK Created at Initialization");
      end if;
   end initialize_word_parameters;

end word_parameters;
