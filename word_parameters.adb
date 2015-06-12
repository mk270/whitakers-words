with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with CONFIG; use CONFIG;
with PREFACE;
pragma Elaborate(PREFACE);
package body WORD_PARAMETERS is
  use TEXT_IO;

  type HELP_TYPE is array (NATURAL range <>) of STRING(1..70);
  BLANK_HELP_LINE : constant STRING(1..70) := (others => ' ');
  NO_HELP : constant HELP_TYPE := (2..1 => BLANK_HELP_LINE);


  type REPLY_TYPE is (N, Y);
  package REPLY_TYPE_IO is new TEXT_IO.ENUMERATION_IO(REPLY_TYPE);
  REPLY : array (BOOLEAN) of REPLY_TYPE := (N, Y);
  MODE_OF_REPLY : array (REPLY_TYPE) of BOOLEAN := (FALSE, TRUE);

  BLANK_INPUT : exception;
  
  --  The default modes are set in the body so that they can be changed
  --  with only this being recompiled, not the rest of the with'ing system
  DEFAULT_MODE_ARRAY : constant MODE_ARRAY := (
                      TRIM_OUTPUT                 => TRUE,      

                      HAVE_OUTPUT_FILE            => FALSE,
                      WRITE_OUTPUT_TO_FILE        => FALSE,

                      DO_UNKNOWNS_ONLY            => FALSE,
                      WRITE_UNKNOWNS_TO_FILE      => FALSE,

                      IGNORE_UNKNOWN_NAMES        => TRUE,
                      IGNORE_UNKNOWN_CAPS         => TRUE,
                      DO_COMPOUNDS                => TRUE,
                      DO_FIXES                    => TRUE,
                      DO_TRICKS                   => TRUE,

                      DO_DICTIONARY_FORMS         => TRUE,
                      SHOW_AGE                    => FALSE,
                      SHOW_FREQUENCY              => FALSE,

                      DO_EXAMPLES                 => FALSE,
                      DO_ONLY_MEANINGS            => FALSE,
                      DO_STEMS_FOR_UNKNOWN        => FALSE    );

  BAD_MODE_FILE : exception;


TRIM_OUTPUT_HELP : constant HELP_TYPE :=  (
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


HAVE_OUTPUT_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to create a file which can hold the ",
   "output for later study, otherwise the results are just displayed on   ",
   "the screen.  The output file is named " & OUTPUT_FULL_NAME
                                  & (39+OUTPUT_FULL_NAME'LENGTH..70 => ' '),
   "This means that one run will necessarily overwrite a previous run,    ",
   "unless the previous results are renamed or copied to a file of another",
   "name.  This is available if the METHOD is INTERACTIVE, no parameters. ",
   "The default is N(o), since this prevents the program from overwriting ",
   "previous work unintentionally.  Y(es) creates the output file.        " );

WRITE_OUTPUT_TO_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when HAVE_OUTPUT_FILE is on, to    ",
   "write results to the file " & OUTPUT_FULL_NAME
                                  & (27+OUTPUT_FULL_NAME'LENGTH..70 => ' '),
   "This option may be turned on and off during running of the program,   ",
   "thereby capturing only certain desired results.  If the option        ",
   "HAVE_OUTPUT_FILE is off, the user will not be given a chance to turn  ",
   "this one on.  Only for INTERACTIVE running.         Default is N(o).  ",
   "This works in English mode, but output in somewhat diffeent so far.   " );

DO_UNKNOWNS_ONLY_HELP : constant HELP_TYPE :=  (
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
   
WRITE_UNKNOWNS_TO_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to write all unresolved words to a  ",
   "UNKNOWNS file named " & UNKNOWNS_FULL_NAME
                                & (21+UNKNOWNS_FULL_NAME'LENGTH..70 => ' '),
   "With this option on , the file of unknowns is written, even though    ",
   "the main output contains both known and unknown (unresolved) words.   ",
   "One may wish to save the unknowns for later analysis, testing, or to  ",
   "form the basis for dictionary additions.  When this option is turned  ",
   "on, the UNKNOWNS file is written, destroying any file from a previous ",
   "run.  However, the write may be turned on and off during a single run ",
   "without destroying the information written in that run.               ",
   "This option is for specialized use, so its default is N(o).           ",
   "This does not work in English mode, but may in the future.            " );

IGNORE_UNKNOWN_NAMES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to assume that any capitalized word ",
   "longer than three letters is a proper name.  As no dictionary can be  ",
   "expected to account for many proper names, many such occur that would ",
   "be called UNKNOWN.  This contaminates the output in most cases, and   ",
   "it is often convenient to ignore these sperious UNKNOWN hits.  This   ",
   "option implements that mode, and calls such words proper names.       ",
   "Any proper names that are in the dictionary are handled in the normal ",
   "manner.                                The default is Y(es).          " );

IGNORE_UNKNOWN_CAPS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to assume that any all caps word    ",
   "is a proper name or similar designation.  This convention is often    ",
   "used to designate speakers in a discussion or play.  No dictionary can",
   "claim to be exaustive on proper names, so many such occur that would  ",
   "be called UNKNOWN.  This contaminates the output in most cases, and   ",
   "it is often convenient to ignore these sperious UNKNOWN hits.  This   ",
   "option implements that mode, and calls such words names.  Any similar ",
   "designations that are in the dictionary are handled in the normal     ",
   "manner, as are normal words in all caps.    The default is Y(es).     " );

DO_COMPOUNDS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to look ahead for the verb TO_BE (or",
   "iri) when it finds a verb participle, with the expectation of finding ",
   "a compound perfect tense or periphastic.  This option can also be a   ",
   "trimming of the output, in that VPAR that do not fit (not NOM) will be",
   "excluded, possible interpretations are lost.  Default choice is Y(es).",
   "This processing is turned off with the choice of N(o).                " );

DO_FIXES_HELP : constant HELP_TYPE :=  (
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

DO_TRICKS_HELP : constant HELP_TYPE :=  (
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

DO_DICTIONARY_FORMS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to output a line with the forms     ",
   "normally associated with a dictionary entry (NOM and GEN of a noun,   ",
   "the four principal parts of a verb, M-F-N NOM of an adjective, ...).  ",
   "This occurs when there is other output (i.e., not with UNKNOWNS_ONLY).",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

SHOW_AGE_HELP : constant HELP_TYPE :=  (
   "This option causes a flag, like '<Late>' to appear for inflection or  ",
   "form in the output.  The AGE indicates when this word/inflection was  ",
   "in use, at least from indications is dictionary citations.  It is     ",
   "just an indication, not controlling, useful when there are choices.   ",
   "No indication means that it is common throughout all periods.         ",
   "The default choice is Y(es), but it can be turned off with a N(o).    " );

SHOW_FREQUENCY_HELP : constant HELP_TYPE :=  (
   "This option causes a flag, like '<rare>' to appear for inflection or  ",
   "form in the output.  The FREQ is indicates the relative usage of the  ",
   "word or inflection, from indications is dictionary citations.  It is  ",
   "just an indication, not controlling, useful when there are choices.   ",
   "No indication means that it is common throughout all periods.         ",
   "The default choice is Y(es), but it can be turned off with a N(o).    " );

DO_EXAMPLES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to provide examples of usage of the ",
   "cases/tenses/etc. that were constructed.  The default choice is N(o). ",
   "This produces lengthly output and is turned on with the choice Y(es). " );

DO_ONLY_MEANINGS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to only output the MEANING for a    ",
   "word, and omit the inflection details.  This is primarily used in     ",
   "analyzing new dictionary material, comparing with the existing.       ",
   "However it may be of use for the translator who knows most all of     ",
   "the words and just needs a little reminder for a few.                 ",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

DO_STEMS_FOR_UNKNOWN_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when it is unable to find a proper ",
   "match in the dictionary, and after various prefixes and suffixes, to  ",
   "list the dictionary entries around the unknown.  This will likely     ",
   "catch a substantive for which only the ADJ stem appears in dictionary,",
   "an ADJ for which there is only a N stem, etc.  This option should     ",
   "probably only be used with individual UNKNOWN words, and off-line     ",
   "from full translations, therefore the default choice is N(o).         ",
   "This processing can be turned on with the choice of Y(es).            " );


SAVE_PARAMETERS_HELP : constant HELP_TYPE :=  (
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

  procedure PUT(HELP : HELP_TYPE) is
  begin
    NEW_LINE;
    for I in HELP'FIRST..HELP'LAST  loop
      PUT_LINE(HELP(I));
    end loop;
    NEW_LINE;
  end PUT;

  procedure PUT_MODES is
    use MODE_TYPE_IO;
    use REPLY_TYPE_IO;
  begin
    if IS_OPEN(MODE_FILE)  then
      CLOSE(MODE_FILE);
    end if;
    CREATE(MODE_FILE, OUT_FILE, MODE_FULL_NAME);
    for I in WORDS_MODE'RANGE  loop
      PUT(MODE_FILE, I);
      SET_COL(MODE_FILE, 35);
      PUT(MODE_FILE, REPLY(WORDS_MODE(I)));
      NEW_LINE(MODE_FILE);
    end loop;
    CLOSE(MODE_FILE);
  end PUT_MODES;

  procedure GET_MODES is --(M : out MODE_ARRAY) is
    use MODE_TYPE_IO;
    use REPLY_TYPE_IO;
    MO : MODE_TYPE;
    REP : REPLY_TYPE;
  begin
    OPEN(MODE_FILE, IN_FILE, MODE_FULL_NAME);
    while not END_OF_FILE(MODE_FILE)  loop
      GET(MODE_FILE, MO);
      GET(MODE_FILE, REP);
      WORDS_MODE(MO) := MODE_OF_REPLY(REP);
    end loop;
    CLOSE(MODE_FILE);

  exception
    when NAME_ERROR  =>
      raise;
    when others =>
      raise BAD_MODE_FILE;
  end GET_MODES;

  procedure INQUIRE(MO : MODE_TYPE; HELP : in HELP_TYPE := NO_HELP) is
    use MODE_TYPE_IO;
    use REPLY_TYPE_IO;
    L1 : STRING(1..100) := (others => ' ');
    LL : NATURAL;
    R  : REPLY_TYPE;
  begin
    PUT(MO);
    PUT(" ?  "); SET_COL(45); PUT("(Currently  ");
    PUT(REPLY(WORDS_MODE(MO))); PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if TRIM(L1(1..LL)) = ""  then
        PUT_LINE("Blank input, skipping the rest of CHANGE_PARAMETERS");
        raise BLANK_INPUT;                 
      elsif L1(1) = '?'  then
        PUT(HELP);
        INQUIRE(MO, HELP);
      else
        GET(L1(1..LL), R, LL);
        WORDS_MODE(MO) := MODE_OF_REPLY(R);
      end if;
    end if;
    NEW_LINE;
  end INQUIRE;


  procedure CHANGE_PARAMETERS is
    L1 : STRING(1..100) := (others => ' ');
    LL : NATURAL;
    R  : REPLY_TYPE;

  begin

    
    PUT_LINE("To set/change parameters reply Y/y or N/n.  Return accepts current value.");
    PUT_LINE("A '?' reply gives infomation/help on that parameter.  A space skips the rest.");
    NEW_LINE;
     
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

    INQUIRE(TRIM_OUTPUT, TRIM_OUTPUT_HELP);


    INQUIRE(HAVE_OUTPUT_FILE, HAVE_OUTPUT_FILE_HELP);


    if IS_OPEN(OUTPUT)  and then not WORDS_MODE(HAVE_OUTPUT_FILE)  then
      CLOSE(OUTPUT);
      WORDS_MODE(WRITE_OUTPUT_TO_FILE) := FALSE;
    end if;
    if not IS_OPEN(OUTPUT) and then WORDS_MODE(HAVE_OUTPUT_FILE)  then
      begin
        CREATE(OUTPUT, OUT_FILE, OUTPUT_FULL_NAME);
      exception
        when others =>
          PUT_LINE("Cannot CREATE WORD.OUT - Check if it is in use elsewhere");
      end;
      end if;

    if WORDS_MODE(HAVE_OUTPUT_FILE)  then
      INQUIRE(WRITE_OUTPUT_TO_FILE, WRITE_OUTPUT_TO_FILE_HELP);
    end if;

    INQUIRE(DO_UNKNOWNS_ONLY, DO_UNKNOWNS_ONLY_HELP);

    INQUIRE(WRITE_UNKNOWNS_TO_FILE, WRITE_UNKNOWNS_TO_FILE_HELP);
    --  If there is an open file then OK
    --  If not open and you now want to start writing to UNKNOWNS, the CREATE
    if not IS_OPEN(UNKNOWNS) and then WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
      begin
        CREATE(UNKNOWNS, OUT_FILE, UNKNOWNS_FULL_NAME);
      exception
        when others =>
          PUT_LINE("Cannot CREATE WORD.UNK - Check if it is in use elsewhere");
      end;
      end if;


    INQUIRE(IGNORE_UNKNOWN_NAMES, IGNORE_UNKNOWN_NAMES_HELP);

    INQUIRE(IGNORE_UNKNOWN_CAPS, IGNORE_UNKNOWN_CAPS_HELP);

    INQUIRE(DO_COMPOUNDS, DO_COMPOUNDS_HELP);

    INQUIRE(DO_FIXES, DO_FIXES_HELP);

    INQUIRE(DO_TRICKS, DO_TRICKS_HELP);


    INQUIRE(DO_DICTIONARY_FORMS, DO_DICTIONARY_FORMS_HELP);

    INQUIRE(SHOW_AGE, SHOW_AGE_HELP);

    INQUIRE(SHOW_FREQUENCY, SHOW_FREQUENCY_HELP);


    INQUIRE(DO_EXAMPLES, DO_EXAMPLES_HELP);

    INQUIRE(DO_ONLY_MEANINGS, DO_ONLY_MEANINGS_HELP);

    INQUIRE(DO_STEMS_FOR_UNKNOWN, DO_STEMS_FOR_UNKNOWN_HELP);


    PUT("Do you wish to save this set of parameters? Y or N (Default) ");
    PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if L1(1) = '?'  then
        PUT(SAVE_PARAMETERS_HELP);
        PUT("Do you wish to save this set of parameters? Y or N (Default) ");
        PUT(" =>");
        GET_LINE(L1, LL);
      end if;
      REPLY_TYPE_IO.GET(L1(1..LL), R, LL);
      if MODE_OF_REPLY(R)  then
        PUT_MODES;
        PUT_LINE("MODE_ARRAY saved in file " & MODE_FULL_NAME);
      end if;
    end if;
    NEW_LINE;

  exception
    when BLANK_INPUT  =>
      null;
    when others =>
      PUT_LINE("Bad input - terminating CHANGE_PARAMETERS");

  end CHANGE_PARAMETERS;

  
     
  procedure INITIALIZE_WORD_PARAMETERS is
begin
  WORDS_MODE := DEFAULT_MODE_ARRAY;
--TEXT_IO.PUT_LINE("Initializing WORD_PARAMETERS");

  DO_MODE_FILE:
  begin
    --  Read the mode file
    GET_MODES; --(WORDS_MODE);
    PREFACE.PUT_LINE("MODE_FILE found - Using those modes and parameters");
  exception
  --  If there is any problem
  --  Put that the mode file is corrupted and the options are:
      --  to proceed with default parameters
      --  to set parameters with a CHANGE (SET) PARAMETERS and save
      --  to examine the mode file with a text editor and try to repair it
    when NAME_ERROR  =>
      WORDS_MODE := DEFAULT_MODE_ARRAY;
    when BAD_MODE_FILE  =>
      PUT_LINE("MODE_FILE exists, but empty or corupted - Default modes used");
      PUT_LINE("You can set new parameters with CHANGE PARAMETERS and save.");
      WORDS_MODE := DEFAULT_MODE_ARRAY;
  when others  =>
      PUT_LINE("MODE_FILE  others ERROR");
      WORDS_MODE := DEFAULT_MODE_ARRAY;
    end DO_MODE_FILE;

  if ((METHOD = INTERACTIVE) or (METHOD = COMMAND_LINE_INPUT)) and then
     (not TEXT_IO.IS_OPEN(OUTPUT)) and then
     (WORDS_MODE(HAVE_OUTPUT_FILE))  then
    TEXT_IO.CREATE(OUTPUT, TEXT_IO.OUT_FILE, OUTPUT_FULL_NAME);
    --TEXT_IO.PUT_LINE("WORD.OUT Created at Initialization");
    PREFACE.PUT_LINE("WORD.OUT Created at Initialization");
  end if;
  if not TEXT_IO.IS_OPEN(UNKNOWNS) and then WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
    TEXT_IO.CREATE(UNKNOWNS, TEXT_IO.OUT_FILE, UNKNOWNS_FULL_NAME);
    PREFACE.PUT_LINE("WORD.UNK Created at Initialization");
  end if;
end INITIALIZE_WORD_PARAMETERS;


end WORD_PARAMETERS;
