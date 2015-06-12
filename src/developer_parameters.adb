with TEXT_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;   --  Omit when put name here
with WORD_PARAMETERS; use WORD_PARAMETERS;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with PREFACE;
with LINE_STUFF; use LINE_STUFF;
pragma Elaborate(PREFACE);
package body DEVELOPER_PARAMETERS is

  use TEXT_IO;


  type HELP_TYPE is array (NATURAL range <>) of STRING(1..70);
  BLANK_HELP_LINE : constant STRING(1..70) := (others => ' ');
  NO_HELP : constant HELP_TYPE := (2..1 => BLANK_HELP_LINE);


  type REPLY_TYPE is (N, Y);
  package REPLY_TYPE_IO is new TEXT_IO.ENUMERATION_IO(REPLY_TYPE);
  REPLY : array (BOOLEAN) of REPLY_TYPE := (N, Y);
  MDEV_OF_REPLY : array (REPLY_TYPE) of BOOLEAN := (FALSE, TRUE);

  BLANK_INPUT : exception;
  
  --  The default MDEVs are set in the body so that they can be changed
  --  with only this being recompiled, not the rest of the with'ing system
  DEFAULT_MDEV_ARRAY : constant MDEV_ARRAY := (

     --               HAVE_DEBUG_FILE             => FALSE,
     --               WRITE_DEBUG_FILE            => FALSE,

                      HAVE_STATISTICS_FILE        => FALSE,
                      WRITE_STATISTICS_FILE       => FALSE,

                      SHOW_DICTIONARY             => FALSE,
                      SHOW_DICTIONARY_LINE        => FALSE,
                      SHOW_DICTIONARY_CODES       => TRUE,   
                      DO_PEARSE_CODES             => FALSE,

                      DO_ONLY_INITIAL_WORD        => FALSE,
                      FOR_WORD_LIST_CHECK         => FALSE,

                      DO_ONLY_FIXES               => FALSE,
                      DO_FIXES_ANYWAY             => FALSE,
                      USE_PREFIXES                => TRUE, 
                      USE_SUFFIXES                => TRUE, 
                      USE_TACKONS                 => TRUE, 
                                            
                      DO_MEDIEVAL_TRICKS          => TRUE,
                      
                      DO_SYNCOPE                  => TRUE,
                      DO_TWO_WORDS                => TRUE,
                      INCLUDE_UNKNOWN_CONTEXT     => TRUE,
                      NO_MEANINGS                 => FALSE,

                      OMIT_ARCHAIC                => TRUE,
                      OMIT_MEDIEVAL               => FALSE,
                      OMIT_UNCOMMON               => TRUE,

                      DO_I_FOR_J                  => FALSE,
                      DO_U_FOR_V                  => FALSE,

                      PAUSE_IN_SCREEN_OUTPUT      => TRUE,
                      NO_SCREEN_ACTIVITY          => FALSE,
                      
                      UPDATE_LOCAL_DICTIONARY     => FALSE,
                      UPDATE_MEANINGS             => FALSE,

                      MINIMIZE_OUTPUT             => TRUE    );

  BAD_MDEV_FILE : exception;




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

HAVE_STATISTICS_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to create a file which can hold     ",
   "certain statistical information about the process.  The file is       ",
   "overwritten for new invocation of the program, so old data must be    ",
   "explicitly saved if it is to be retained.  The statistics are in TEXT ",
   "format.     The statistics file is named " & STATS_FULL_NAME
                                & (42+STATS_FULL_NAME'LENGTH..70 => ' '),
   "This information is only of development use, so the default is N(o).  " );

WRITE_STATISTICS_FILE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, with HAVE_STATISTICS_FILE, to put  ",
   "derived statistics in a file named " & STATS_FULL_NAME
                                   & (36+STATS_FULL_NAME'LENGTH..70 => ' '),
   "This option may be turned on and off while running of the program,    ",
   "thereby capturing only certain desired results.  The file is reset at ",
   "each invocation of the program, if the HAVE_STATISTICS_FILE is set.   ",
   "If the option HAVE_STATISTICS_FILE is off, the user will not be given ",
   "a chance to turn this one on.                Default is N(o).         " );


SHOW_DICTIONARY_HELP : constant HELP_TYPE :=  (
   "This option causes a flag, like 'GEN>' to be put before the meaning   ",
   "in the output.  While this is useful for certain development purposes,",
   "it forces off a few characters from the meaning, and is really of no  ",
   "interest to most users.                                               ",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

SHOW_DICTIONARY_LINE_HELP : constant HELP_TYPE :=  (
   "This option causes the number of the dictionary line for the current  ",
   "meaning to be output.  This is of use to no one but the dictionary    ",
   "maintainer.  The default choice is N(o).  It is activated by Y(es).   ");

SHOW_DICTIONARY_CODES_HELP : constant HELP_TYPE :=  (
   "This option causes the codes for the dictionary entry for the current ",
   "meaning to be output.  This may not be useful to any but the most     ",
   "involved user.  The default choice is N(o).  It is activated by Y(es).");

DO_PEARSE_CODES_HELP : constant HELP_TYPE :=  (
   "This option causes special codes to be output flagging the different  ",
   "kinds of output lines.  01 for forms, 02 for dictionary forms, and    ",
   "03 for meaning. The default choice is N(o).  It is activated by Y(es).",
   "There are no Pearse codes in English mode.                            ");

DO_ONLY_INITIAL_WORD_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to only analyze the initial word on ",
   "each line submitted.  This is a tool for checking and integrating new ",
   "dictionary input, and will be of no interest to the general user.     ",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

FOR_WORD_LIST_CHECK_HELP : constant HELP_TYPE :=  (
   "This option works in conjunction with DO_ONLY_INITIAL_WORD to allow   ",
   "the processing of scanned dictionarys or text word lists.  It accepts ",
   "only the forms common in dictionary entries, like NOM S for N or ADJ, ",
   "or PRES ACTIVE IND 1 S for V.  It is be used only with DO_INITIAL_WORD",
   "The default choice is N(o), but it can be turned on with a Y(es).     " );

DO_ONLY_FIXES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to ignore the normal dictionary     ",
   "search and to go direct to attach various prefixes and suffixes before",
   "processing. This is a pure research tool.  It allows one to examine   ",
   "the coverage of pure stems and dictionary primary compositions.       ",
   "This option is only available if DO_FIXES is turned on.               ",
   "This is entirely a development and research tool, not to be used in   ",
   "conventional translation situations, so the default choice is N(o).   ",
   "This processing can be turned on with the choice of Y(es).            " );


DO_FIXES_ANYWAY_HELP : constant HELP_TYPE :=  (
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

USE_PREFIXES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to implement prefixes from ADDONS   ",
   "whenever and wherever FIXES are called for.  The purpose of this      ",
   "option is to allow some flexibility while the program in running to   ",
   "select various combinations of fixes, to turn them on and off,        ",
   "individually as well as collectively.  This is an option usually      ",
   "employed by the developer while experimenting with the ADDONS file.   ",
   "This option is only effective in connection with DO_FIXES.            ",
   "This is primarily a development tool, so the conventional user should ",
   "probably maintain the default  choice of Y(es).                       " );

USE_SUFFIXES_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to implement suffixes from ADDONS   ",
   "whenever and wherever FIXES are called for.  The purpose of this      ",
   "option is to allow some flexibility while the program in running to   ",
   "select various combinations of fixes, to turn them on and off,        ",
   "individually as well as collectively.  This is an option usually      ",
   "employed by the developer while experimenting with the ADDONS file.   ",
   "This option is only effective in connection with DO_FIXES.            ",
   "This is primarily a development tool, so the conventional user should ",
   "probably maintain the default  choice of Y(es).                       " );

USE_TACKONS_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to implement TACKONS from ADDONS    ",
   "whenever and wherever FIXES are called for.  The purpose of this      ",
   "option is to allow some flexibility while the program in running to   ",
   "select various combinations of fixes, to turn them on and off,        ",
   "individually as well as collectively.  This is an option usually      ",
   "employed by the developer while experimenting with the ADDONS file.   ",
   "This option is only effective in connection with DO_FIXES.            ",
   "This is primarily a development tool, so the conventional user should ",
   "probably maintain the default  choice of Y(es).                       " );


DO_MEDIEVAL_TRICKS_HELP : constant HELP_TYPE :=  (
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
   

DO_SYNCOPE_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to postulate that syncope of        ",
   "perfect stem verbs may have occured (e.g, aver -> ar in the perfect), ",
   "and to try various possibilities for the insertion of a removed 'v'.  ",
   "To do this it has to fully process the modified candidates, which can ",
   "have a consderable impact on the speed of processind a large file.    ",
   "However, this trick seldom producesa false positive, and syncope is   ",
   "very common in Latin (first year texts excepted).  Default is Y(es).  ",
   "This processing is turned off with the choice of N(o).                " );

DO_TWO_WORDS_HELP : constant HELP_TYPE :=  (
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
   

INCLUDE_UNKNOWN_CONTEXT_HELP : constant HELP_TYPE :=  (
   "This option instructs the program, when writing to an UNKNOWNS file,  ",
   "to put out the whole context of the UNKNOWN (the whole input line on  ",
   "which the UNKNOWN was found).  This is appropriate for processing     ",
   "large text files in which it is expected that there will be relatively",
   "few UNKNOWNS.    The main use at the moment is to provide display     ",
   "of the input line on the output file in the case of UNKNOWNS_ONLY.    ");

NO_MEANINGS_HELP : constant HELP_TYPE :=  (  
   "This option instructs the program to omit putting out meanings.       ", 
   "This is only useful for certain dictionary maintenance procedures.    ",
   "The combination not DO_DICTIONARY_FORMS, MEANINGS_ONLY, NO_MEANINGS   ",
   "results in no visible output, except spacing lines.    Default is N)o.");
   

OMIT_ARCHAIC_HELP : constant HELP_TYPE :=  (
   "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
   "This option instructs the program to omit inflections and dictionary  ",
   "entries with an AGE code of A (Archaic).  Archaic results are rarely  ",
   "of interest in general use.  If there is no other possible form, then ",
   "the Archaic (roughly defined) will be reported.  The default is Y(es)." );


OMIT_MEDIEVAL_HELP : constant HELP_TYPE :=  (
   "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
   "This option instructs the program to omit inflections and dictionary  ",
   "entries with AGE codes of E or later, those not in use in Roman times.",
   "While later forms and words are a significant application, most users ",
   "will not want them.  If there is no other possible form, then the     ",
   "Medieval (roughly defined) will be reported.   The default is Y(es).  " );

OMIT_UNCOMMON_HELP : constant HELP_TYPE :=  (
   "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
   "This option instructs the program to omit inflections and dictionary  ",
   "entries with FREQ codes indicating that the selection is uncommon.    ",
   "While these forms area significant feature of the program, many users ",
   "will not want them.  If there is no other possible form, then the     ",
   "uncommon (roughly defined) will be reported.   The default is Y(es).  " );

DO_I_FOR_J_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to modify the output so that the j/J",
   "is represented as i/I.  The consonant i was writen as j in cursive in ",
   "Imperial times and called i longa, and often rendered as j in medieval",
   "times.  The capital is usually rendered as I, as in inscriptions.     ",
   "If this is NO/FALSE, the output will have the same character as input.",
   "The program default, and the dictionary convention is to retain the j.",
   "Reset if this ia unsuitable for your application. The default is N(o)." );

DO_U_FOR_V_HELP : constant HELP_TYPE :=  (
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



PAUSE_IN_SCREEN_OUTPUT_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to pause in output on the screen    ",
   "after about 16 lines so that the user can read the output, otherwise  ",
   "it would just scroll off the top.  A RETURN/ENTER gives another page. ",
   "If the program is waiting for a return, it cannot take other input.   ",
   "This option is active only for keyboard entry or command line input,  ",
   "and only when there is no output file.  It is moot if only single word",
   "input or brief output.                 The default is Y(es).          " );


NO_SCREEN_ACTIVITY_HELP : constant HELP_TYPE :=  (
   "This option instructs the program not to keep a running screen of the ",
   "input.  This is probably only to be used by the developer to calibrate",
   "run times for large text file input, removing the time necessary to   ",
   "write to screen.                       The default is N(o).           ");
 
    

UPDATE_LOCAL_DICTIONARY_HELP : constant HELP_TYPE :=  (
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

UPDATE_MEANINGS_HELP : constant HELP_TYPE :=  (
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
     

MINIMIZE_OUTPUT_HELP : constant HELP_TYPE :=  (
   "This option instructs the program to minimize the output.  This is a  ",
   "somewhat flexible term, but the use of this option will probably lead ",
   "to less output.                        The default is Y(es).          " );


SAVE_PARAMETERS_HELP : constant HELP_TYPE :=  (
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

  procedure PUT(HELP : HELP_TYPE) is
  begin
    NEW_LINE;
    for I in HELP'FIRST..HELP'LAST  loop
      PUT_LINE(HELP(I));
    end loop;
    NEW_LINE;
  end PUT;


  procedure UPDATE_LOCAL_DICTIONARY_FILE is
    use TEXT_IO;
    BLANK_LINE : STRING(1..80) := (others => ' ');
    LINE, STEM_LINE, PART_LINE, MEAN_LINE : STRING(1..80) := BLANK_LINE;
    L, SL, PL, ML : INTEGER := 0;    --  SL BAD NAME !!!!!!!!!!!
    --DICT_LOC : DICTIONARY;   --  Def in LINE_STUFF
    DICT_LOC_FILE : FILE_TYPE;
    DUMMY : FILE_TYPE;
                                               --  Omit when put name here
    DICT_LOC_NAME : constant STRING :=
                    ADD_FILE_NAME_EXTENSION(DICTIONARY_FILE_NAME, "LOCAL");

    procedure READY_DICT_LOC_FILE is
    --  Effectively goes to the end of DICT_LOC to ready for appending
    --  Does this by making a new file and writing the old DICT_LOC into it
    --  If there is not already a DICT_LOC, it creates one
    begin
      OPEN(DICT_LOC_FILE, IN_FILE, DICT_LOC_NAME);
      CREATE(DUMMY, OUT_FILE);
      while not END_OF_FILE(DICT_LOC_FILE)  loop
        GET_LINE(DICT_LOC_FILE, LINE, L);
        PUT_LINE(DUMMY, LINE(1..L));
      end loop;
      RESET(DUMMY, IN_FILE);
      DELETE(DICT_LOC_FILE);     --  Might RESET, but environment might not support
      CREATE(DICT_LOC_FILE, OUT_FILE, DICT_LOC_NAME);
      while not END_OF_FILE(DUMMY)  loop
        GET_LINE(DUMMY, LINE, L);
        PUT_LINE(DICT_LOC_FILE, LINE(1..L));
      end loop;
      DELETE(DUMMY);
    exception
      when NAME_ERROR  =>
        CREATE(DICT_LOC_FILE, OUT_FILE, DICT_LOC_NAME);
    end READY_DICT_LOC_FILE;

    procedure APPEND_TO_DICT_LOC_FILE is
    --  This just appends the 3 lines of a dictionary entry to DICT_LOC
    --  It prepares the file to write at the end, writes, then closes it
    begin
      READY_DICT_LOC_FILE;
      PUT_LINE(DICT_LOC_FILE, STEM_LINE(1..SL));   --  SL bad name
      PUT(DICT_LOC_FILE, PART_LINE(1..PL));
      PUT_LINE(DICT_LOC_FILE, " X X X X X ");
      PUT_LINE(DICT_LOC_FILE, MEAN_LINE(1..ML));

      CLOSE(DICT_LOC_FILE);

    end APPEND_TO_DICT_LOC_FILE;

  begin
    loop

      TEXT_IO.PUT("STEMS =>");
      GET_LINE(STEM_LINE, SL);
      if SL > 0  then  --  if no input for stems, then just skip the entry
        TEXT_IO.PUT("PART  =>");
        GET_LINE(PART_LINE, PL);
        TEXT_IO.PUT("MEAN =>");
        GET_LINE(MEAN_LINE, ML);
      else
        exit;       --  on no entry, just CR
      end if;

      begin
        APPEND_TO_DICT_LOC_FILE;



DICT_LOC := NULL_DICTIONARY;
LOAD_DICTIONARY(DICT_LOC,
                        ADD_FILE_NAME_EXTENSION(DICTIONARY_FILE_NAME, "LOCAL"));
--  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
        LOAD_STEM_FILE(LOCAL);
        DICTIONARY_AVAILABLE(LOCAL) := TRUE;
        exit;       --  If everything OK, otherwise loop back and try again
      end;

    end loop;

  end UPDATE_LOCAL_DICTIONARY_FILE;





  procedure PUT_MDEVS is
    use MDEV_TYPE_IO;
    use REPLY_TYPE_IO;
  begin
    if IS_OPEN(MDEV_FILE)  then
      CLOSE(MDEV_FILE);
    end if;
    CREATE(MDEV_FILE, OUT_FILE, MDEV_FULL_NAME);
    for I in WORDS_MDEV'RANGE  loop
      PUT(MDEV_FILE, I);
      SET_COL(MDEV_FILE, 35);
      PUT(MDEV_FILE, REPLY(WORDS_MDEV(I)));
      NEW_LINE(MDEV_FILE);
    end loop;
    PUT(MDEV_FILE, "START_FILE_CHARACTER             '" &
                    START_FILE_CHARACTER &"'"); NEW_LINE(MDEV_FILE);
    PUT(MDEV_FILE, "CHANGE_PARAMETERS_CHARACTER      '" &
                    CHANGE_PARAMETERS_CHARACTER &"'"); NEW_LINE(MDEV_FILE);
    PUT(MDEV_FILE, "CHANGE_DEVELOPER_MODES_CHARACTER '" &
                    CHANGE_DEVELOPER_MODES_CHARACTER &"'"); NEW_LINE(MDEV_FILE);
    CLOSE(MDEV_FILE);
  end PUT_MDEVS;

  procedure GET_MDEVS is
    use MDEV_TYPE_IO;
    use REPLY_TYPE_IO;
    MO : MDEV_TYPE;
    REP : REPLY_TYPE;
    LINE : STRING(1..100) := (others => ' ');
    LAST : INTEGER := 0;
  begin
    OPEN(MDEV_FILE, IN_FILE, MDEV_FULL_NAME);
    for I in WORDS_MDEV'RANGE  loop
      GET(MDEV_FILE, MO);
      GET(MDEV_FILE, REP);
      WORDS_MDEV(MO) := MDEV_OF_REPLY(REP);
    end loop;
    SKIP_LINE(MDEV_FILE);

    GET_LINE(MDEV_FILE, LINE, LAST);
    if LINE(1..20) = "START_FILE_CHARACTER"  then
      if ((LINE(35) in '!'..'/')  or
          (LINE(35) in ':'..'@')  or
          (LINE(35) in '['..'`')  or
          (LINE(35) in '{'..'~'))  and
          (LINE(35) /= CHANGE_PARAMETERS_CHARACTER)  and
          (LINE(35) /= CHANGE_DEVELOPER_MODES_CHARACTER)  then
        START_FILE_CHARACTER := LINE(35);
      else
        PUT_LINE("Not an acceptable START_FILE_CHARACTER, may conflict");
        PUT_LINE("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      end if;
    else
      raise BAD_MDEV_FILE;
    end if;

    GET_LINE(MDEV_FILE, LINE, LAST);
    if LINE(1..27) = "CHANGE_PARAMETERS_CHARACTER"  then
      if ((LINE(35) in '!'..'/')  or
          (LINE(35) in ':'..'@')  or
          (LINE(35) in '['..'`')  or
          (LINE(35) in '{'..'~'))  and
          (LINE(35) /= START_FILE_CHARACTER)  and
          (LINE(35) /= CHANGE_DEVELOPER_MODES_CHARACTER)  then
        CHANGE_PARAMETERS_CHARACTER := LINE(35);
      else
        PUT_LINE("Not an acceptable CHANGE_PARAMETERS_CHARACTER, may conflict");
        PUT_LINE("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      end if;
    else
      raise BAD_MDEV_FILE;
    end if;

    GET_LINE(MDEV_FILE, LINE, LAST);
    if LINE(1..32) = "CHANGE_DEVELOPER_MODES_CHARACTER"  then
      if ((LINE(35) in '!'..'/')  or
          (LINE(35) in ':'..'@')  or
          (LINE(35) in '['..'`')  or
          (LINE(35) in '{'..'~'))  and
          (LINE(35) /= START_FILE_CHARACTER)  and
          (LINE(35) /= CHANGE_PARAMETERS_CHARACTER)  then
        CHANGE_DEVELOPER_MODES_CHARACTER := LINE(35);
      else
        PUT_LINE("Not an acceptable CHANGE_DEVELOPER_MODES_CHARACTER, may conflict");
        PUT_LINE("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      end if;
    else
      raise BAD_MDEV_FILE;
    end if;
    CLOSE(MDEV_FILE);

  exception
    when NAME_ERROR  =>
      raise;
    when others =>
      raise BAD_MDEV_FILE;
  end GET_MDEVS;

  procedure INQUIRE(MO : MDEV_TYPE; HELP : in HELP_TYPE := NO_HELP) is
    use MDEV_TYPE_IO;
    use REPLY_TYPE_IO;
    L1 : STRING(1..100);
    LL : NATURAL;
    R  : REPLY_TYPE;
  begin
    PUT(MO);
    PUT(" ?  "); SET_COL(45); PUT("(Currently  ");
    PUT(REPLY(WORDS_MDEV(MO))); PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if TRIM(L1(1..LL)) = ""  then
        PUT_LINE("Blank input, skipping the rest of CHANGE_DEVELOPER_MODES");
        raise BLANK_INPUT;
      elsif L1(1) = '?'  then
        PUT(HELP);
        INQUIRE(MO, HELP);
      else
        GET(L1(1..LL), R, LL);
        WORDS_MDEV(MO) := MDEV_OF_REPLY(R);
      end if;
    end if;
    NEW_LINE;
  end INQUIRE;


  procedure CHANGE_DEVELOPER_MODES is
    L1 : STRING(1..100);
    LL : NATURAL;
    R  : REPLY_TYPE;

  begin
    
  PUT_LINE("To set developer modes reply Y/y or N/n.  Return accepts current value.");
  PUT_LINE("A '?' reply gives infomation/help on that parameter.  A space skips the rest.");
  PUT_LINE("Developer modes are only for special requirements and may not all be operable.");
  NEW_LINE;
  
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

    INQUIRE(HAVE_STATISTICS_FILE, HAVE_STATISTICS_FILE_HELP);
    if IS_OPEN(STATS)  and then not WORDS_MDEV(HAVE_STATISTICS_FILE)  then
      DELETE(STATS);
      WORDS_MDEV(WRITE_STATISTICS_FILE) := FALSE;
    end if;
    if not IS_OPEN(STATS) and then WORDS_MDEV(HAVE_STATISTICS_FILE)  then
      begin
        CREATE(STATS, OUT_FILE, STATS_FULL_NAME);
      exception
        when others =>
          PUT_LINE("Cannot CREATE WORD.STA - Check if it is in use elsewhere");
      end;
    end if;

    if WORDS_MDEV(HAVE_STATISTICS_FILE)  then
      INQUIRE(WRITE_STATISTICS_FILE, WRITE_STATISTICS_FILE_HELP);
    end if;

    INQUIRE(DO_ONLY_INITIAL_WORD, DO_ONLY_INITIAL_WORD_HELP);
    if WORDS_MDEV(DO_ONLY_INITIAL_WORD)  then
      INQUIRE(FOR_WORD_LIST_CHECK, FOR_WORD_LIST_CHECK_HELP);
    else
      WORDS_MDEV(FOR_WORD_LIST_CHECK) := FALSE;
    end if;



    INQUIRE(SHOW_DICTIONARY, SHOW_DICTIONARY_HELP);

    INQUIRE(SHOW_DICTIONARY_LINE, SHOW_DICTIONARY_LINE_HELP);

    INQUIRE(SHOW_DICTIONARY_CODES, SHOW_DICTIONARY_CODES_HELP);

    INQUIRE(DO_PEARSE_CODES, DO_PEARSE_CODES_HELP);


    if WORDS_MODE(DO_FIXES) then
      INQUIRE(DO_ONLY_FIXES, DO_ONLY_FIXES_HELP);
      INQUIRE(DO_FIXES_ANYWAY, DO_FIXES_ANYWAY_HELP);
    end if;

    INQUIRE(USE_PREFIXES, USE_PREFIXES_HELP);
    
    INQUIRE(USE_SUFFIXES, USE_SUFFIXES_HELP);
    
    INQUIRE(USE_TACKONS, USE_TACKONS_HELP);
    
        
    if WORDS_MODE(DO_TRICKS) then
      INQUIRE(DO_MEDIEVAL_TRICKS, DO_MEDIEVAL_TRICKS_HELP);
    end if;


    INQUIRE(DO_SYNCOPE, DO_SYNCOPE_HELP);

    INQUIRE(DO_TWO_WORDS, DO_TWO_WORDS_HELP);

    INQUIRE(INCLUDE_UNKNOWN_CONTEXT, INCLUDE_UNKNOWN_CONTEXT_HELP);

    INQUIRE(NO_MEANINGS, NO_MEANINGS_HELP);


    INQUIRE(OMIT_ARCHAIC, OMIT_ARCHAIC_HELP);

    INQUIRE(OMIT_MEDIEVAL, OMIT_MEDIEVAL_HELP);

    INQUIRE(OMIT_UNCOMMON, OMIT_UNCOMMON_HELP);


    INQUIRE(DO_I_FOR_J, DO_I_FOR_J_HELP);

    INQUIRE(DO_U_FOR_V, DO_U_FOR_V_HELP);


    INQUIRE(PAUSE_IN_SCREEN_OUTPUT, PAUSE_IN_SCREEN_OUTPUT_HELP);
    
    INQUIRE(NO_SCREEN_ACTIVITY, NO_SCREEN_ACTIVITY_HELP);        

    
    
    INQUIRE(UPDATE_LOCAL_DICTIONARY, UPDATE_LOCAL_DICTIONARY_HELP);

    INQUIRE(UPDATE_MEANINGS, UPDATE_MEANINGS_HELP);


    INQUIRE(MINIMIZE_OUTPUT, MINIMIZE_OUTPUT_HELP);



    PUT("START_FILE_CHARACTER ?  "); SET_COL(45); PUT("(Currently  '");
    PUT(START_FILE_CHARACTER); PUT("'");
    PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if ((L1(1) in '!'..'/')  or
          (L1(1) in ':'..'@')  or
          (L1(1) in '['..'`')  or
          (L1(1) in '{'..'~'))  and
          (L1(1) /= CHANGE_PARAMETERS_CHARACTER)  and
          (L1(1) /= CHANGE_DEVELOPER_MODES_CHARACTER)  then
        START_FILE_CHARACTER := L1(1);
      else
        PUT_LINE("Not an acceptable character, may conflict with other input");
        PUT_LINE("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      end if;
    end if;
    NEW_LINE;

    PUT("CHANGE_PARAMETERS_CHARACTER ?  "); SET_COL(45); PUT("(Currently  '");
    PUT(CHANGE_PARAMETERS_CHARACTER); PUT("'");
    PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if ((L1(1) in '!'..'/')  or
          (L1(1) in ':'..'@')  or
          (L1(1) in '['..'`')  or
          (L1(1) in '{'..'~'))  and
          (L1(1) /= START_FILE_CHARACTER)  and
          (L1(1) /= CHANGE_DEVELOPER_MODES_CHARACTER)  then
        CHANGE_PARAMETERS_CHARACTER := L1(1);
      else
        PUT_LINE("Not an acceptable character, may conflict with other input");
        PUT_LINE("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      end if;
    end if;
    NEW_LINE;


    PUT("CHANGE_DEVELOPER_MODES_CHARACTER ?  ");
    SET_COL(45); PUT("(Currently  '");
    PUT(CHANGE_DEVELOPER_MODES_CHARACTER); PUT("'");
    PUT(" =>");
    GET_LINE(L1, LL);
    if LL /= 0  then
      if ((L1(1) in '!'..'/')  or
          (L1(1) in ':'..'@')  or
          (L1(1) in '['..'`')  or
          (L1(1) in '{'..'~'))  and
          (L1(1) /= START_FILE_CHARACTER)  and
          (L1(1) /= CHANGE_LANGUAGE_CHARACTER)  and
          (L1(1) /= CHANGE_PARAMETERS_CHARACTER)  then
        CHANGE_DEVELOPER_MODES_CHARACTER := L1(1);
      else
        PUT_LINE("Not an acceptable character, may conflict with other input");
        PUT_LINE("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
      end if;
    end if;
    NEW_LINE;

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
      if MDEV_OF_REPLY(R)  then
        PUT_MDEVS;
        PUT_LINE("MDEV_ARRAY saved in file " & MDEV_FULL_NAME);
      end if;
    end if;
    NEW_LINE;

  exception
    when BLANK_INPUT  =>
      null;
    when others =>
      PUT_LINE("Bad input - terminating CHANGE_DEVELOPER_PARAMETERS");

  end CHANGE_DEVELOPER_MODES;


procedure INITIALIZE_DEVELOPER_PARAMETERS is
begin


  DO_MDEV_FILE:
  begin
  --  Read the MDEV file
    GET_MDEVS;
    PREFACE.PUT_LINE("MDEV_FILE found - Using those MDEVs and parameters");
  exception
  --  If there is any problem
  --  Put that the MDEV file is corrupted and the options are:
      --  to proceed with default parameters
      --  to set parameters with a CHANGE (SET) PARAMETERS and save
      --  to examine the MDEV file with a text editor and try to repair it
    when NAME_ERROR  =>
      WORDS_MDEV := DEFAULT_MDEV_ARRAY;
    when BAD_MDEV_FILE  =>
      PREFACE.PUT_LINE("MDEV_FILE exists, but empty or corupted - Default MDEVs used");
      PREFACE.PUT_LINE("You can set new parameters with CHANGE PARAMETERS and save.");
      WORDS_MDEV := DEFAULT_MDEV_ARRAY;
  end DO_MDEV_FILE;


--  if not IS_OPEN(DBG) and then WORDS_MDEV(HAVE_DEBUG_FILE)  then
--    CREATE(DBG, OUT_FILE, DEBUG_FULL_NAME);
--    PREFACE.PUT_LINE("WORD.DBG Created at Initialization");
--  end if;
  if not IS_OPEN(STATS) and then WORDS_MDEV(HAVE_STATISTICS_FILE)  then
    CREATE(STATS, OUT_FILE, STATS_FULL_NAME);
    PREFACE.PUT_LINE("WORD.STA Created at Initialization");
  end if;

end INITIALIZE_DEVELOPER_PARAMETERS;


end DEVELOPER_PARAMETERS;
