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

with Ada.Text_IO; use Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;

--  Omit when Put name here
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Latin_Utils.Preface;
with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
use Latin_Utils;
pragma Elaborate (Latin_Utils.Preface);
package body Support_Utils.Developer_Parameters is

   type Help_Type is array (Natural range <>) of String (1 .. 70);
   Blank_Help_Line : constant String (1 .. 70) := (others => ' ');
   No_Help : constant Help_Type := (2 .. 1 => Blank_Help_Line);

   type Reply_Type is (N, Y);
   package Reply_Type_Io is new Ada.Text_IO.Enumeration_IO (Reply_Type);
   Reply : constant array (Boolean) of Reply_Type := (N, Y);
   Mdev_Of_Reply : constant array (Reply_Type) of Boolean := (False, True);

   Blank_Input : exception;

   --  The default MDEVs are set in the body so that they can be changed
   --  with only this being recompiled, not the rest of the with'ing system
   Default_Mdev_Array : constant Mdev_Array := (

     --               HAVE_DEBUG_FILE             => FALSE,
     --               WRITE_DEBUG_FILE            => FALSE,

     Have_Statistics_File        => False,
     Write_Statistics_File       => False,

     Show_Dictionary             => False,
     Show_Dictionary_Line        => False,
     Show_Dictionary_Codes       => True,
     Do_Pearse_Codes             => False,

     Do_Only_Initial_Word        => False,
     For_Word_List_Check         => False,

     Do_Only_Fixes               => False,
     Do_Fixes_Anyway             => False,
     Use_Prefixes                => True,
     Use_Suffixes                => True,
     Use_Tackons                 => True,

     Do_Medieval_Tricks          => True,

     Do_Syncope                  => True,
     Do_Two_Words                => True,
     Include_Unknown_Context     => True,
     No_Meanings                 => False,

     Omit_Archaic                => True,
     Omit_Medieval               => False,
     Omit_Uncommon               => True,

     Do_I_For_J                  => False,
     Do_U_For_V                  => False,

     Pause_In_Screen_Output      => True,
     No_Screen_Activity          => False,

     Update_Local_Dictionary     => False,
     Update_Meanings             => False,

     Minimize_Output             => True);

   Bad_Mdev_File : exception;

   --HAVE_DEBUG_FILE_HELP : constant HELP_TYPE :=  (
   --  "This option instructs the program to Create a file which can hold     ",
   --  "certain internal information about the current search.  The file is   ",
   --  "overwritten for every word in order to prevent it from growing out of ",
   --  "hand, so information about the last word searched is saved in case of ",
   --  "failure.  The debug Output file is named " & DEBUG_FULL_NAME
   --                             & (42+DEBUG_FULL_NAME'LENGTH .. 70 => ' '),
   --  "Use of this option, along with the WRITE_DEBUG_FILE option may slow   ",
   --  "the program significantly.  This information is usually only useful   ",
   --  "to the developer, so the default is N(o).                            ");
   --
   --WRITE_DEBUG_FILE_HELP : constant HELP_TYPE :=  (
   --  "This option instructs the program, when HAVE_DEBUG_FILE is on, to Put ",
   --  "some debug data to a file named " & DEBUG_FULL_NAME
   --                               & (33+DEBUG_FULL_NAME'LENGTH .. 70 => ' '),
   --   "This option may be turned on and off while running of the program,    "
   --  "thereby capturing only certain desired results.  The file is reset and",
   --  "restarted after each word parsed, so that it does not Get too big.    ",
   --  "If the option HAVE_DEBUG_FILE is off, the user will not be given a    ",
   --  "chance to turn this one on.                  Default is N(o).        ");
   --

   Have_Statistics_File_Help : constant Help_Type :=  (
     "This option instructs the program to Create a file which can hold     ",
     "certain statistical information about the process.  The file is       ",
     "overwritten for new invocation of the program, so old data must be    ",
     "explicitly saved if it is to be retained.  The statistics are in TEXT ",
     "format.     The statistics file is named " & Stats_Full_Name
     & (42 + Stats_Full_Name'Length .. 70 => ' '),
     "This information is only of development use, so the default is N(o).  ");

   Write_Statistics_File_Help : constant Help_Type :=  (
     "This option instructs the program, with HAVE_STATISTICS_FILE, to Put  ",
     "derived statistics in a file named " & Stats_Full_Name
     & (36 + Stats_Full_Name'Length .. 70 => ' '),
     "This option may be turned on and off while running of the program,    ",
     "thereby capturing only certain desired results.  The file is reset at ",
     "each invocation of the program, if the HAVE_STATISTICS_FILE is set.   ",
     "If the option HAVE_STATISTICS_FILE is off, the user will not be given ",
     "a chance to turn this one on.                Default is N(o).         ");

   Show_Dictionary_Help : constant Help_Type :=  (
     "This option causes a flag, like 'GEN>' to be Put before the meaning   ",
     "in the Output.  While this is useful for certain development purposes,",
     "it forces off a few Characters from the meaning, and is really of no  ",
     "interest to most users.                                               ",
     "The default choice is N(o), but it can be turned on with a Y(es).     ");

   Show_Dictionary_Line_Help : constant Help_Type :=  (
     "This option causes the number of the dictionary line for the current  ",
     "meaning to be Output.  This is of use to no one but the dictionary    ",
     "maintainer.  The default choice is N(o).  It is activated by Y(es).   ");

   Show_Dictionary_Codes_Help : constant Help_Type :=  (
     "This option causes the codes for the dictionary entry for the current ",
     "meaning to be Output.  This may not be useful to any but the most     ",
     "involved user.  The default choice is N(o).  It is activated by Y(es).");

   Do_Pearse_Codes_Help : constant Help_Type :=  (
     "This option causes special codes to be Output flagging the different  ",
     "kinds of Output lines.  01 for forms, 02 for dictionary forms, and    ",
     "03 for meaning. The default choice is N(o).  It is activated by Y(es).",
     "There are no Pearse codes in English mode.                            ");

   Do_Only_Initial_Word_Help : constant Help_Type :=  (
     "This option instructs the program to only analyze the initial word on ",
     "each line submitted.  This is a tool for checking and integrating new ",
     "dictionary Input, and will be of no interest to the general user.     ",
     "The default choice is N(o), but it can be turned on with a Y(es).     ");

   For_Word_List_Check_Help : constant Help_Type :=  (
     "This option works in conjunction with DO_ONLY_INITIAL_WORD to allow   ",
     "the processing of scanned dictionaries or text word lists.  It accepts",
     "only the forms common in dictionary entries, like NOM S for N or ADJ, ",
     "or PRES ACTIVE IND 1 S for V.  It is be used only with DO_INITIAL_WORD",
     "The default choice is N(o), but it can be turned on with a Y(es).     ");

   Do_Only_Fixes_Help : constant Help_Type :=  (
     "This option instructs the program to ignore the normal dictionary     ",
     "search and to go direct to attach various prefixes and suffixes before",
     "processing. This is a pure research tool.  It allows one to examine   ",
     "the coverage of pure stems and dictionary primary compositions.       ",
     "This option is only available if DO_FIXES is turned on.               ",
     "This is entirely a development and research tool, not to be used in   ",
     "conventional translation situations, so the default choice is N(o).   ",
     "This processing can be turned on with the choice of Y(es).            ");

   Do_Fixes_Anyway_Help : constant Help_Type :=  (
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
     "      ------    PRESENTLY NOT IMPLEMENTED    ------                   ");

   Use_Prefixes_Help : constant Help_Type :=  (
     "This option instructs the program to implement prefixes from ADDONS   ",
     "whenever and wherever FIXES are called for.  The purpose of this      ",
     "option is to allow some flexibility while the program in running to   ",
     "select various combinations of fixes, to turn them on and off,        ",
     "individually as well as collectively.  This is an option usually      ",
     "employed by the developer while experimenting with the ADDONS file.   ",
     "This option is only effective in connection with DO_FIXES.            ",
     "This is primarily a development tool, so the conventional user should ",
     "probably maintain the default  choice of Y(es).                       ");

   Use_Suffixes_Help : constant Help_Type :=  (
     "This option instructs the program to implement suffixes from ADDONS   ",
     "whenever and wherever FIXES are called for.  The purpose of this      ",
     "option is to allow some flexibility while the program in running to   ",
     "select various combinations of fixes, to turn them on and off,        ",
     "individually as well as collectively.  This is an option usually      ",
     "employed by the developer while experimenting with the ADDONS file.   ",
     "This option is only effective in connection with DO_FIXES.            ",
     "This is primarily a development tool, so the conventional user should ",
     "probably maintain the default  choice of Y(es).                       ");

   Use_Tackons_Help : constant Help_Type :=  (
     "This option instructs the program to implement TACKONS from ADDONS    ",
     "whenever and wherever FIXES are called for.  The purpose of this      ",
     "option is to allow some flexibility while the program in running to   ",
     "select various combinations of fixes, to turn them on and off,        ",
     "individually as well as collectively.  This is an option usually      ",
     "employed by the developer while experimenting with the ADDONS file.   ",
     "This option is only effective in connection with DO_FIXES.            ",
     "This is primarily a development tool, so the conventional user should ",
     "probably maintain the default  choice of Y(es).                       ");

   Do_Medieval_Tricks_Help : constant Help_Type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, and after various prefixes and suffixes, and ",
     "trying every Classical Latin trick it can think of, to go to a few    ",
     "that are usually only found in medieval Latin, replacements of z -> di",
     "caul -> col, st -> est, ix -> is, nct -> nt. It also tries some things",
     "like replacing doubled consonants in classical with a single one.     ",
     "Together these tricks are useful, but may give False Positives (>20%).",
     "This option is only available if the general DO_TRICKS is chosen.     ",
     "If the text is late or medieval, this option is much more useful than ",
     "tricks for classical.  The dictionary can never contain all spelling  ",
     "variations found in medieval Latin, but some constructs are common.   ",
     "The default choice is N(o), since the results are iffy, medieval only,",
     "and expensive.  This processing is turned on with the choice of Y(es).");

   Do_Syncope_Help : constant Help_Type :=  (
     "This option instructs the program to postulate that syncope of        ",
     "perfect stem verbs may have occurred (e.g, aver -> ar in the perfect),",
     "and to try various possibilities for the insertion of a removed 'v'.  ",
     "To do this it has to fully process the modified candidates, which can ",
     "have a considerable impact on the speed of processing a large file.   ",
     "However, this trick seldom produces a False Positive, and syncope is  ",
     "very common in Latin (first year texts excepted).  Default is Y(es).  ",
     "This processing is turned off with the choice of N(o).                ");

   Do_Two_Words_Help : constant Help_Type :=  (
     "There are some few common Latin expressions that combine two inflected",
     "words (e.g. respublica, paterfamilias).  There are numerous examples  ",
     "of numbers composed of two words combined together.                   ",
     "Sometimes a text or inscription will have words run together.         ",
     "When WORDS is unable to reach a satisfactory solution with all other  ",
     "tricks, as a last stab it will try to break the Input into two words. ",
     "This most often fails.  Even if mechanically successful, the result is",
     "usually False and must be examined by the user.  If the result is     ",
     "correct, it is probably clear to the user.  Otherwise,  beware.  .    ",
     "Since this is a last chance and infrequent, the default is Y(es);     ",
     "This processing is turned off with the choice of N(o).                ");

   Include_Unknown_Context_Help : constant Help_Type :=  (
     "This option instructs the program, when writing to an UNKNOWNS file,  ",
     "to Put out the whole context of the UNKNOWN (the whole Input line on  ",
     "which the UNKNOWN was found).  This is appropriate for processing     ",
     "large text files in which it is expected that there will be relatively",
     "few UNKNOWNS.    The main use at the moment is to provide display     ",
     "of the Input line on the Output file in the case of UNKNOWNS_ONLY.    ");

   No_Meanings_Help : constant Help_Type :=  (
     "This option instructs the program to omit Putting out meanings.       ",
     "This is only useful for certain dictionary maintenance procedures.    ",
     "The combination not DO_DICTIONARY_FORMS, MEANINGS_ONLY, NO_MEANINGS   ",
     "results in no visible Output, except spacing lines.    Default is N)o.");

   Omit_Archaic_Help : constant Help_Type :=  (
     "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
     "This option instructs the program to omit inflections and dictionary  ",
     "entries with an AGE code of A (Archaic).  Archaic results are rarely  ",
     "of interest in general use.  If there is no other possible form, then ",
     "the Archaic (roughly defined) will be reported.  The default is Y(es).");

   Omit_Medieval_Help : constant Help_Type :=  (
     "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
     "This option instructs the program to omit inflections and dictionary  ",
     "entries with AGE codes of E or later, those not in use in Roman times.",
     "While later forms and words are a significant application, most users ",
     "will not want them.  If there is no other possible form, then the     ",
     "Medieval (roughly defined) will be reported.   The default is Y(es).  ");

   Omit_Uncommon_Help : constant Help_Type :=  (
     "THIS OPTION IS CAN ONLY BE ACTIVE IF WORDS_MODE(TRIM_OUTPUT) IS SET!  ",
     "This option instructs the program to omit inflections and dictionary  ",
     "entries with FREQ codes indicating that the selection is uncommon.    ",
     "While these forms area significant feature of the program, many users ",
     "will not want them.  If there is no other possible form, then the     ",
     "uncommon (roughly defined) will be reported.   The default is Y(es).  ");

   Do_I_For_J_Help : constant Help_Type :=  (
     "This option instructs the program to modify the Output so that the j/J",
     "is represented as i/I.  The consonant i was Written as j in cursive in",
     "Imperial times and called i longa, and often rendered as j in medieval",
     "times.  The capital is usually rendered as I, as in inscriptions.     ",
     "If this is NO/FALSE, the Output will have the same Character as Input.",
     "The program default, and the dictionary convention is to retain the j.",
     "Reset if this is unsuitable for your application. The default is N(o).");

   Do_U_For_V_Help : constant Help_Type :=  (
     "This option instructs the program to modify the Output so that the u  ",
     "is represented as v.  The consonant u was Written sometimes as uu.    ",
     "The pronunciation was as current w, and important for poetic meter.   ",
     "With the printing press came the practice of distinguishing consonant ",
     "u with the Character v, and was common for centuries.  The practice of",
     "using only u has been adopted in some 20th century publications (OLD),",
     " but it is confusing to many modern readers.  The capital is commonly ",
     "V in any case, as it was and is in inscriptions (easier to chisel).   ",
     "If this is NO/FALSE, the Output will have the same Character as Input.",
     "The program default, and the dictionary convention is to retain the v.",
     "Reset If this is unsuitable for your application. The default is N(o).");

   Pause_In_Screen_Output_Help : constant Help_Type :=  (
     "This option instructs the program to pause in Output on the screen    ",
     "after about 16 lines so that the user can read the Output, otherwise  ",
     "it would just scroll off the top.  A RETURN/ENTER gives another page. ",
     "If the program is waiting for a return, it cannot take other Input.   ",
     "This option is active only for keyboard entry or command line Input,  ",
     "and only when there is no Output file.  It is moot if only single word",
     "Input or brief Output.                 The default is Y(es).          ");

   No_Screen_Activity_Help : constant Help_Type :=  (
     "This option instructs the program not to keep a running screen of the ",
     "Input.  This is probably only to be used by the developer to calibrate",
     "run times for large text file Input, removing the time necessary to   ",
     "Write to screen.                       The default is N(o).           ");

   Update_Local_Dictionary_Help : constant Help_Type :=  (
     "This option instructs the program to invite the user to Input a new   ",
     "word to the local dictionary on the fly.  This is only active if the  ",
     "program is not using an (@) Input file!  If an UNKNOWN is discovered, ",
     "the program asks for STEM, PART, and MEAN, the basic elements of a    ",
     "dictionary entry.  These are Put into the local dictionary right then,",
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
     "      ------    NOT AVAILABLE IN THIS VERSION   -------               ");

   Update_Meanings_Help : constant Help_Type :=  (
     "This option instructs the program to invite the user to modify the    ",
     "meaning displayed on a word translation.  This is only active if the  ",
     "program is not using an (@) Input file!  These changes are Put into   ",
     "the dictionary right then and permanently, and are available from     ",
     "then on, in this session, and all later sessions.   Unfortunately,    ",
     "these changes will not survive the replacement of the dictionary by a ",
     "new version from the developer.  Changes can only be recovered by     ",
     "considerable processing by the developer, and should be left there.   ",
     "This option is only for experienced users and should remain off.      ",
     "                                          The default is N(o).        ",
     "      ------    NOT AVAILABLE IN THIS VERSION   -------               ");

   Minimize_Output_Help : constant Help_Type :=  (
     "This option instructs the program to minimize the Output.  This is a  ",
     "somewhat flexible term, but the use of this option will probably lead ",
     "to less Output.                        The default is Y(es).          ");

   Save_Parameters_Help : constant Help_Type :=  (
     "This option instructs the program, to save the current parameters, as ",
     "just established by the user, in a file WORD.MDV.  If such a file     ",
     "exists, the program will load those parameters at the start.  If no   ",
     "such file can be found in the current subdirectory, the program will  ",
     "start with a default set of parameters.  Since this parameter file is ",
     "human-readable ASCII, it may also be Created with a text editor.  If  ",
     "the file found has been improperly Created, is in the wrong format, or",
     "otherwise uninterpretable by the program, it will be ignored and the  ",
     "default parameters used, until a proper parameter file in written by  ",
     "the program.  Since one may want to make temporary changes during a   ",
     "run, but revert to the usual set, the default is N(o).                ");

   procedure Put (Help : Help_Type) is
   begin
      New_Line;
      for I in Help'First .. Help'Last  loop
         Put_Line (Help (I));
      end loop;
      New_Line;
   end Put;

   procedure Update_Local_Dictionary_File is
      Blank_Line : constant String (1 .. 80) := (others => ' ');
      Line, Stem_Line, Part_Line, Mean_Line : String (1 .. 80) := Blank_Line;
      L, Sl, Pl, Ml : Integer := 0;    --  SL BAD NAME !!!!!!!!!!!
                                       --  DICT_LOC : DICTIONARY;
                                       --  Def in LINE_STUFF
      Dict_Loc_File : File_Type;
      Dummy : File_Type;
      --  Omit when Put name here
      Dict_Loc_Name : constant String :=
        Add_File_Name_Extension (Dictionary_File_Name, "LOCAL");

      procedure Ready_Dict_Loc_File is
         --  Effectively goes to the end of DICT_LOC to ready for appending
         --  Does this by making a new file and writing the old DICT_LOC into it
         --  If there is not already a DICT_LOC, it Creates one
      begin
         Open (Dict_Loc_File, In_File, Dict_Loc_Name);
         Create (Dummy, Out_File);
         while not End_Of_File (Dict_Loc_File)  loop
            Get_Line (Dict_Loc_File, Line, L);
            Put_Line (Dummy, Line (1 .. L));
         end loop;
         Reset (Dummy, In_File);
         --  Might RESET, but environment might not support
         Delete (Dict_Loc_File);
         Create (Dict_Loc_File, Out_File, Dict_Loc_Name);
         while not End_Of_File (Dummy)  loop
            Get_Line (Dummy, Line, L);
            Put_Line (Dict_Loc_File, Line (1 .. L));
         end loop;
         Delete (Dummy);
      exception
         when Name_Error  =>
            Create (Dict_Loc_File, Out_File, Dict_Loc_Name);
      end Ready_Dict_Loc_File;

      procedure Append_To_Dict_Loc_File is
         --  This just appends the 3 lines of a dictionary entry to DICT_LOC
         --  It prepares the file to Write at the end, Writes, then Closes it
      begin
         Ready_Dict_Loc_File;
         Put_Line (Dict_Loc_File, Stem_Line (1 .. Sl));   --  SL bad name
         Put (Dict_Loc_File, Part_Line (1 .. Pl));
         Put_Line (Dict_Loc_File, " X X X X X ");
         Put_Line (Dict_Loc_File, Mean_Line (1 .. Ml));

         Close (Dict_Loc_File);

      end Append_To_Dict_Loc_File;

   begin
      loop

         Ada.Text_IO.Put ("STEMS =>");
         Get_Line (Stem_Line, Sl);
         if Sl > 0  then  --  if no Input for stems, then just skip the entry
            Ada.Text_IO.Put ("PART  =>");
            Get_Line (Part_Line, Pl);
            Ada.Text_IO.Put ("MEAN =>");
            Get_Line (Mean_Line, Ml);
         else
            exit;       --  on no entry, just CR
         end if;

         begin
            Append_To_Dict_Loc_File;

            Dict_Loc := Null_Dictionary;
            Load_Dictionary (Dict_Loc,
              Add_File_Name_Extension (Dictionary_File_Name, "LOCAL"));
            --  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
            Load_Stem_File (Local);
            Dictionary_Available (Local) := True;
            exit;       --  If everything OK, otherwise loop back and try again
         end;

      end loop;

   end Update_Local_Dictionary_File;

   -- Is C one of: '!' .. '/', ':' .. '@', '[' .. '`', '{' .. '~'
   function Is_Special_Graphical (C : Character) return Boolean
   is
      Special_Graphical : constant array (Character) of Boolean :=
        ('!' .. '/' => True, ':' .. '@' => True,
        '[' .. '`' => True, '{' .. '~' => True,
        others => False
        );
   begin
      return Special_Graphical (C);
   end Is_Special_Graphical;

   procedure Put_Mdevs is
      use Mdev_Type_Io;
      use Reply_Type_Io;
   begin
      if Is_Open (Mdev_File)  then
         Close (Mdev_File);
      end if;
      Create (Mdev_File, Out_File, Mdev_Full_Name);
      for I in Words_Mdev'Range  loop
         Put (Mdev_File, I);
         Set_Col (Mdev_File, 35);
         Put (Mdev_File, Reply (Words_Mdev (I)));
         New_Line (Mdev_File);
      end loop;
      Put (Mdev_File, "START_FILE_CHARACTER             '" &
        Start_File_Character & "'");
      New_Line (Mdev_File);
      Put (Mdev_File, "CHANGE_PARAMETERS_CHARACTER      '" &
        Change_Parameters_Character & "'");
      New_Line (Mdev_File);
      Put (Mdev_File, "CHANGE_DEVELOPER_MODES_CHARACTER '" &
        Change_Developer_Modes_Character & "'");
      New_Line (Mdev_File);
      Close (Mdev_File);
   end Put_Mdevs;

   procedure Get_Mdevs is
      use Mdev_Type_Io;
      use Reply_Type_Io;
      Mo : Mdev_Type;
      Rep : Reply_Type;
      Line : String (1 .. 100) := (others => ' ');
      Last : Integer := 0;
   begin
      Open (Mdev_File, In_File, Mdev_Full_Name);
      for I in Words_Mdev'Range  loop
         Get (Mdev_File, Mo);
         Get (Mdev_File, Rep);
         Words_Mdev (Mo) := Mdev_Of_Reply (Rep);
      end loop;
      Skip_Line (Mdev_File);

      Get_Line (Mdev_File, Line, Last);
      if Line (1 .. 20) = "START_FILE_CHARACTER"  then
         if Is_Special_Graphical (Line (35)) and
           (Line (35) /= Change_Parameters_Character)  and
           (Line (35) /= Change_Developer_Modes_Character)
         then
            Start_File_Character := Line (35);
         else
            Put_Line ("Not an acceptable START_FILE_CHARACTER, may conflict");
            Put_Line ("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      else
         raise Bad_Mdev_File;
      end if;

      Get_Line (Mdev_File, Line, Last);
      if Line (1 .. 27) = "CHANGE_PARAMETERS_CHARACTER"  then
         if Is_Special_Graphical (Line (35)) and
           (Line (35) /= Start_File_Character)  and
           (Line (35) /= Change_Developer_Modes_Character)
         then
            Change_Parameters_Character := Line (35);
         else
            Put_Line
              ("Not an acceptable CHANGE_PARAMETERS_CHARACTER, may conflict");
            Put_Line ("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      else
         raise Bad_Mdev_File;
      end if;

      Get_Line (Mdev_File, Line, Last);
      if Line (1 .. 32) = "CHANGE_DEVELOPER_MODES_CHARACTER"  then
         if Is_Special_Graphical (Line (35)) and
           (Line (35) /= Start_File_Character)  and
           (Line (35) /= Change_Parameters_Character)
         then
            Change_Developer_Modes_Character := Line (35);
         else
            Put_Line ("Not an acceptable CHANGE_DEVELOPER_MODES_CHARACTER," &
              " may conflict");
            Put_Line ("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      else
         raise Bad_Mdev_File;
      end if;
      Close (Mdev_File);

   exception
      when Name_Error  =>
         raise;
      when others =>
         raise Bad_Mdev_File;
   end Get_Mdevs;

   procedure Inquire (Mo : Mdev_Type; Help : in Help_Type := No_Help) is
      use Mdev_Type_Io;
      use Reply_Type_Io;
      L1 : String (1 .. 100);
      Ll : Natural;
      R  : Reply_Type;
   begin
      Put (Mo);
      Put (" ?  "); Set_Col (45); Put ("(Currently  ");
      Put (Reply (Words_Mdev (Mo))); Put (" =>");
      Get_Line (L1, Ll);
      if Ll /= 0  then
         if Trim (L1 (1 .. Ll)) = ""  then
            Put_Line
              ("Blank Input, skipping the rest of CHANGE_DEVELOPER_MODES");
            raise Blank_Input;
         elsif L1 (1) = '?'  then
            Put (Help);
            Inquire (Mo, Help);
         else
            Get (L1 (1 .. Ll), R, Ll);
            Words_Mdev (Mo) := Mdev_Of_Reply (R);
         end if;
      end if;
      New_Line;
   end Inquire;

   procedure Change_Developer_Modes is
      L1 : String (1 .. 100);
      Ll : Natural;
      R  : Reply_Type;

   begin

      Put_Line ("To set developer modes reply Y/y or N/n." &
        "  Return accepts current value.");
      Put_Line ("A '?' reply gives information/help on that parameter." &
        "  A space skips the rest.");
      Put_Line ("Developer modes are only for special requirements and " &
        "may not all be operable.");
      New_Line;

      --  Interactive MDEV - lets you do things on unknown words

      --  You can say it is a noun and then look at the endings
      --  Or look all the endings and guess what part of speech

      --  You can look at the dictionary items that are Close to the word
      --  There may be cases in which the stem is found but is not of right part
      --  So maybe the word list is deficient and that root goes also to a ADJ
      --  even if it is listed only for a N.
      --  One can also look for ADV here with ending 'e', etc.

      --  You can look up the word in a paper dictionary (with the help of
      --  ending)
      --  And then enter the word into DICT.LOC, so it will hit next time

      --  All unknowns could be recorded in a file for later reference

      --  A '?' gives information (help) about the item in question

      --  One can change the symbol that the main program uses for change
      --  and file

      --  One can save the new parameters or let them revert to previous
      --  There should be a basic set of parameters that one can always go to

      --  There should be moods of translation, maybe to switch dictionaries

      --  Maybe to turn on or off pre/suffix
      --  Maybe to allow the user to look at just all the prefixes that match

      --    INQUIRE (HAVE_DEBUG_FILE, HAVE_DEBUG_FILE_HELP);
      --    if IS_OPEN (DBG)  and then not WORDS_MDEV (HAVE_DEBUG_FILE)  then
      --      DELETE (DBG);
      --      WORDS_MDEV (WRITE_DEBUG_FILE) := FALSE;
      --    end if;
      --    if not IS_OPEN (DBG) and then WORDS_MDEV (HAVE_DEBUG_FILE)  then
      --      begin
      --        CREATE (DBG, OUT_FILE, DEBUG_FULL_NAME);
      --      exception
      --        when others =>
      --          PUT_LINE ("Cannot CREATE WORD.DBG " &
      --                    "-- Check if it is in use elsewhere");
      --      end;
      --    end if;
      --
      --    if WORDS_MDEV (HAVE_DEBUG_FILE)  then
      --      INQUIRE (WRITE_DEBUG_FILE, WRITE_DEBUG_FILE_HELP);
      --    end if;

      Inquire (Have_Statistics_File, Have_Statistics_File_Help);
      if Is_Open (Stats)  and then not Words_Mdev (Have_Statistics_File)  then
         Delete (Stats);
         Words_Mdev (Write_Statistics_File) := False;
      end if;
      if not Is_Open (Stats) and then Words_Mdev (Have_Statistics_File)  then
         begin
            Create (Stats, Out_File, Stats_Full_Name);
         exception
            when others =>
               Put_Line
                 ("Cannot CREATE WORD.STA - Check if it is in use elsewhere");
         end;
      end if;

      if Words_Mdev (Have_Statistics_File)  then
         Inquire (Write_Statistics_File, Write_Statistics_File_Help);
      end if;

      Inquire (Do_Only_Initial_Word, Do_Only_Initial_Word_Help);
      if Words_Mdev (Do_Only_Initial_Word)  then
         Inquire (For_Word_List_Check, For_Word_List_Check_Help);
      else
         Words_Mdev (For_Word_List_Check) := False;
      end if;

      Inquire (Show_Dictionary, Show_Dictionary_Help);

      Inquire (Show_Dictionary_Line, Show_Dictionary_Line_Help);

      Inquire (Show_Dictionary_Codes, Show_Dictionary_Codes_Help);

      Inquire (Do_Pearse_Codes, Do_Pearse_Codes_Help);

      if Words_Mode (Do_Fixes) then
         Inquire (Do_Only_Fixes, Do_Only_Fixes_Help);
         Inquire (Do_Fixes_Anyway, Do_Fixes_Anyway_Help);
      end if;

      Inquire (Use_Prefixes, Use_Prefixes_Help);

      Inquire (Use_Suffixes, Use_Suffixes_Help);

      Inquire (Use_Tackons, Use_Tackons_Help);

      if Words_Mode (Do_Tricks) then
         Inquire (Do_Medieval_Tricks, Do_Medieval_Tricks_Help);
      end if;

      Inquire (Do_Syncope, Do_Syncope_Help);

      Inquire (Do_Two_Words, Do_Two_Words_Help);

      Inquire (Include_Unknown_Context, Include_Unknown_Context_Help);

      Inquire (No_Meanings, No_Meanings_Help);

      Inquire (Omit_Archaic, Omit_Archaic_Help);

      Inquire (Omit_Medieval, Omit_Medieval_Help);

      Inquire (Omit_Uncommon, Omit_Uncommon_Help);

      Inquire (Do_I_For_J, Do_I_For_J_Help);

      Inquire (Do_U_For_V, Do_U_For_V_Help);

      Inquire (Pause_In_Screen_Output, Pause_In_Screen_Output_Help);

      Inquire (No_Screen_Activity, No_Screen_Activity_Help);

      Inquire (Update_Local_Dictionary, Update_Local_Dictionary_Help);

      Inquire (Update_Meanings, Update_Meanings_Help);

      Inquire (Minimize_Output, Minimize_Output_Help);

      Put ("START_FILE_CHARACTER ?  "); Set_Col (45); Put ("(Currently  '");
      Put (Start_File_Character); Put ("'");
      Put (" =>");
      Get_Line (L1, Ll);
      if Ll /= 0  then
         if Is_Special_Graphical (L1 (1)) and
           (L1 (1) /= Change_Parameters_Character)  and
           (L1 (1) /= Change_Developer_Modes_Character)
         then
            Start_File_Character := L1 (1);
         else
            Put_Line ("Not an acceptable Character, " &
              "may conflict with other Input");
            Put_Line ("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      end if;
      New_Line;

      Put ("CHANGE_PARAMETERS_CHARACTER ?  ");
      Set_Col (45); Put ("(Currently  '");
      Put (Change_Parameters_Character); Put ("'");
      Put (" =>");
      Get_Line (L1, Ll);
      if Ll /= 0  then
         if Is_Special_Graphical (L1 (1)) and
           (L1 (1) /= Start_File_Character)  and
           (L1 (1) /= Change_Developer_Modes_Character)
         then
            Change_Parameters_Character := L1 (1);
         else
            Put_Line ("Not an acceptable Character," &
              " may conflict with other Input");
            Put_Line ("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      end if;
      New_Line;

      Put ("CHANGE_DEVELOPER_MODES_CHARACTER ?  ");
      Set_Col (45); Put ("(Currently  '");
      Put (Change_Developer_Modes_Character); Put ("'");
      Put (" =>");
      Get_Line (L1, Ll);
      if Ll /= 0  then
         if Is_Special_Graphical (L1 (1)) and
           (L1 (1) /= Start_File_Character)  and
           (L1 (1) /= Change_Language_Character)  and
           (L1 (1) /= Change_Parameters_Character)
         then
            Change_Developer_Modes_Character := L1 (1);
         else
            Put_Line ("Not an acceptable Character, " &
              "may conflict with other Input");
            Put_Line ("NO CHANGE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         end if;
      end if;
      New_Line;

      Put ("Do you wish to save this set of parameters? Y or N (Default) ");
      Put (" =>");
      Get_Line (L1, Ll);
      if Ll /= 0  then
         if L1 (1) = '?'  then
            Put (Save_Parameters_Help);
            Put
              ("Do you wish to save this set of parameters? Y or N (Default) ");
            Put (" =>");
            Get_Line (L1, Ll);
         end if;
         Reply_Type_Io.Get (L1 (1 .. Ll), R, Ll);
         if Mdev_Of_Reply (R)  then
            Put_Mdevs;
            Put_Line ("MDEV_ARRAY saved in file " & Mdev_Full_Name);
         end if;
      end if;
      New_Line;

   exception
      when Blank_Input  =>
         null;
      when others =>
         Put_Line ("Bad Input - terminating CHANGE_DEVELOPER_PARAMETERS");

   end Change_Developer_Modes;

   procedure Initialize_Developer_Parameters is
   begin

      Do_Mdev_File :
      begin
         --  Read the MDEV file
         Get_Mdevs;
         Preface.Put_Line
           ("MDEV_FILE found - Using those MDEVs and parameters");
      exception
         --  If there is any problem
         --  Put that the MDEV file is corrupted and the options are:
         --  to proceed with default parameters
         --  to set parameters with a CHANGE (SET) PARAMETERS and save
         --  to examine the MDEV file with a text editor and try to repair it
         when Name_Error  =>
            Words_Mdev := Default_Mdev_Array;
         when Bad_Mdev_File  =>
            Preface.Put_Line
              ("MDEV_FILE exists, but empty or corupted - Default MDEVs used");
            Preface.Put_Line
              ("You can set new parameters with CHANGE PARAMETERS and save.");
            Words_Mdev := Default_Mdev_Array;
      end Do_Mdev_File;

      --  if not IS_OPEN (DBG) and then WORDS_MDEV (HAVE_DEBUG_FILE)  then
      --    CREATE (DBG, OUT_FILE, DEBUG_FULL_NAME);
      --    PREFACE.PUT_LINE ("WORD.DBG Created at Initialization");
      --  end if;
      if not Is_Open (Stats) and then Words_Mdev (Have_Statistics_File)  then
         Create (Stats, Out_File, Stats_Full_Name);
         Preface.Put_Line ("WORD.STA Created at Initialization");
      end if;

   end Initialize_Developer_Parameters;

end Support_Utils.Developer_Parameters;
