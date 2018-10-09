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

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Config; use Latin_Utils.Config;
with Latin_Utils.Preface;
use Latin_Utils;
pragma Elaborate (Latin_Utils.Preface);
package body Support_Utils.Word_Parameters is
   use Ada.Text_IO;

   type Help_Type is array (Natural range <>) of String (1 .. 70);
   Blank_Help_Line : constant String (1 .. 70) := (others => ' ');
   No_Help : constant Help_Type := (2 .. 1 => Blank_Help_Line);

   type Reply_Type is (N, Y);
   package Reply_Type_Io is new Ada.Text_IO.Enumeration_IO (Reply_Type);
   Reply : constant array (Boolean) of Reply_Type := (N, Y);
   Mode_Of_Reply : constant array (Reply_Type) of Boolean := (False, True);

   Blank_Input : exception;

   --  The default modes are set in the body so that they can be changed
   --  with only this being recompiled, not the rest of the with'ing system
   Default_Mode_Array : constant Mode_Array := (
     Trim_Output                 => True,

     Have_Output_File            => False,
     Write_Output_To_File        => False,

     Do_Unknowns_Only            => False,
     Write_Unknowns_To_File      => False,

     Ignore_Unknown_Names        => True,
     Ignore_Unknown_Caps         => True,
     Do_Compounds                => True,
     Do_Fixes                    => True,
     Do_Tricks                   => True,

     Do_Dictionary_Forms         => True,
     Show_Age                    => False,
     Show_Frequency              => False,

     Do_Examples                 => False,
     Do_Only_Meanings            => False,
     Do_Stems_For_Unknown        => False);

   Bad_Mode_File : exception;

   -- FIXME: this help text seems to duplicate what's in developer_parameters
   Trim_Output_Help : constant Help_Type :=  (
     "This option instructs the program to remove from the Output list of   ",
     "possible constructs those which are least likely.  There is now a fair",
     "amount of trimming, killing LOC and VOC plus removing Uncommon and    ",
     "non-classical (Archaic/Medieval) when more common results are found   ",
     "and this action is requested (turn it off in MDV (!) parameters).     ",
     "When a TRIM has been done, Output is usually followed by asterix (*). ",
     "The asterix may be missing depending on where the TRIM is done.       ",
     "There certainly is no absolute assurence that the items removed are   ",
     "not correct, just that they are statistically less likely.            ",
     "Note that poets are likely to employ unusual words and inflections for",
     "various reasons.  These may be trimmed out if this parameter in on.   ",
     "When in English mode, trim just reduces the Output to the top six     ",
     "results, if there are that many.  Asterix means there are more.       ",
     "                                                The default is Y(es)  ");

   Have_Output_File_Help : constant Help_Type :=  (
     "This option instructs the program to Create a file which can hold the ",
     "Output for later study, otherwise the results are just displayed on   ",
     "the screen.  The Output file is named " & Output_Full_Name
     & (39 + Output_Full_Name'Length .. 70 => ' '),
     "This means that one run will necessarily overWrite a previous run,    ",
     "unless the previous results are renamed or copied to a file of another",
     "name.  This is available if the METHOD is INTERACTIVE, no parameters. ",
     "The default is N(o), since this prevents the program from overwriting ",
     "previous work unintentionally.  Y(es) Creates the Output file.        ");

   Write_Output_To_File_Help : constant Help_Type :=  (
     "This option instructs the program, when HAVE_OUTPUT_FILE is on, to    ",
     "Write results to the file " & Output_Full_Name
     & (27 + Output_Full_Name'Length .. 70 => ' '),
     "This option may be turned on and off during running of the program,   ",
     "thereby capturing only certain desired results.  If the option        ",
     "HAVE_OUTPUT_FILE is off, the user will not be given a chance to turn  ",
     "this one on.  Only for INTERACTIVE running.         Default is N(o).  ",
     "This works in English mode, but Output in somewhat different so far.  ");

   Do_Unknowns_Only_Help : constant Help_Type :=  (
     "This option instructs the program to only Output those words that it  ",
     "cannot resolve.  Of course, it has to do processing on all words, but ",
     "those that are found (with prefix/suffix, if that option in on) will  ",
     "be ignored.  The purpose of this option is to allow a quick look to   ",
     "determine if the dictionary and process is going to do an acceptable  ",
     "job on the current text.  It also allows the user to assemble a list  ",
     "of unknown words to look up manually, and perhaps augment the system  ",
     "dictionary.  For those purposes, the system is usually run with the   ",
     "MINIMIZE_OUTPUT option, just producing a list.  Another use is to run ",
     "without MINIMIZE to an Output file.  This gives a list of the Input   ",
     "text with the unknown words, by line.  This functions as a spelling   ",
     "checker for Latin texts.  The default is N(o).                        ",
     "This does not work in English mode, but may in the future.            ");

   Write_Unknowns_To_File_Help : constant Help_Type :=  (
     "This option instructs the program to Write all unresolved words to a  ",
     "UNKNOWNS file named " & Unknowns_Full_Name
     & (21 + Unknowns_Full_Name'Length .. 70 => ' '),
     "With this option on, the file of unknowns is written, even though     ",
     "the main Output contains both known and unknown (unresolved) words.   ",
     "One may wish to save the unknowns for later analysis, testing, or to  ",
     "form the basis for dictionary additions.  When this option is turned  ",
     "on, the UNKNOWNS file is written, destroying any file from a previous ",
     "run.  However, the Write may be turned on and off during a single run ",
     "without destroying the information written in that run.               ",
     "This option is for specialized use, so its default is N(o).           ",
     "This does not work in English mode, but may in the future.            ");

   Ignore_Unknown_Names_Help : constant Help_Type :=  (
     "This option instructs the program to assume that any capitalized word ",
     "longer than three letters is a proper name.  As no dictionary can be  ",
     "expected to account for many proper names, many such occur that would ",
     "be called UNKNOWN.  This contaminates the Output in most cases, and   ",
     "it is often convenient to ignore these spurious UNKNOWN hits.  This   ",
     "option implements that mode, and calls such words proper names.       ",
     "Any proper names that are in the dictionary are handled in the normal ",
     "manner.                                The default is Y(es).          ");

   Ignore_Unknown_Caps_Help : constant Help_Type :=  (
     "This option instructs the program to assume that any all caps word    ",
     "is a proper name or similar designation.  This convention is often    ",
     "used to designate speakers in a discussion or play.  No dictionary can",
     "claim to be exhaustive on proper names, so many such occur that would ",
     "be called UNKNOWN.  This contaminates the Output in most cases, and   ",
     "it is often convenient to ignore these spurious UNKNOWN hits.  This   ",
     "option implements that mode, and calls such words names.  Any similar ",
     "designations that are in the dictionary are handled in the normal     ",
     "manner, as are normal words in all caps.    The default is Y(es).     ");

   Do_Compounds_Help : constant Help_Type :=  (
     "This option instructs the program to look ahead for the verb TO_BE (or",
     "iri) when it finds a verb participle, with the expectation of finding ",
     "a compound perfect tense or periphrastic.  This option can also be a  ",
     "trimming of the output, in that VPAR that do not fit (not NOM) will be",
     "excluded, possible interpretations are lost.  Default choice is Y(es).",
     "This processing is turned off with the choice of N(o).                ");

   Do_Fixes_Help : constant Help_Type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, to attach various prefixes and suffixes and  ",
     "try again.  This effort is successful in about a quarter of the cases ",
     "which would otherwise give UNKNOWN results, or so it seems in limited ",
     "tests.  For those cases in which a result is produced, about half give",
     "easily interpreted Output; many of the rest are etymologically True,  ",
     "but not necessarily obvious; about a tenth give entirely spurious     ",
     "derivations.  The user must proceed with caution.                     ",
     "The default choice is Y(es), since the results are generally useful.  ",
     "This processing can be turned off with the choice of N(o).            ");

   Do_Tricks_Help : constant Help_Type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, and after various prefixes and suffixes, to  ",
     "try every dirty Latin trick it can think of, mainly common letter     ",
     "replacements like cl -> cul, vul -> vol, ads -> ass, inp -> imp, etc. ",
     "Together these tricks are useful, but may give False Positives (>10%).",
     "They provide for recognized variants in classical spelling.  Most of  ",
     "the texts with which this program will be used have been well edited  ",
     "and standardized in spelling.  Now, moreover,  the dictionary is being",
     "populated to such a state that the hit rate on tricks has fallen to a ",
     "low level.  It is very seldom productive, and it is always expensive. ",
     "The only excuse for keeping it as default is that now the dictionary  ",
     "is quite extensive and misses are rare.         Default is now Y(es). ");

   Do_Dictionary_Forms_Help : constant Help_Type :=  (
     "This option instructs the program to Output a line with the forms     ",
     "normally associated with a dictionary entry (NOM and GEN of a noun,   ",
     "the four principal parts of a verb, M-F-N NOM of an adjective, ...).  ",
     "This occurs when there is other Output (i.e., not with UNKNOWNS_ONLY).",
     "The default choice is N(o), but it can be turned on with a Y(es).     ");

   Show_Age_Help : constant Help_Type :=  (
     "This option causes a flag, like '<Late>' to appear for inflection or  ",
     "form in the Output.  The AGE indicates when this word/inflection was  ",
     "in use, at least from indications is dictionary citations.  It is     ",
     "just an indication, not controlling, useful when there are choices.   ",
     "No indication means that it is common throughout all periods.         ",
     "The default choice is Y(es), but it can be turned off with a N(o).    ");

   Show_Frequency_Help : constant Help_Type :=  (
     "This option causes a flag, like '<rare>' to appear for inflection or  ",
     "form in the Output.  The FREQ is indicates the relative usage of the  ",
     "word or inflection, from indications is dictionary citations.  It is  ",
     "just an indication, not controlling, useful when there are choices.   ",
     "No indication means that it is common throughout all periods.         ",
     "The default choice is Y(es), but it can be turned off with a N(o).    ");

   Do_Examples_Help : constant Help_Type :=  (
     "This option instructs the program to provide examples of usage of the ",
     "cases/tenses/etc. that were constructed.  The default choice is N(o). ",
     "This produces lengthy Output and is turned on with the choice Y(es).  ");

   Do_Only_Meanings_Help : constant Help_Type :=  (
     "This option instructs the program to only Output the MEANING for a    ",
     "word, and omit the inflection details.  This is primarily used in     ",
     "analyzing new dictionary material, comparing with the existing.       ",
     "However it may be of use for the translator who knows most all of     ",
     "the words and just needs a little reminder for a few.                 ",
     "The default choice is N(o), but it can be turned on with a Y(es).     ");

   Do_Stems_For_Unknown_Help : constant Help_Type :=  (
     "This option instructs the program, when it is unable to find a proper ",
     "match in the dictionary, and after various prefixes and suffixes, to  ",
     "list the dictionary entries around the unknown.  This will likely     ",
     "catch a substantive for which only the ADJ stem appears in dictionary,",
     "an ADJ for which there is only a N stem, etc.  This option should     ",
     "probably only be used with individual UNKNOWN words, and off-line     ",
     "from full translations, therefore the default choice is N(o).         ",
     "This processing can be turned on with the choice of Y(es).            ");

   Save_Parameters_Help : constant Help_Type :=  (
     "This option instructs the program, to save the current parameters, as ",
     "just established by the user, in a file WORD.MOD.  If such a file     ",
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

   procedure Put_Modes is
      use Mode_Type_Io;
      use Reply_Type_Io;
   begin
      if Is_Open (Mode_File)  then
         Close (Mode_File);
      end if;
      Create (Mode_File, Out_File, Mode_Full_Name);
      for I in Words_Mode'Range  loop
         Put (Mode_File, I);
         Set_Col (Mode_File, 35);
         Put (Mode_File, Reply (Words_Mode (I)));
         New_Line (Mode_File);
      end loop;
      Close (Mode_File);
   end Put_Modes;

   procedure Get_Modes is --(M : out MODE_ARRAY) is
      use Mode_Type_Io;
      use Reply_Type_Io;
      Mo : Mode_Type;
      Rep : Reply_Type;
   begin
      Open (Mode_File, In_File, Mode_Full_Name);
      while not End_Of_File (Mode_File)  loop
         Get (Mode_File, Mo);
         Get (Mode_File, Rep);
         Words_Mode (Mo) := Mode_Of_Reply (Rep);
      end loop;
      Close (Mode_File);

   exception
      when Name_Error  =>
         raise;
      when others =>
         raise Bad_Mode_File;
   end Get_Modes;

   procedure Inquire (Mo : Mode_Type; Help : in Help_Type := No_Help) is
      use Mode_Type_Io;
      use Reply_Type_Io;
      L1 : String (1 .. 100) := (others => ' ');
      Ll : Natural;
      R  : Reply_Type;
   begin
      Put (Mo);
      Put (" ?  "); Set_Col (45); Put ("(Currently  ");
      Put (Reply (Words_Mode (Mo))); Put (" =>");
      Get_Line (L1, Ll);
      if Ll /= 0  then
         if Trim (L1 (1 .. Ll)) = ""  then
            Put_Line ("Blank Input, skipping the rest of CHANGE_PARAMETERS");
            raise Blank_Input;
         elsif L1 (1) = '?'  then
            Put (Help);
            Inquire (Mo, Help);
         else
            Get (L1 (1 .. Ll), R, Ll);
            Words_Mode (Mo) := Mode_Of_Reply (R);
         end if;
      end if;
      New_Line;
   end Inquire;

   procedure Change_Parameters is
      L1 : String (1 .. 100) := (others => ' ');
      Ll : Natural;
      R  : Reply_Type;

   begin

      Put_Line ("To set/change parameters reply Y/y or N/n" &
        ".  Return accepts current value.");
      Put_Line ("A '?' reply gives information/help on that parameter." &
        "  A space skips the rest.");
      New_Line;

      --  Interactive mode - lets you do things on unknown words

      --  You can say it is a noun and then look at the endings
      --  Or look all the endings and guess what part of speech

      --  You can look at the dictionary items that are Close to the word
      --  There may be cases in which the stem is found but is not of right part
      --  So maybe the word list is deficient and that root goes also to a ADJ
      --  even if it is listed only for a N.
      --  One can also look for ADV here with ending 'e', etc.

      --  You can look up the word in a paper dictionary (with the help
      --  of ending)
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

      Inquire (Trim_Output, Trim_Output_Help);

      Inquire (Have_Output_File, Have_Output_File_Help);

      if Is_Open (Output)  and then not Words_Mode (Have_Output_File)  then
         Close (Output);
         Words_Mode (Write_Output_To_File) := False;
      end if;
      if not Is_Open (Output) and then Words_Mode (Have_Output_File)  then
         begin
            Create (Output, Out_File, Output_Full_Name);
         exception
            when others =>
               Put_Line
                 ("Cannot CREATE WORD.OUT - Check if it is in use elsewhere");
         end;
      end if;

      if Words_Mode (Have_Output_File)  then
         Inquire (Write_Output_To_File, Write_Output_To_File_Help);
      end if;

      Inquire (Do_Unknowns_Only, Do_Unknowns_Only_Help);

      Inquire (Write_Unknowns_To_File, Write_Unknowns_To_File_Help);
      --  If there is an Open file then OK
      --  If not Open and you now want to start writing to UNKNOWNS, the CREATE
      if not Is_Open (Unknowns) and then
        Words_Mode (Write_Unknowns_To_File)
      then
         begin
            Create (Unknowns, Out_File, Unknowns_Full_Name);
         exception
            when others =>
               Put_Line
                 ("Cannot CREATE WORD.UNK - Check if it is in use elsewhere");
         end;
      end if;

      Inquire (Ignore_Unknown_Names, Ignore_Unknown_Names_Help);

      Inquire (Ignore_Unknown_Caps, Ignore_Unknown_Caps_Help);

      Inquire (Do_Compounds, Do_Compounds_Help);

      Inquire (Do_Fixes, Do_Fixes_Help);

      Inquire (Do_Tricks, Do_Tricks_Help);

      Inquire (Do_Dictionary_Forms, Do_Dictionary_Forms_Help);

      Inquire (Show_Age, Show_Age_Help);

      Inquire (Show_Frequency, Show_Frequency_Help);

      Inquire (Do_Examples, Do_Examples_Help);

      Inquire (Do_Only_Meanings, Do_Only_Meanings_Help);

      Inquire (Do_Stems_For_Unknown, Do_Stems_For_Unknown_Help);

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
         if Mode_Of_Reply (R)  then
            Put_Modes;
            Put_Line ("MODE_ARRAY saved in file " & Mode_Full_Name);
         end if;
      end if;
      New_Line;

   exception
      when Blank_Input  =>
         null;
      when others =>
         Put_Line ("Bad Input - terminating CHANGE_PARAMETERS");

   end Change_Parameters;

   procedure Initialize_Word_Parameters is
   begin
      Words_Mode := Default_Mode_Array;
      --TEXT_IO.PUT_LINE ("Initializing WORD_PARAMETERS");

      Do_Mode_File :
      begin
         --  Read the mode file
         Get_Modes; --(WORDS_MODE);
         Preface.Put_Line
           ("MODE_FILE found - Using those modes and parameters");
      exception
         --  If there is any problem
         --  Put that the mode file is corrupted and the options are:
         --  to proceed with default parameters
         --  to set parameters with a CHANGE (SET) PARAMETERS and save
         --  to examine the mode file with a text editor and try to repair it
         when Name_Error  =>
            Words_Mode := Default_Mode_Array;
         when Bad_Mode_File  =>
            Put_Line
              ("MODE_FILE exists, but empty or corupted - Default modes used");
            Put_Line
              ("You can set new parameters with CHANGE PARAMETERS and save.");
            Words_Mode := Default_Mode_Array;
         when others  =>
            Put_Line ("MODE_FILE  others ERROR");
            Words_Mode := Default_Mode_Array;
      end Do_Mode_File;

      if ((Method = Interactive) or (Method = Command_Line_Input)) and then
        (not Ada.Text_IO.Is_Open (Output)) and then
        (Words_Mode (Have_Output_File))
      then
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Output_Full_Name);
         --TEXT_IO.PUT_LINE ("WORD.OUT Created at Initialization");
         Preface.Put_Line ("WORD.OUT Created at Initialization");
      end if;
      if not Ada.Text_IO.Is_Open (Unknowns)
        and then Words_Mode (Write_Unknowns_To_File)
      then
         Ada.Text_IO.Create (Unknowns, Ada.Text_IO.Out_File,
           Unknowns_Full_Name);
         Preface.Put_Line ("WORD.UNK Created at Initialization");
      end if;
   end Initialize_Word_Parameters;

end Support_Utils.Word_Parameters;
