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

with Ada.Text_IO;
package Support_Utils.Developer_Parameters is

   --  This package defines a number of parameters that areused in the program
   --  The default values are set in the body, so that they may be changed
   --  easily

   --  These files are used by the program if requested, but not necessary
   --  They are all text files and human readable

   --  DEVELOPER MODE_FILE is used by the program to remember values
   Mdev_File : Ada.Text_IO.File_Type;
   Mdev_Full_Name : constant String := "WORD.MDV";

   --  Debug not currently in use
   --  --  DBG collects debug Output for one entry at a time
   --  DBG : TEXT_IO.FILE_TYPE;
   --  DEBUG_FULL_NAME : constant STRING := "WORD.DBG";

   --  STATS collects statistics on the program, stems used, inflections, etc.
   Stats : Ada.Text_IO.File_Type;
   Stats_Full_Name : constant String := "WORD.STA";

   type Mdev_Type is (
     --               HAVE_DEBUG_FILE,      --  No longer in use
     --               WRITE_DEBUG_FILE,

     Have_Statistics_File,
     Write_Statistics_File,

     Show_Dictionary,
     Show_Dictionary_Line,
     Show_Dictionary_Codes,
     Do_Pearse_Codes,

     Do_Only_Initial_Word,
     For_Word_List_Check,

     Do_Only_Fixes,
     Do_Fixes_Anyway,
     Use_Prefixes,
     Use_Suffixes,
     Use_Tackons,

     Do_Medieval_Tricks,

     Do_Syncope,
     Do_Two_Words,
     Include_Unknown_Context,
     No_Meanings,

     Omit_Archaic,
     Omit_Medieval,
     Omit_Uncommon,

     Do_I_For_J,
     Do_U_For_V,

     Pause_In_Screen_Output,
     No_Screen_Activity,

     Update_Local_Dictionary,
     Update_Meanings,

     Minimize_Output);

   package Mdev_Type_Io is new Ada.Text_IO.Enumeration_IO (Mdev_Type);

   type Mdev_Array is array (Mdev_Type) of Boolean;

   Words_Mdev : Mdev_Array;        --  Initialized in body

   Start_File_Character               : Character := '@';
   Change_Developer_Modes_Character   : Character := '!';

   procedure Change_Developer_Modes;

   procedure Update_Local_Dictionary_File;

   procedure Initialize_Developer_Parameters;

end Support_Utils.Developer_Parameters;
