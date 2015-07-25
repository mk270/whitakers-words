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

package Latin_Utils.Latin_File_Names is
   --  In order to port the program LATIN to another system, the file names
   --  must be made consistent with that system.
   --  This package is withed into all units that declare external file names
   --  and its modification should take care of the system dependence of names
   --  Then one needs to copy the ASCII data files on the disk to files named
   --  in accordance with the modified package.
   --  Note that there are some files that take extensions in DOS, and there
   --  is a function that takes those extensions and makes a legal file name.
   --  In other systems this will have to be handled to Create a legal file name

   --  This package can be presented as the first to be compiled, however
   --  the actual need for file mames does not come until deep in the system
   --  Conventionally, the naming is Put off until the file is actually
   --  used, and the name is passed as a parameter from there to the
   --  earlier procedures which call them

   --  The following files are used in the DOS LATIN program and are
   --  DOS legal, names no longer than 8 Characters, with '.' and extension

   --  Single files, that is, that need only the one FULL name, no variations
   --  These files are Input files and may have any name legal in your system
   --  and contain the ASCII information copied from the porting system

   Inflections_Full_Name     : constant String := "INFLECTS.LAT";
   Inflections_Sections_Name : constant String := "INFLECTS.SEC";

   Uniques_Full_Name      : constant String := "UNIQUES.LAT";
   Addons_Full_Name       : constant String := "ADDONS.LAT";

   --  These files may be Created and used by the program
   Mode_Full_Name         : constant String := "WORD.MOD";
   Output_Full_Name       : constant String := "WORD.OUT";
   Unknowns_Full_Name     : constant String := "WORD.UNK";
   Parse_Full_Name        : constant String := "WORD.PRS";

   --  These file names are used with extensions (e.g., GEN, SPE, LOC)
   --  for the various dictionaries
   --  The function ADD_FILE_NAME_EXTENSION below is used to Create
   --  a full file name
   --  Note that for DOS they are not complete names (no '.')
   --  but DOS is forgiving and will give it a pass

   Dictionary_File_Name  : constant String := "DICT";
   Dict_File_Name        : constant String := "DICTFILE";
   Dict_Line_Name        : constant String := "DICTLINE";
   Stem_List_Name        : constant String := "STEMLIST";
   Stem_File_Name        : constant String := "STEMFILE";
   Indx_File_Name        : constant String := "INDXFILE";

   function Add_File_Name_Extension (Name, Extension : String) return String;
   --  This is the function that Creates a file name legal for your system
   --  with a FILE_NAME defined above and a program specified extension

end Latin_Utils.Latin_File_Names;
