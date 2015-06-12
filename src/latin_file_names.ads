package LATIN_FILE_NAMES is
  --  In order to port the program LATIN to another system, the file names
  --  must be made consistent with that system.
  --  This package is withed into all units that declare external file names
  --  and its modification should take care of the system dependence of names
  --  Then one needs to copy the ASCII data files on the disk to files named
  --  in accordance with the modified package.
  --  Note that there are some files that take extensions in DOS, and there 
  --  is a function that takes those extensions and makes a legal file name.
  --  In other systems this will have to be handled to create a legal file name

  --  This package can be presented as the first to be compiled, however 
  --  the actual need for file mames does not come until deep in the system
  --  Conventionally, the naming is put off until the file is actually
  --  used, and the name is passed as a parameter from there to the 
  --  earlier procedures which call them

  --  The following files are used in the DOS LATIN program and are 
  --  DOS legal, names no longer than 8 characters, with '.' and extension


  --  Single files, that is, that need only the one FULL name, no variations
  --  These files are input files and may have any name legal in your system
  --  and contain the ASCII information copied from the porting system

  INFLECTIONS_FULL_NAME     : constant STRING := "INFLECTS.LAT";
  INFLECTIONS_SECTIONS_NAME : constant STRING := "INFLECTS.SEC";

  UNIQUES_FULL_NAME      : constant STRING := "UNIQUES.LAT";
  ADDONS_FULL_NAME       : constant STRING := "ADDONS.LAT";

  --  These files may be created and used by the program
  MODE_FULL_NAME         : constant STRING := "WORD.MOD";
  OUTPUT_FULL_NAME       : constant STRING := "WORD.OUT";
  UNKNOWNS_FULL_NAME     : constant STRING := "WORD.UNK";
  PARSE_FULL_NAME        : constant STRING := "WORD.PRS";

  --  These file names are used with extensions (e.g., GEN, SPE, LOC) 
  --  for the various dictionaries
  --  The function ADD_FILE_NAME_EXTENSION below is used to create 
  --  a full file name
  --  Note that for DOS they are not complete names (no '.')
  --  but DOS is forgiving and will give it a pass

  DICTIONARY_FILE_NAME  : constant STRING := "DICT";
  DICT_FILE_NAME        : constant STRING := "DICTFILE";
  DICT_LINE_NAME        : constant STRING := "DICTLINE";
  STEM_LIST_NAME        : constant STRING := "STEMLIST";
  STEM_FILE_NAME        : constant STRING := "STEMFILE";
  INDX_FILE_NAME        : constant STRING := "INDXFILE";


  function ADD_FILE_NAME_EXTENSION(NAME, EXTENSION : STRING) return STRING;
  --  This is the function that creates a file name legal for your system
  --  with a FILE_NAME defined above and a program specified extension

end LATIN_FILE_NAMES;   
