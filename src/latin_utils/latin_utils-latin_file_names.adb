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

package body Latin_Utils.Latin_File_Names is

   function add_file_name_extension (name, extension : String) return String is
      --  This is the version that Creates a DOS file name
      --  One that has a name, a '.', and an extension no longer than 3 chars
      --  Arbitarily, we also truncate the NAME to 8 Characters
      --  To port to another system, one needs to do this function appropriately
      name_length : Integer := name'Length;
      extension_length : Integer := extension'Length;
   begin
      if name_length >= 8  then
         name_length := 8;
      end if;
      if extension'Length >= 3  then
         extension_length := 3;
      end if;
      return name (name'First .. name_length) & '.'
        & extension (extension'First .. extension_length);
   end add_file_name_extension;

end Latin_Utils.Latin_File_Names;
