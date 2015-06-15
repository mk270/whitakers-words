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

package body latin_file_names is

   function add_file_name_extension(name, extension : string) return string is
      --  This is the version that creates a DOS file name
      --  One that has a name, a '.', and an extension no longer than 3 characters
      --  Arbitarily, we also truncate the NAME to 8 characters
      --  To port to another system, one needs to do this function appropriately
      name_length : integer := name'length;
      extension_length : integer := extension'length;
   begin
      if name_length >= 8  then
         name_length := 8;
      end if;
      if extension'length >= 3  then
         extension_length := 3;
      end if;
      return name(name'first .. name_length) & '.' & extension(extension'first .. extension_length);
   end add_file_name_extension;

end latin_file_names;
