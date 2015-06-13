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
	  return name(1..name_length) & '.' & extension(1..extension_length);
   end add_file_name_extension;

end latin_file_names;
