package config is

   output_screen_size : integer := 20;

   type configuration_type is (developer_version, user_version, only_meanings); 
   
   configuration : configuration_type := developer_version;

   type method_type is (interactive, command_line_input, command_line_files);

   method : method_type := interactive;

   type language_type is (latin_to_english, english_to_latin);

   language : language_type := latin_to_english;

   suppress_preface : boolean := false;   
   
   
end config;
