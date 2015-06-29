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

with text_io;
with strings_package; use strings_package;
with latin_file_names; use latin_file_names;
with word_parameters; use word_parameters;
with developer_parameters; use developer_parameters;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with word_support_package; use word_support_package;
with preface;
with word_package; use word_package;
with config; use config;
with english_support_package; use english_support_package;
with banner; use banner;

with parse; use parse;

pragma elaborate(word_parameters);

procedure process_input(configuration : configuration_type;
                        command_line : string := "")
is
   -- use inflections_package.integer_io;
   -- use inflection_record_io;
   use text_io;

   procedure delete_if_open(filename : string; dict_name : dictionary_kind) is
   begin
      begin
         if dict_io.is_open(dict_file(dict_name)) then
            dict_io.delete(dict_file(dict_name));
         else
            dict_io.open(dict_file(dict_name), dict_io.in_file,
              add_file_name_extension(dict_file_name, filename));
            dict_io.delete(dict_file(dict_name));
         end if;
      exception when others => null;
      end;   --  not there, so don't have to DELETE
   end delete_if_open;

   -- get and handle a line of input
   -- return value says whether there is more input, i.e. false -> quit
   function get_input_line return boolean
   is
      blank_line : constant string(1..2500) := (others => ' ');
      line : string(1 .. 2500) := (others => ' ');
      l : integer := 0;
   begin
      --  Block to manipulate file of lines
      if (name(current_input) = name(standard_input))  then
         scroll_line_number := integer(text_io.line(text_io.standard_output));
         preface.new_line;
         preface.put("=>");
      end if;

      line := blank_line;
      get_line(line, l);
      if (l = 0) or else (trim(line(1..l)) = "")  then
         --  Count blank lines
         --LINE_NUMBER := LINE_NUMBER + 1;
         if (name(current_input) = name(standard_input))  then
            --  INPUT is keyboard
            preface.put("Blank exits =>");
            get_line(line, l);
            -- Second try
            if (l = 0) or else (trim(line(1..l)) = "")  then
               -- Two in a row
               return false;
            end if;
         else
            --  INPUT is file

            --LINE_NUMBER := LINE_NUMBER + 1;
            --  Count blank lines in file
            if end_of_file(current_input) then
               set_input(standard_input);
               close(input);
            end if;
         end if;
      end if;

      if (trim(line(1..l)) /= "")  then
         -- Not a blank line so L(1) (in file input)
         if line(1) = start_file_character  then
            if (name(current_input) /= name(standard_input)) then
               text_io.put_line("Cannot have file of words (@FILE) " &
                 "in an @FILE");
            else
               text_io.open(input, text_io.in_file, trim(line(2..l)));
               text_io.set_input(input);
            end if;
         elsif line(1) = change_parameters_character  and then
           (name(current_input) = name(standard_input)) and then
           not config.suppress_preface  then
            change_parameters;
         elsif line(1) = change_language_character  then
            change_language(line(2));
         elsif
           line(1) = change_developer_modes_character  and then
           (name(current_input) = name(standard_input)) and then
           not config.suppress_preface  then
            change_developer_modes;
         else
            if (name(current_input) /= name(standard_input))  then
               preface.new_line;
               preface.put_line(line(1..l));
            end if;
            if words_mode(write_output_to_file)     then
               if not config.suppress_preface     then
                  new_line(output);
                  text_io.put_line(output, line(1..l));
               end if;
            end if;
            --  Count lines to be parsed
            line_number := line_number + 1;

            parse.parse_line(configuration, line(1..l));
         end if;
      end if;

      return true;

   exception
      when name_error | use_error =>
         if (name(current_input) /= name(standard_input))  then
            set_input(standard_input);
            close(input);
         end if;
         put_line("An unknown or unacceptable file name. Try Again");
         return true;
      when end_error =>
         --  The end of the input file resets to CON:
         if (name(current_input) /= name(standard_input))  then
            set_input(standard_input);
            close(input);
            if method = command_line_files  then raise give_up; end if;
            return true;
         else
            put_line("Raised END_ERROR, although in STANDARD_INPUT");
            put_line("^Z is inappropriate keyboard input, " &
              "WORDS should be terminated with a blank line");
            raise give_up;
         end if;
      when status_error =>
         --  The end of the input file resets to CON:
         put_line("Raised STATUS_ERROR");
         return false;
   end;

begin
      --  PARSE
   if method = command_line_input  then
      if trim(command_line) /= ""  then
         parse.parse_line(configuration, command_line);
      end if;

   else
      banner.print_main_banner(start_file_character,
        change_parameters_character, help_character);

      if english_dictionary_available(general)  then
         preface.put_line("English-to-Latin available");
         preface.put_line(
           change_language_character & "E changes to English-to-Latin, " &
           change_language_character & "L changes back     [tilde E]");
      end if;

      if configuration = only_meanings  then
         banner.print_mode_warning;
      end if;

      while get_input_line loop
         null;
      end loop;

   end if;     --  On command line input

   begin
      stem_io.open(stem_file(local), stem_io.in_file,
                   add_file_name_extension(stem_file_name,
                                           "LOCAL"));
      --  Failure to OPEN will raise an exception, to be handled below
      if stem_io.is_open(stem_file(local)) then
         stem_io.delete(stem_file(local));
      end if;
   exception
      when others =>
         null;      --  If cannot OPEN then it does not exist, so is deleted
   end;
   --  The rest of this seems like overkill, it might have been done elsewhere

   delete_if_open("LOCAL", local);
   delete_if_open("ADDONS", addons);
   delete_if_open("UNIQUE", unique);

exception
   when storage_error  =>    --  Have tried at least twice, fail
      preface.put_line("Continuing STORAGE_ERROR Exception in PARSE");
      preface.put_line("If insufficient memory in DOS, try removing TSRs");
   when give_up  =>
      preface.put_line("Giving up!");
   when others  =>
      preface.put_line("Unexpected exception raised in PARSE");
end process_input;
