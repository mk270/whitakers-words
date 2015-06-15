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

with text_io; use text_io;
with strings_package; use strings_package;
with config;
with word_parameters; use word_parameters;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with developer_parameters; use developer_parameters;
with word_package; use word_package;
with english_support_package; use english_support_package;
with dictionary_form;

procedure search_english(input_english_word : string; pofs : part_of_speech_type := x) is
   use ewds_direct_io;
   input_word : eword := lower_case(head(input_english_word, eword_size));
   input_pofs : constant part_of_speech_type := pofs;

   output_array : ewds_array(1..500) := (others => null_ewds_record);
   number_of_hits : integer := 0;
   j1, j2, j : ewds_direct_io.count := 0;

   d_k : constant dictionary_kind := general;    --  For the moment

   ewds : ewds_record := null_ewds_record;

   first_try, second_try : boolean := true;

   procedure load_output_array(ewds : in ewds_record) is
   begin
      if ewds.pofs <= input_pofs  then
         number_of_hits := number_of_hits + 1;
         output_array(number_of_hits) := ewds;
      end if;
   end load_output_array;

   --procedure TRIM_OUTPUT_ARRAY is
   procedure sort_output_array is
      hits : integer := 0;
   begin

      --  Bubble sort
  hit_loop:
      loop
         hits := 0;

     switch:
         declare
            dw : ewds_record := null_ewds_record;

         begin
        inner_loop:    --  Order by RANK, FREQ, SEMI
            for i in 1..number_of_hits-1  loop
               if output_array(i+1).rank  >  output_array(i).rank     or else

                 (output_array(i+1).rank  =  output_array(i).rank     and then
                    output_array(i+1).freq  <  output_array(i).freq)  or else

                 (output_array(i+1).rank  =  output_array(i).rank     and then
                    output_array(i+1).freq  =  output_array(i).freq   and then
                    output_array(i+1).semi  <  output_array(i).semi)          then

                  dw := output_array(i);
                  output_array(i) := output_array(i+1);
                  output_array(i+1) := dw;
                  hits := hits + 1;
                  --PUT_LINE("HITS    " & INTEGER'IMAGE(HITS));
               end if;
            end loop inner_loop;

         end switch;
         exit when hits = 0;
      end loop hit_loop;

   end sort_output_array;

   procedure dump_output_array(output : in text_io.file_type) is
      de : dictionary_entry := null_dictionary_entry;
      number_to_show : integer := number_of_hits;
      one_screen : constant integer := 6;
   begin
      --TEXT_IO.PUT_LINE("DUMP_OUTPUT");
      if number_of_hits = 0  then
         text_io.put_line(output, "No Match");
      else

         sort_output_array;

         --TEXT_IO.PUT_LINE("DUMP_OUTPUT SORTED");

         trimmed := false;
         if words_mode(trim_output)  then
            if number_of_hits > one_screen  then
               number_to_show := one_screen;
               trimmed := true;
            else
               number_to_show := number_of_hits;
            end if;
         end if;

         for i in 1..number_to_show  loop
            text_io.new_line(output);

        do_pause:
            begin
               if (integer(text_io.line(output)) >
                     scroll_line_number + config.output_screen_size)  then
                  pause(output);
                  scroll_line_number := integer(text_io.line(output));
               end if;
            end do_pause;

            dict_io.read(dict_file(general), de, dict_io.count(output_array(i).n));
            put(output, dictionary_form(de));
            text_io.put(output, "   ");

            if de.part.pofs = n  then
               text_io.put(output, "  ");  decn_record_io.put(output, de.part.n.decl);
               text_io.put(output, "  " & gender_type'image(de.part.n.gender) & "  ");
            end if;
            if (de.part.pofs = v)   then
               text_io.put(output, "  ");  decn_record_io.put(output, de.part.v.con);
            end if;
            if (de.part.pofs = v)  and then  (de.part.v.kind in gen..perfdef)  then
               text_io.put(output, "  " & verb_kind_type'image(de.part.v.kind) & "  ");
            end if;

            if words_mdev(show_dictionary_codes)    then
               text_io.put(output, " [");
               age_type_io.put(output, de.tran.age);
               area_type_io.put(output, de.tran.area);
               geo_type_io.put(output, de.tran.geo);
               frequency_type_io.put(output, de.tran.freq);
               source_type_io.put(output, de.tran.source);
               text_io.put(output, "]  ");
            end if;

            if words_mdev(show_dictionary) then
               text_io.put(output, ext(d_k) & ">");
            end if;
            --TEXT_IO.PUT_LINE("DUMP_OUTPUT SHOW");

            if words_mdev(show_dictionary_line)  then
               text_io.put(output, "("
                             & trim(integer'image(output_array(i).n)) & ")");
            end if;

            text_io.new_line(output);

            --TEXT_IO.PUT_LINE("DUMP_OUTPUT MEAN");

            text_io.put(output, trim(de.mean));
            text_io.new_line(output);

         end loop;
         --TEXT_IO.PUT_LINE("DUMP_OUTPUT TRIMMED");

         if trimmed  then
            put_line(output, "*");
         end if;

      end if;    --  On HITS = 0

   exception
      when others =>
         null;   --  If N not in DICT_FILE
   end dump_output_array;

begin

   j1 := 1;
   j2 := size(ewds_file);

   first_try := true;

   second_try := true;

   j := (j1 + j2) / 2;

binary_search:
    loop
       --   TEXT_IO.PUT_LINE("J = " & INTEGER'IMAGE(INTEGER(J)));

       if (j1 = j2-1) or (j1 = j2) then
          if first_try  then
             j := j1;
             first_try := false;
          elsif second_try  then
             j := j2;
             second_try := false;
          else
             exit binary_search;
          end if;
       end if;

       --  Should D_K
       set_index(ewds_file, j);
       read(ewds_file, ewds);
       if  "<"(lower_case(ewds.w), input_word)  then  --  Not LTU, not u=v
          j1 := j;
          j := (j1 + j2) / 2;
       elsif  ">"(lower_case(ewds.w), input_word)  then
          j2 := j;
          j := (j1 + j2) / 2;
       else
          for i in reverse j1..j  loop
             set_index(ewds_file, ewds_direct_io.count(i));
             read(ewds_file, ewds);    --  Reads and advances index!!

             if "="(lower_case(ewds.w), input_word)  then
                load_output_array(ewds);

             else
                exit;
             end if;
          end loop;

          for i in j+1..j2  loop
             set_index(ewds_file, ewds_direct_io.count(i));
             read(ewds_file, ewds);

             if "="(lower_case(ewds.w), input_word)  then
                load_output_array(ewds);

             else
                exit binary_search;
             end if;
          end loop;
          exit binary_search;

       end if;
    end loop binary_search;

    if  words_mode(write_output_to_file)      then
       dump_output_array(output);
    else
       dump_output_array(current_output);
    end if;
exception
   when others  =>
      text_io.put_line("exception SEARCH NUMBER_OF_HITS = " &
                         integer'image(number_of_hits));
      raise;
end search_english;
