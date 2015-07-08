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

with Text_IO; use Text_IO;
with Strings_package; use Strings_package;
with config;
with word_parameters; use word_parameters;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with developer_parameters; use developer_parameters;
with word_package; use word_package;
with english_support_package; use english_support_package;
with dictionary_form;

procedure search_english(Input_english_word : String; pofs : part_of_speech_type := x) is
   use ewds_direct_io;
   Input_word : eword := lower_case(head(Input_english_word, eword_size));
   Input_pofs : constant part_of_speech_type := pofs;

   Output_array : ewds_array(1..500) := (others => null_ewds_record);
   number_of_hits : Integer := 0;
   j1, j2, j : ewds_direct_io.Count := 0;

   d_k : constant dictionary_kind := general;    --  For the moment

   ewds : ewds_record := null_ewds_record;

   first_try, second_try : Boolean := True;

   procedure load_Output_array(ewds : in ewds_record) is
   begin
      if ewds.pofs <= Input_pofs  then
         number_of_hits := number_of_hits + 1;
         Output_array(number_of_hits) := ewds;
      end if;
   end load_Output_array;

   --procedure TRIM_OUTPUT_ARRAY is
   procedure sort_Output_array is
      hits : Integer := 0;
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
               if Output_array(i+1).rank  >  Output_array(i).rank     or else

                 (Output_array(i+1).rank  =  Output_array(i).rank     and then
                    Output_array(i+1).freq  <  Output_array(i).freq)  or else

                 (Output_array(i+1).rank  =  Output_array(i).rank     and then
                    Output_array(i+1).freq  =  Output_array(i).freq   and then
                    Output_array(i+1).semi  <  Output_array(i).semi)
               then
                  dw := Output_array(i);
                  Output_array(i) := Output_array(i+1);
                  Output_array(i+1) := dw;
                  hits := hits + 1;
                  --PUT_LINE("HITS    " & INTEGER'IMAGE(HITS));
               end if;
            end loop inner_loop;
         end switch;
         exit hit_loop when hits = 0;
      end loop hit_loop;
   end sort_Output_array;

   procedure dump_Output_array(Output : in Text_IO.File_Type) is
      de : dictionary_entry := null_dictionary_entry;
      number_to_show : Integer := number_of_hits;
      one_screen : constant Integer := 6;
   begin
      --TEXT_IO.PUT_LINE("DUMP_OUTPUT");
      if number_of_hits = 0  then
         Text_IO.Put_Line(Output, "No Match");
      else
         sort_Output_array;

         --TEXT_IO.PUT_LINE("DUMP_OUTPUT SORTED");

         trimmed := False;
         if words_mode(trim_Output)  then
            if number_of_hits > one_screen  then
               number_to_show := one_screen;
               trimmed := True;
            else
               number_to_show := number_of_hits;
            end if;
         end if;

         for i in 1..number_to_show  loop
            Text_IO.New_Line(Output);

            do_pause:
            begin
               if Integer(Text_IO.Line(Output)) >
                  scroll_line_number + config.Output_screen_size
               then
                  pause(Output);
                  scroll_line_number := Integer(Text_IO.Line(Output));
               end if;
            end do_pause;

            dict_io.Read(dict_file(general), de, dict_io.Count(Output_array(i).n));
            Put(Output, dictionary_form(de));
            Text_IO.Put(Output, "   ");

            if de.part.pofs = n  then
               Text_IO.Put(Output, "  ");  decn_record_io.Put(Output, de.part.n.decl);
               Text_IO.Put(Output, "  " & gender_type'Image(de.part.n.gender) & "  ");
            end if;
            if de.part.pofs = v then
               Text_IO.Put(Output, "  ");  decn_record_io.Put(Output, de.part.v.con);
            end if;
            if (de.part.pofs = v)  and then  (de.part.v.kind in gen..perfdef)  then
               Text_IO.Put(Output, "  " & verb_kind_type'Image(de.part.v.kind) & "  ");
            end if;

            if words_mdev(show_dictionary_codes)    then
               Text_IO.Put(Output, " [");
               age_type_io.Put(Output, de.tran.age);
               area_type_io.Put(Output, de.tran.area);
               geo_type_io.Put(Output, de.tran.geo);
               frequency_type_io.Put(Output, de.tran.freq);
               source_type_io.Put(Output, de.tran.source);
               Text_IO.Put(Output, "]  ");
            end if;

            if words_mdev(show_dictionary) then
               Text_IO.Put(Output, ext(d_k) & ">");
            end if;
            --TEXT_IO.PUT_LINE("DUMP_OUTPUT SHOW");

            if words_mdev(show_dictionary_line)  then
               Text_IO.Put(Output, "("
                             & trim(Integer'Image(Output_array(i).n)) & ")");
            end if;

            Text_IO.New_Line(Output);

            --TEXT_IO.PUT_LINE("DUMP_OUTPUT MEAN");

            Text_IO.Put(Output, trim(de.mean));
            Text_IO.New_Line(Output);

         end loop;
         --TEXT_IO.PUT_LINE("DUMP_OUTPUT TRIMMED");

         if trimmed  then
            Put_Line(Output, "*");
         end if;

      end if;    --  On HITS = 0

   exception
      when others =>
         null;   --  If N not in DICT_FILE
   end dump_Output_array;

begin

   j1 := 1;
   j2 := Size(ewds_file);

   first_try := True;

   second_try := True;

   j := (j1 + j2) / 2;

   binary_search:
   loop
      --   TEXT_IO.PUT_LINE("J = " & INTEGER'IMAGE(INTEGER(J)));

      if (j1 = j2-1) or (j1 = j2) then
         if first_try  then
            j := j1;
            first_try := False;
         elsif second_try  then
            j := j2;
            second_try := False;
         else
            exit binary_search;
         end if;
      end if;

      --  Should D_K
      Set_Index(ewds_file, j);
      Read(ewds_file, ewds);
      if  "<"(lower_case(ewds.w), Input_word)  then  --  Not LTU, not u=v
         j1 := j;
         j := (j1 + j2) / 2;
      elsif  ">"(lower_case(ewds.w), Input_word)  then
         j2 := j;
         j := (j1 + j2) / 2;
      else
         for i in reverse j1..j  loop
            Set_Index(ewds_file, ewds_direct_io.Count(i));
            Read(ewds_file, ewds);    --  Reads and advances index!!

            if "="(lower_case(ewds.w), Input_word)  then
               load_Output_array(ewds);
            else
               exit;
            end if;
         end loop;

         for i in j+1..j2  loop
            Set_Index(ewds_file, ewds_direct_io.Count(i));
            Read(ewds_file, ewds);

            if "="(lower_case(ewds.w), Input_word)  then
               load_Output_array(ewds);
            else
               exit binary_search;
            end if;
         end loop;

         exit binary_search;
      end if;
   end loop binary_search;

   if  words_mode(Write_Output_to_file)      then
      dump_Output_array(Output);
   else
      dump_Output_array(Current_Output);
   end if;
exception
   when others  =>
      Text_IO.Put_Line("exception SEARCH NUMBER_OF_HITS = " &
                         Integer'Image(number_of_hits));
      raise;
end search_english;
