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
with inflections_package; use inflections_package;
with io_exceptions;
procedure makeinfl is
   package integer_io is new text_io.integer_io(integer);
   use text_io;
   use integer_io;
   use stem_key_type_io;
   use inflection_record_io;
   use quality_record_io;
   use ending_record_io;
   use age_type_io;
   use frequency_type_io;
   use lel_section_io;

   porting : constant boolean := true;    --FALSE for WAKEINFL;

   m : integer := 0;
   n1, n2, n3, n4, n5 : integer := 0;

   output : text_io.file_type;
   inflections_sections_file : lel_section_io.file_type;

   procedure file_inflections_sections is
      --  Reads the INFLECTS. file and prepares an inflections list
      --  Then it writes that list into an array
      --  Loads the inflection array into a file for later retrieval
      inflections_file : text_io.file_type;
      inflections_sections_file : lel_section_io.file_type;
      ir : inflection_record;
      line : string(1..100) := (others => ' ');
      last, l : integer := 0;
      sn : ending_size_type := ending_size_type'first;
      sx : character := ' ';

      type inflection_item;
      type inflection_list is access inflection_item;

      type inflection_item is
         record
            ir   : inflection_record;
            succ : inflection_list;
         end record;

      type latin_inflections is array (integer range 0..max_ending_size,
                                       character  range ' '..'z') of inflection_list;
      null_latin_inflections : constant latin_inflections := (others => (others => null));

      l_i : latin_inflections := null_latin_inflections;

      lel : lel_section := (others => null_inflection_record);
      j1, j2, j3, j4, j5 : integer := 0;

      procedure null_lel is
      begin
         for i in lel'range  loop
            lel(i) := null_inflection_record;
         end loop;
      end null_lel;

      procedure load_inflections_list is
         --  Takes the INFLECT. file and populates the L_I list of inflections
         --  indexed on ending size and last letter of ending
      begin
         put_line("Begin  LOAD_INFLECTIONS_LIST");
         number_of_inflections := 0;

         l_i := null_latin_inflections;
         open(inflections_file, in_file, inflections_full_name);
         text_io.put("INFLECTIONS file loading");
         while not end_of_file(inflections_file)  loop

        read_a_line:
            begin
               get_non_comment_line(inflections_file, line, last);

               if last > 0  then
                  get(line(1..last), ir, l);
                  sn := ir.ending.size;
                  if sn = 0  then
                     sx := ' ';
                  else
                     sx := ir.ending.suf(sn);
                  end if;
                  l_i(sn, sx) := new inflection_item'(ir, l_i(sn, sx));
                  number_of_inflections := number_of_inflections + 1;
                  --TEXT_IO.PUT(INTEGER'IMAGE(NUMBER_OF_INFLECTIONS) & "  "); INFLECTION_RECORD_IO.PUT(IR); NEW_LINE;
               end if;
            exception
               when constraint_error | io_exceptions.data_error  =>
                  put_line("****" & line(1..last));
            end read_a_line;

         end loop;
         close(inflections_file);
         put_line("INFLECTIONS_LIST LOADED   " & integer'image(number_of_inflections));

      end load_inflections_list;

      procedure list_to_lel_file  is
         --  From ILC (=L_I) list of inflections, prepares the LEL inflections array
         ilc : latin_inflections := l_i;
      begin

         create(inflections_sections_file, out_file, inflections_sections_name);

         null_lel;
         ilc := l_i;                              --  Resetting the list to start over
         while ilc(0, ' ') /= null  loop
            j5 := j5 + 1;
            lel(j5) := ilc(0, ' ').ir;
            ilc(0, ' ') := ilc(0, ' ').succ;
         end loop;
         write(inflections_sections_file, lel, 5);
         n5 := j5;

         null_lel;
         ilc := l_i;                              --  Resetting the list to start over
         for ch in character range 'a'..'z'  loop
            for n in reverse 1..max_ending_size  loop
               while ilc(n, ch) /= null  loop
                  if   not
                    (ilc(n, ch).ir.qual.pofs = pron  and then
                       (ilc(n, ch).ir.qual.pron.decl.which = 1  or
                          ilc(n, ch).ir.qual.pron.decl.which = 2))  then

                     if ch in inflections_section_1  then
                        j1 := j1 + 1;
                        lel(j1) := ilc(n, ch).ir;

                     end if;
                  end if;
                  ilc(n, ch) := ilc(n, ch).succ;
               end loop;
            end loop;
         end loop;
         write(inflections_sections_file, lel, 1);
         n1 := j1;

         null_lel;
         ilc := l_i;                              --  Resetting the list to start over
         for ch in character range 'a'..'z'  loop
            for n in reverse 1..max_ending_size  loop
               while ilc(n, ch) /= null  loop
                  if   not
                    (ilc(n, ch).ir.qual.pofs = pron  and then
                       (ilc(n, ch).ir.qual.pron.decl.which = 1  or
                          ilc(n, ch).ir.qual.pron.decl.which = 2))  then

                     if ch in inflections_section_2  then
                        j2 := j2 + 1;
                        lel(j2) := ilc(n, ch).ir;

                     end if;
                  end if;
                  ilc(n, ch) := ilc(n, ch).succ;
               end loop;
            end loop;
         end loop;
         write(inflections_sections_file, lel, 2);
         n2 := j2;

         null_lel;
         ilc := l_i;                              --  Resetting the list to start over
         for ch in character range 'a'..'z'  loop
            for n in reverse 1..max_ending_size  loop
               while ilc(n, ch) /= null  loop
                  if   not
                    (ilc(n, ch).ir.qual.pofs = pron  and then
                       (ilc(n, ch).ir.qual.pron.decl.which = 1  or
                          ilc(n, ch).ir.qual.pron.decl.which = 2))  then

                     if ch in inflections_section_3  then
                        j3 := j3 + 1;
                        lel(j3) := ilc(n, ch).ir;

                     end if;
                  end if;
                  ilc(n, ch) := ilc(n, ch).succ;
               end loop;
            end loop;
         end loop;
         write(inflections_sections_file, lel, 3);
         n3 := j3;

         null_lel;
         ilc := l_i;                              --  Resetting the list to start over
         for ch in character range 'a'..'z'  loop
            for n in reverse 1..max_ending_size  loop
               while ilc(n, ch) /= null  loop
                  if   not
                    (ilc(n, ch).ir.qual.pofs = pron  and then
                       (ilc(n, ch).ir.qual.pron.decl.which = 1  or
                          ilc(n, ch).ir.qual.pron.decl.which = 2))  then

                     if (ch in inflections_section_4)  then
                        j4 := j4 + 1;
                        lel(j4) := ilc(n, ch).ir;

                     end if;
                  end if;
                  ilc(n, ch) := ilc(n, ch).succ;
               end loop;
            end loop;
         end loop;

         --  Now put the PACK in 4            --  Maybe it should be in 5 ????
         ilc := l_i;                              --  Resetting the list to start over
         for ch in character range 'a'..'z'  loop
            for n in reverse 1..max_ending_size  loop
               while ilc(n, ch) /= null  loop
                  if (ilc(n, ch).ir.qual.pofs = pron  and then
                        (ilc(n, ch).ir.qual.pron.decl.which = 1  or
                           ilc(n, ch).ir.qual.pron.decl.which = 2))  then  --  2 no longer PACK

                     j4 := j4 + 1;
                     lel(j4) := ilc(n, ch).ir;

                  end if;
                  ilc(n, ch) := ilc(n, ch).succ;
               end loop;
            end loop;
         end loop;
         write(inflections_sections_file, lel, 4);
         n4 := j4;

         close(inflections_sections_file);

      end list_to_lel_file;

   begin

      load_inflections_list;

      text_io.set_col(33);
      text_io.put("--  ");
      integer_io.put(number_of_inflections);
      text_io.put_line(" entries    --  Loaded correctly");

      list_to_lel_file;                     --  Load arrays to file
      text_io.put_line("File INFLECTS.SEC  --  Loaded");

   exception
      when others =>
         text_io.put_line("Exception in FILE_INFLECTIONS_SECTIONS");
   end file_inflections_sections;

begin

   put_line("Produces INFLECTS.SEC file from INFLECTS.");

   file_inflections_sections;

   if not porting  then
      put_line("using FILE_INFLECTIONS_SECTIONS, also produces INFLECTS.LIN file");

      create(output, out_file, "INFLECTS.LIN");
   end if;

   establish_inflections_section;

   lel_section_io.open(inflections_sections_file, in_file,
                       inflections_sections_name);

   if not porting  then
      for i in bel'range    loop                     --  Blank endings
         if  bel(i) /= null_inflection_record  then
            m := m + 1;
            put(output, bel(i).qual);
            set_col(output, 50);
            put(output, bel(i).key, 1);
            set_col(output, 52);
            put(output, bel(i).ending);
            set_col(output, 62);
            put(output, bel(i).age);
            set_col(output, 64);
            put(output, bel(i).freq);
            new_line(output);
         end if;
      end loop;
   end if;

   for n in 1..4  loop
      read(inflections_sections_file, lel, lel_section_io.positive_count(n));

      if not porting  then
         for i in lel'range    loop                     --  Non-blank endings
            if  lel(i) /= null_inflection_record  then
               m := m + 1;
               put(output, lel(i).qual);
               set_col(output, 50);
               put(output, lel(i).key, 1);
               set_col(output, 52);
               put(output, lel(i).ending);
               set_col(output, 62);
               put(output, lel(i).age);
               set_col(output, 64);
               put(output, lel(i).freq);
               new_line(output);
            end if;
         end loop;
      end if;

   end loop;

   new_line;
   put("LINE_INFLECTIONS finds "); put(m); put_line(" inflections"); new_line;

   for i in character range ' '..' '  loop
      integer_io.put(0); put("    "); put(i); put("    "); put(belf(0, i));
      put("  ");   put(bell(0, i));
      put("    "); put(bell(0, i) - belf(0, i) + 1); new_line;
   end loop;
   new_line;

   for i in character range 'a'..'z'  loop
      for n in reverse 1..max_ending_size  loop
         if (lell(n, i) > 0)  and then (lelf(n, i) <= lell(n, i))  then
            put(n); put("    "); put(i); put("    "); put(lelf(n, i));
            put("  ");   put(lell(n, i));
            put("    "); put(lell(n, i) - lelf(n, i) + 1); new_line;
         end if;
      end loop;
   end loop;
   new_line;

   for i in character range 'a'..'z'  loop
      for n in reverse 1..max_ending_size  loop
         if (pell(n, i) > 0)  and then (pelf(n, i) <= pell(n, i))  then
            put(n); put("    "); put(i); put("    "); put(pelf(n, i));
            put("  ");   put(pell(n, i));
            put("    "); put(pell(n, i) - pelf(n, i) + 1); new_line;
         end if;
      end loop;
   end loop;
   new_line;

   new_line;
   put(n5);  put("    ");
   put(n1);  put("    ");
   put(n2);  put("    ");
   put(n3);  put("    ");
   put(n4);  put("    ");
   new_line;

end makeinfl;
