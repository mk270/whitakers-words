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

with addons_package; use addons_package;
with latin_file_names; use latin_file_names;
with strings_package; use strings_package;
with config;  use config;
with uniques_package; use uniques_package;
with word_parameters; use word_parameters;
with preface;
with developer_parameters; use developer_parameters;
with line_stuff; use line_stuff;
with english_support_package; use english_support_package;
package body word_package is

   inflections_sections_file : lel_section_io.file_type;

   procedure pause(output : text_io.file_type) is
      pause_line : string(1..300);
      pause_last : integer := 0;
   begin
      if words_mdev(pause_in_screen_output)  then
         if method = interactive  then
            if text_io.name(output) =
              text_io.name(text_io.standard_output)  then
               text_io.put_line(text_io.standard_output,
                 "                          MORE - hit RETURN/ENTER to continue");
               text_io.get_line(text_io.standard_input, pause_line, pause_last);
            end if;
         elsif method = command_line_input  then
            text_io.put_line(text_io.standard_output,
              "                          MORE - hit RETURN/ENTER to continue");
            text_io.get_line(text_io.standard_input, pause_line, pause_last);
         elsif method = command_line_files  then
            null;                       --  Do not PAUSE
         end if;
      end if;
   exception
      when others  =>
         text_io.put_line("Unexpected exception in PAUSE");
   end pause;

   function min(a, b : integer) return integer is
   begin
      if a <= b  then
         return a; end if;
         return b;
   end min;

   function ltu(c, d : character) return boolean is
   begin
      if (d = 'v')  then
         if (c < 'u')  then
            return true;
         else
            return false;
         end if;
      elsif (d = 'j')  then
         if (c < 'i')  then
            return true;
         else
            return false;
         end if;
      elsif (d = 'V')  then
         if (c < 'U')  then
            return true;
         else
            return false;
         end if;
      elsif (d = 'J')  then
         if (c < 'I')  then
            return true;
         else
            return false;
         end if;
      else
         return c < d;
      end if;
   end ltu;

   function equ(c, d : character) return boolean is
   begin
      if (d = 'u') or (d = 'v')  then
         if (c = 'u') or (c = 'v')  then
            return true;
         else
            return false;
         end if;
      elsif (d = 'i') or (d = 'j')  then
         if (c = 'i') or (c = 'j')  then
            return true;
         else
            return false;
         end if;
      elsif (d = 'U') or (d = 'V')  then
         if (c = 'U') or (c = 'V')  then
            return true;
         else
            return false;
         end if;
      elsif (d = 'I') or (d = 'J')  then
         if (c = 'I') or (c = 'J')  then
            return true;
         else
            return false;
         end if;
      else
         return c = d;
      end if;
   end equ;

   function gtu(c, d : character) return boolean is
   begin
      if d = 'u'  then
         if (c > 'v')  then
            return true;
         else
            return false;
         end if;
      elsif d = 'i'  then
         if (c > 'j')  then
            return true;
         else
            return false;
         end if;
      elsif d = 'U'  then
         if (c > 'V')  then
            return true;
         else
            return false;
         end if;
      elsif d = 'I'  then
         if (c > 'J')  then
            return true;
         else
            return false;
         end if;
      else
         return c > d;
      end if;
   end gtu;

   function ltu(s, t : string) return boolean is
   begin
      for i in 1..s'length  loop   --  Not TRIMed, so same length
         if equ(s(s'first+i-1), t(t'first+i-1))  then
            null;
         elsif gtu(s(s'first+i-1), t(t'first+i-1))  then
            return false;
         elsif ltu(s(s'first+i-1), t(t'first+i-1))  then
            return true;
         end if;
      end loop;
      return false;
   end ltu;

   function gtu(s, t : string) return boolean is
   begin
      for i in 1..s'length  loop   --  Not TRIMed, so same length
         if equ(s(s'first+i-1), t(t'first+i-1))  then
            null;
         elsif ltu(s(s'first+i-1), t(t'first+i-1))  then
            return false;
         elsif gtu(s(s'first+i-1), t(t'first+i-1))  then
            return true;
         end if;
      end loop;
      return false;
   end gtu;

   function equ(s, t : string) return boolean is
   begin
      if s'length /= t'length  then
         return false;
      end if;

      for i in 1..s'length  loop
         if not equ(s(s'first+i-1), t(t'first+i-1))  then
            return false;
         end if;
      end loop;

      return true;
   end equ;

   procedure run_uniques(s : in string; unique_found : out boolean;
                                        pa : in out parse_array; pa_last : in out integer) is
      sl : constant string        --  BAD NAME!!!!!!!!!!!!!!!!!!
        := lower_case(trim(s));
      st : constant stem_type := head(sl, max_stem_size);
      unql : unique_list;   --  Unique list for a letter
   begin
      unique_found := false;
      if sl(sl'first) = 'v'  then
         unql := unq('u');   --  Unique list for a letter
      elsif sl(sl'first) = 'j'  then
         unql := unq('i');   --  Unique list for a letter
      else
         unql := unq(sl(sl'first));   --  Unique list for a letter
      end if;

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE("Called UNIQUES with =>" & SL & "|");

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE("UNQL ");

      while unql /= null  loop
         --  If there is a match, add to PA
         --TEXT_IO.PUT_LINE("UNIQUE =>" & UNQL.PR.STEM);
         --if ST = LOWER_CASE(UNQL.PR.STEM)  then
         if equ(st, lower_case(unql.stem)) then
            pa_last := pa_last + 1;
            pa(pa_last) := (unql.stem,
                            (unql.qual,
                             0,
                             null_ending_record,
                             x,
                             x),
                            unique,
                            unql.mnpc);

            --TEXT_IO.PUT_LINE("UNIQUE    HIT     *********" & INTEGER'IMAGE(PA_LAST));
            unique_found := true;
         end if;
         unql := unql.succ;
      end loop;

   end run_uniques;

   procedure run_inflections(s : in string; sl : in out sal;
                                            restriction : dict_restriction := regular) is
      --  Trys all possible inflections against the input word in S
      --  and constructs a STEM_LIST of those that survive SL
      use lel_section_io;
      use inflection_record_io;
      word : constant string := lower_case(trim(s));
      last_of_word : constant character := word(word'last);
      length_of_word   : constant integer := word'length;
      stem_length  : integer := 0;
      pr   : parse_record;
      m : integer := 1;

   begin
      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE("Called RUN_INFLECTIONS with =>" & WORD & "|");
      if word'length = 0  then
         sl(m) := null_parse_record;
         return;
      end if;

      sa := not_a_stem_array;

      --  Add all of these to list of possible ending records
      --  since the blank ending agrees with everything
      --  PACK/PRON have no blank endings
      if ((restriction /= pack_only) and (restriction /= qu_pron_only))  and then
        (word'length <= max_stem_size)  then
         for i in belf(0, ' ')..bell(0, ' ')  loop
            pr := (word & null_stem_type(length_of_word+1..stem_type'length),
                   bel(i), default_dictionary_kind, null_mnpc);
            sl(m) := pr;
            m := m + 1;

         end loop;
         sa(length_of_word) := pr.stem;  --  Is always a possibility (null ending)

      end if;

      --  Here we read in the INFLECTIONS_SECTION that is applicable
      if restriction = regular  then
         case last_of_word is
            when 'a' | 'c' | 'd' | 'e' | 'i'  =>
               read(inflections_sections_file, lel, 1);
            when 'm' | 'n' | 'o' | 'r'  =>
               read(inflections_sections_file, lel, 2);
            when 's'  =>
               read(inflections_sections_file, lel, 3);
            when 't' | 'u'  =>
               read(inflections_sections_file, lel, 4);
            when others  =>
               --PUT_LINE("Only blank inflections are found");
               return;
         end case;
      elsif restriction = pack_only  or restriction = qu_pron_only  then
         read(inflections_sections_file, lel, 4);
      end if;

      --  Now do the non-blank endings      --  Only go to LENGTH_OF_WORD
      for z in reverse 1..min(max_ending_size, length_of_word)  loop

         --  Check if Z agrees with a PDL SIZE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         --  Maybe make PDL on size, if it has to be a list, or order by size if array
         if lell(z, last_of_word) > 0  then   --  Any likely inflections at all

            for i in lelf(z, last_of_word)..lell(z, last_of_word) loop
               if equ(lower_case(lel(i).ending.suf(1..z)),
                      lower_case(word(word'last-z+1..word'last)))  then
                  --  Add to list of possible ending records
                  --STEM_LENGTH := WORD'LENGTH - LEL(I).ENDING.SIZE;
                  stem_length := word'length - z;
                  --PUT(STEM_LENGTH);
                  --TEXT_IO.PUT_LINE("#######################################################");

                  if stem_length <= max_stem_size  then  --  Reject too long words
                                                         --  Check if LEL IR agrees with PDL IR  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                     pr := (word(word'first..stem_length) &
                              null_stem_type(stem_length+1..max_stem_size),
                            lel(i), default_dictionary_kind, null_mnpc);
                     sl(m) := pr;
                     m := m + 1;

                     sa(stem_length) := pr.stem;    --  Gets set dozens of times
                                                    --  Could order the endings by length (suffix sort) so length changes slowly

                     --PUT_LINE("LENGTH = " & INTEGER'IMAGE(STEM_LENGTH)
                     --& "   SA =>" & PR.STEM & "|");

                  end if;
               end if;
            end loop;

         end if;
      end loop;
   end run_inflections;

   procedure try_to_load_dictionary(d_k : dictionary_kind) is
   begin
      stem_io.open(stem_file(d_k), stem_io.in_file,
                   add_file_name_extension(stem_file_name,
                                           dictionary_kind'image(d_k)));
      dict_io.open(dict_file(d_k), dict_io.in_file,
                   add_file_name_extension(dict_file_name,
                                           dictionary_kind'image(d_k)));
      load_indices_from_indx_file(d_k);
      dictionary_available(d_k) := true;

   exception
      when others  =>
         dictionary_available(d_k) := false;
   end try_to_load_dictionary;

   procedure dictionary_search(ssa : stem_array_type;
                               d_k : dictionary_kind;
                               restriction : dict_restriction := regular) is
      --  Prepares a PDL list of possible dictionary hits
      --  Search a dictionary (D_K) looking for all stems that match
      --  any of the stems that are physically possible with Latin inflections
      use stem_io;

      --type NAT_32 is range 0..2**31-1;   --###############
      j, j1, j2, jj : stem_io.count := 0;

      index_on : constant string := ssa(ssa'last);
      index_first, index_last : stem_io.count := 0;
      ds : dictionary_stem;
      first_try, second_try : boolean := true;

      function first_two(w : string) return string is
         --  'v' could be represented by 'u', like the new Oxford Latin Dictionary
         --  Fixes the first two letters of a word/stem which can be done right
         s : constant string := lower_case(w);
         ss : string(w'range) := w;

         function ui(c : character) return character  is
         begin
            if (c = 'v')   then
               return 'u';
            elsif (c = 'V')  then
               return 'U';
            elsif (c = 'j')  then
               return 'i';
            elsif (c = 'J')  then
               return 'I';
            else
               return c;
            end if;
         end ui;

      begin

         if s'length = 1  then
            ss(s'first) := ui(w(s'first));
         else
            ss(s'first)   := ui(w(s'first));
            ss(s'first+1) := ui(w(s'first+1));
         end if;

         return ss;
      end first_two;

      procedure load_pdl is
      begin
         case restriction is
            when regular    =>
               if not (ds.part.pofs = pack  or
                         (ds.part.pofs = pron  and then
                            (ds.part.pron.decl.which = 1)))  then
                  pdl_index := pdl_index + 1;
                  pdl(pdl_index) := pruned_dictionary_item'(ds, d_k);
               end if;

            when pack_only  =>
               if ds.part.pofs = pack  then
                  pdl_index := pdl_index + 1;
                  pdl(pdl_index) := pruned_dictionary_item'(ds, d_k);
               end if;

            when qu_pron_only  =>
               if ds.part.pofs = pron  and then
                 (ds.part.pron.decl.which = 1)  then
                  pdl_index := pdl_index + 1;
                  pdl(pdl_index) := pruned_dictionary_item'(ds, d_k);
               end if;

            when others =>
               pdl_index := pdl_index + 1;
               pdl(pdl_index) := pruned_dictionary_item'(ds, d_k);
         end case;

      end load_pdl;

   begin
      --  Now go through the dictionary list DL for the first letters
      --  and make a reduced dictionary list PDL

      if d_k = local  then
         index_first := first_index((first_two(index_on)(1), 'a'), d_k);
         index_last  := last_index((first_two(index_on)(1), 'a'), d_k);
      else
         index_first := first_index(first_two(index_on), d_k);
         index_last  := last_index(first_two(index_on), d_k);
      end if;

      if index_first > 0  and then index_first <= index_last then

         j1 := index_first;    --######################
         j2 := index_last;

     stem_array_loop:
         for k in ssa'range  loop
            if trim(ssa(k))'length > 1  then
               --  This may be checking for 0 and 1 letter SSAs which are done elsewhere

               if d_k = local  then    --  Special processing for unordered DICT.LOC
                  for j in j1..j2  loop       --  Sweep exaustively through the scope
                     set_index(stem_file(d_k), stem_io.count(j));
                     read(stem_file(d_k), ds);

                     if equ(lower_case(ds.stem), ssa(k))  then
                        --TEXT_IO.PUT_LINE("HIT LOC =   " & DS.STEM & " - " & SSA(K));
                        load_pdl;
                     end if;
                  end loop;

               else                     --  Regular dictionaries

                  first_try := true;

                  second_try := true;

                  j := (j1 + j2) / 2;

              binary_search:
                  loop

                     if (j1 = j2-1) or (j1 = j2) then
                        if first_try  then
                           j := j1;
                           first_try := false;
                        elsif second_try  then
                           j := j2;
                           second_try := false;
                        else
                           jj := j;
                           exit binary_search;
                        end if;
                     end if;

                     set_index(stem_file(d_k), j);
                     read(stem_file(d_k), ds);

                     if  ltu(lower_case(ds.stem), ssa(k))  then
                        j1 := j;
                        j := (j1 + j2) / 2;
                     elsif  gtu(lower_case(ds.stem), ssa(k))  then
                        j2 := j;
                        j := (j1 + j2) / 2;
                     else
                        for i in reverse j1..j  loop
                           set_index(stem_file(d_k), stem_io.count(i));
                           read(stem_file(d_k), ds);

                           if equ(lower_case(ds.stem), ssa(k))  then
                              jj := i;
                              load_pdl;

                           else
                              exit;
                           end if;
                        end loop;

                        for i in j+1..j2  loop
                           set_index(stem_file(d_k), stem_io.count(i));
                           read(stem_file(d_k), ds);

                           if equ(lower_case(ds.stem), ssa(k))  then
                              jj := i;
                              load_pdl;

                           else
                              exit binary_search;
                           end if;
                        end loop;
                        exit binary_search;

                     end if;
                  end loop binary_search;
                  j1 := jj;
                  j2 := index_last;

               end if;               --  On LOCAL check
            end if;               --  On LENGTH > 1
         end loop stem_array_loop;
      end if;
   end dictionary_search;

   procedure search_dictionaries(ssa : in stem_array_type;
                                                       restriction : dict_restriction := regular) is
      use stem_io;
      fc : character := ' ';
   begin
      pdl := (others => null_pruned_dictionary_item);
      pdl_index := 0;
      --PUT_LINE("Search for blank stems");
      --  BDL is always used, so it is loaded initially and not called from disk
      --  Check all stems of the dictionary entry against the reduced stems

      --  Determine if there is a pure blank "  " stem
      if len(ssa(ssa'first)) = 0    then   --  a size would help?
                                           --PUT("HIT on blank stem   I = ");PUT('1');
                                           --PUT("  STEM = ");PUT_LINE(BDL(1).STEM);
                                           --PDL := new PRUNED_DICTIONARY_ITEM'(BDL(1), GENERAL, PDL);
         pdl_index := pdl_index + 1;
         pdl(pdl_index) := pruned_dictionary_item'(bdl(1), general);
      end if;
      --  Now there is only one blank stem (2 of to_be), but need not always be so

      --  Determine if there is a blank stem  (SC = ' ')
      --  Prepare for the posibility that one stem is short but there are others
      fc := ' ';
      if ssa(ssa'first)(1) = ' ' then
         if ssa'length > 1  and then ssa(ssa'first+1)(2) = ' '  then
            fc := ssa(ssa'first+1)(1);
         end if;
      elsif ssa(ssa'first)(2) = ' '  then
         fc := ssa(ssa'first)(1);
      end if;

      --  If there is a single letter stem  (FC /= ' ') then
      if fc /= ' '  then
         for i in 2..bdl_last  loop
            --  Check all stems of the dictionary entry against the reduced stems
            --if LOWER_CASE(BDL(I).STEM(1)) = FC  then
            if equ(lower_case(bdl(i).stem(1)),  fc)  then
               --PUT("HIT on 1 letter stem   I = ");PUT(I);PUT("  STEM = ");PUT_LINE(BDL(I).STEM);
               pdl_index := pdl_index + 1;
               pdl(pdl_index) := pruned_dictionary_item'(bdl(i), general);
               --  D_K set to GENERAL, but should not SPE have a chance? !!!!!!!!!
            end if;
         end loop;
      end if;

      if ssa'length = 0  then
         --        PUT_LINE("Empty stem array, don't bother searching");
         return;
         --      elsif LEN(SSA(SSA'LAST)) <= 1  then
         --        PUT_LINE("No two letter stems, have done searching");
         --      else
         --        PUT_LINE("Searching Dictionaries");
      end if;

      for d_k in dictionary_kind  loop
         if dictionary_available(d_k)  then
            if not is_open(stem_file(d_k))  then
               open(stem_file(d_k), stem_io.in_file,
                    add_file_name_extension(stem_file_name,
                                            dictionary_kind'image(d_k)));
            end if;
            dictionary_search(ssa, d_k, restriction);
            close(stem_file(d_k));  --??????
         end if;
      end loop;

   end search_dictionaries;

   procedure change_language(c : character) is
   begin  if upper_case(c) = 'L'  then
      language := latin_to_english;
      preface.put_line("Language changed to " & language_type'image(language));
   elsif upper_case(c) = 'E'  then
      if english_dictionary_available(general)  then
         language:= english_to_latin;
         preface.put_line("Language changed to " & language_type'image(language));
         preface.put_line("Input a single English word (+ part of speech - N, ADJ, V, PREP, ...)");
      else
         preface.put_line("No English dictionary available");
      end if;
   else
      preface.put_line("Bad LANGAUGE input - no change, remains " & language_type'image(language));
   end if;
   exception
      when others  =>
         preface.put_line("Bad LANGAUGE input - no change, remains " & language_type'image(language));
   end change_language;

   procedure word(raw_word : in string;
                  pa : in out parse_array; pa_last : in out integer) is

      input_word : constant string := lower_case(raw_word);
      pa_save : constant integer := pa_last;

      unique_found : boolean := false;

      ss, sss : sal := (others => null_parse_record);

      procedure order_stems(sx : in out sal) is
         use inflection_record_io;
         use dict_io;
         hits : integer := 0;
         sl : sal := sx;
         sl_last : integer := 0;
         sm : parse_record;
      begin
         if sx(1) = null_parse_record  then
            return; end if;
            --PUT_LINE("ORDERing_STEMS");

            for i in sl'range  loop
               exit when sl(i) = null_parse_record;
               sl_last := sl_last + 1;
            end loop;
            --PUT_LINE("In ORDER  SL_LAST = " & INTEGER'IMAGE(SL_LAST));

            --  Bubble sort since this list should usually be very small (1-5)
        hit_loop:
            loop
               hits := 0;

           switch:
               begin
                  --  Need to remove duplicates in ARRAY_STEMS
                  --  This sort is very sloppy
                  --  One problem is that it can mix up some of the order of PREFIX, XXX, LOC
                  --  I ought to do this for every set of results from different approaches
                  --  not just in one fell swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              inner_loop:
                  for i in 1..sl_last-1  loop
                     if sl(i+1) /= null_parse_record  then
                        if (sl(i+1).mnpc < sl(i).mnpc)  or else
                          (sl(i+1).mnpc = sl(i).mnpc   and then
                             sl(i+1).ir.ending.size < sl(i).ir.ending.size)  or else
                          (sl(i+1).mnpc = sl(i).mnpc   and then
                             sl(i+1).ir.ending.size = sl(i).ir.ending.size  and then
                             sl(i+1).ir.qual < sl(i).ir.qual)  or else
                          (sl(i+1).mnpc = sl(i).mnpc   and then
                             sl(i+1).ir.ending.size = sl(i).ir.ending.size  and then
                             sl(i+1).ir.qual = sl(i).ir.qual   and then
                             sl(i+1).d_k  < sl(i).d_k)   then
                           sm := sl(i);
                           sl(i) := sl(i+1);                                                                            sl(i+1) := sm;
                           sl(i+1) := sm;
                           hits := hits + 1;
                        end if;
                     else
                        exit inner_loop;
                     end if;
                  end loop inner_loop;

               end switch;

               exit when hits = 0;
            end loop hit_loop;
            sx := sl;
      end order_stems;

      procedure array_stems(sx : in sal;
                            pa : in out parse_array; pa_last : in out integer) is
         sl : constant sal := sx;
         opr : parse_record := null_parse_record;
      begin

         if sl(1) = null_parse_record  then
            return;
         else

            opr := null_parse_record;
            for i in sl'range  loop
               if sl(i) /= null_parse_record  then
                  --PUT('*'); PUT(SL(I)); NEW_LINE;

              supress_key_check:
                  declare

                     function "<=" (a, b : parse_record) return boolean is
                        use dict_io;
                     begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                        if a.ir.qual = b.ir.qual        and then
                          a.mnpc = b.mnpc  then
                           return true;
                        else
                           return false;
                        end if;
                     end "<=";

                  begin
                     if sl(i) <= opr  then       --  Get rid of duplicates, if ORDER is OK
                                                 --PUT('-'); PUT(SL(I)); NEW_LINE;
                        null;
                     else
                        pa_last := pa_last + 1;
                        pa(pa_last) := sl(i);
                        opr := sl(i);
                     end if;
                  end supress_key_check;

               else
                  exit;
               end if;
            end loop;

         end if;

      end array_stems;

      procedure reduce_stem_list(sl : in sal; sxx : in out sal;
                                              --  Need in out if want to print it at the end
                                              --procedure REDUCE_STEM_LIST(SL : in SAL; SXX : out SAL;
                                              prefix : in prefix_item := null_prefix_item;
                                              suffix : in suffix_item := null_suffix_item) is
         mnpc_part : mnpc_type := null_mnpc;
         pdl_part : part_entry;
         com : comparison_type := x;
         num_sort : numeral_sort_type := x;
         ls : integer := 0;
         m : integer := 0;

         pdl_key : stem_key_type;
         pdl_p   : part_of_speech_type;
         --sl_key  : stem_key_type;
         --sl_p    : part_of_speech_type;

         function "<=" (left, right : part_of_speech_type) return boolean is
         begin
            if right = left  or else
              (left = pack and right = pron)  or else
              right = x    then
               return true;
            else
               return false;
            end if;
         end "<=";

         function "<=" (left, right : gender_type)   return boolean is
         begin
            if (right = left   or else
                  (right = c  and left /= n)  or else
                  (right = x))  then
               return true;
            else
               return false;
            end if;
         end "<=";

         function "<=" (left, right : stem_key_type)   return boolean is
         begin
            if (right = left   or else
                  (right = 0))  then
               return true;
            else
               return false;
            end if;
         end "<=";

      begin
         sxx := (others => null_parse_record);  --  Essentially initializing
         --  For the reduced dictionary list PDL
         m := 0;
     on_pdl:
         for j in 1..pdl_index  loop

            pdl_part := pdl(j).ds.part;
            pdl_key := pdl(j).ds.key;
            mnpc_part := pdl(j).ds.mnpc;

            --  Is there any point in going through the process for this PDL
            pdl_p  := pdl(j).ds.part.pofs;  --  Used only for FIX logic below

            --  If there is no SUFFIX then carry on
            if (suffix = null_suffix_item)  then  --  No suffix working, fall through
                                                  --PUT_LINE("No SUFFIX in REDUCE - Fall through to PREFIX check ");
               null;
            elsif
              (pdl_p = n    and then pdl_part.n.decl = (9, 8)) or  --  No suffix for
              (pdl_p = adj  and then pdl_part.adj.decl = (9, 8)) then  --  abbreviations
                                                                       --   Can be no suffix on abbreviation");
               goto end_of_pdl_loop;
            else                      --  There is SUFFIX, see if it agrees with PDL
               if pdl_p <= suffix.entr.root  and then     --  Does SUFFIX agree in ROOT
                 ((pdl_key <= suffix.entr.root_key)  or else
                    ((pdl_key = 0) and then
                       ((pdl_p = n) or (pdl_p = adj) or (pdl_p = v)) and then
                       ((suffix.entr.root_key = 1) or (suffix.entr.root_key = 2))))  then
                  --PUT_LINE("HIT HIT HIT HIT HIT HIT HIT HIT HIT     SUFFIX SUFFIX    in REDUCE");
                  case suffix.entr.target.pofs is      --  Transform PDL_PART to TARGET
                     when n =>
                        pdl_part := (n, suffix.entr.target.n);
                     when pron =>
                        pdl_part := (pron, suffix.entr.target.pron);
                     when adj =>
                        pdl_part := (adj, suffix.entr.target.adj);
                     when num =>
                        pdl_part := (num, suffix.entr.target.num);
                     when adv =>
                        pdl_part := (adv, suffix.entr.target.adv);
                     when v =>
                        pdl_part := (v, suffix.entr.target.v);
                     when others  =>
                        null;                  --  No others so far, except X = all
                  end case;
                  pdl_key := suffix.entr.target_key;
                  pdl_p  := pdl_part.pofs;  --  Used only for FIX logic below
                                            --PUT("    Changed to    "); PUT(PDL_PART); PUT(PDL_KEY); NEW_LINE;

               else
                  --PUT_LINE("In REDUCE_STEM_LIST   There is no legal suffix");
                  --            exit;
                  goto end_of_pdl_loop;
               end if;
            end if;

            if (prefix = null_prefix_item)  then      --  No PREFIX, drop through
                                                      --PUT_LINE("No PREFIX in REDUCE - Fall through to MATCHing ");
               null;
            elsif
              (pdl_p = n    and then pdl_part.n.decl = (9, 8)) or  --  No prefix for
              (pdl_p = adj  and then pdl_part.adj.decl = (9, 8)) or --  abbreviations
              (pdl_p = interj  or pdl_p = conj) then  --  or INTERJ or CONJ
                                                      --PUT_LINE("In REDUCE_STEM_LIST   no prefix on abbreviationi, interj, conj");
               goto end_of_pdl_loop;
            else
               if (pdl_p = prefix.entr.root)  or    --  = ROOT
                 (pdl_part.pofs = prefix.entr.root)  then  --  or part mod by suf
                  null;
               elsif (prefix.entr.root = x)  then  --   or ROOT = X
                  null;
               else
                  goto end_of_pdl_loop;
               end if;
            end if;

            --  SUFFIX and PREFIX either agree or don't exist (agrees with everything)
            ls := len(add_suffix(add_prefix(pdl(j).ds.stem, prefix), suffix));
        on_sl:
            for i in sl'range loop
               exit when sl(i) = null_parse_record;

               if ls  = len(sl(i).stem)  then

                  --  Scan through the whole unreduced stem list
                  --  Single out those stems that match (pruned) dictionary entries
                  --^^^^^^^^^^^^^^^^^should be able to do this better with new arrangement

                  --sl_key := sl(i).ir.key;
                  --sl_p := sl(i).ir.qual.pofs;

                  if (
                      ((pdl_key <= sl(i).ir.key) )  or else
                        ((pdl_key = 0)  and then
                           (((pdl_p = n) or (pdl_p = adj) or (pdl_p = v)) and then
                              ((sl(i).ir.key = 1) or (sl(i).ir.key = 2)) ))
                     )  and then   --  and KEY
                    ( pdl_part.pofs  = eff_part(sl(i).ir.qual.pofs) )  then

                     if
                       (pdl_part.pofs = n                                and then
                          pdl_part.n.decl <= sl(i).ir.qual.n.decl            and then
                          pdl_part.n.gender <= sl(i).ir.qual.n.gender)             then
                        --  Need to transfer the gender of the noun dictionary item
                        m := m + 1;
                        sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                   ir => (
                                          qual => (
                                                   pofs => n,
                                                   n => (
                                                         pdl_part.n.decl,
                                                         sl(i).ir.qual.n.cs,
                                                         sl(i).ir.qual.n.number,
                                                         pdl_part.n.gender  )  ),
                                          key => sl(i).ir.key,
                                          ending => sl(i).ir.ending,
                                          age => sl(i).ir.age,
                                          freq => sl(i).ir.freq),
                                   d_k => pdl(j).d_k,
                                   mnpc => mnpc_part);

                     elsif
                       (pdl_part.pofs = pron                             and then
                          pdl_part.pron.decl <= sl(i).ir.qual.pron.decl)          then
                        --PUT(" HIT  PRON  ");
                        --  Need to transfer the kind of the pronoun dictionary item
                        m := m + 1;
                        sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                   ir => (
                                          qual => (
                                                   pofs => pron,
                                                   pron => (
                                                            pdl_part.pron.decl,
                                                            sl(i).ir.qual.pron.cs,
                                                            sl(i).ir.qual.pron.number,
                                                            sl(i).ir.qual.pron.gender  )  ),
                                          key => sl(i).ir.key,
                                          ending => sl(i).ir.ending,
                                          age => sl(i).ir.age,
                                          freq => sl(i).ir.freq),
                                   d_k => pdl(j).d_k,
                                   mnpc => mnpc_part);

                     elsif (pdl_part.pofs = adj)                          and then
                       (pdl_part.adj.decl <= sl(i).ir.qual.adj.decl)     and then
                       ((sl(i).ir.qual.adj.co   <= pdl_part.adj.co  ) or
                          ((sl(i).ir.qual.adj.co = x)  or (pdl_part.adj.co = x)))    then
                        --  Note the reversal on comparisom
                        --PUT(" HIT  ADJ   ");
                        --  Need to transfer the gender of the dictionary item
                        --  Need to transfer the CO of the ADJ dictionary item
                        if pdl_part.adj.co in pos..super  then
                           --  If the dictionary entry has a unique CO, use it
                           com := pdl_part.adj.co;
                        else
                           --  Otherwise, the entry is X, generate a CO from KEY
                           com := adj_comp_from_key(pdl_key);
                        end if;
                        m := m + 1;
                        sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                   ir => (
                                          qual => (
                                                   pofs => adj,
                                                   adj => (
                                                           pdl_part.adj.decl,
                                                           sl(i).ir.qual.adj.cs,
                                                           sl(i).ir.qual.adj.number,
                                                           sl(i).ir.qual.adj.gender,
                                                           com )  ),
                                          key => sl(i).ir.key,
                                          ending => sl(i).ir.ending,
                                          age => sl(i).ir.age,
                                          freq => sl(i).ir.freq),
                                   d_k => pdl(j).d_k,
                                   mnpc => mnpc_part);

                     elsif (pdl_part.pofs = num)                          and then
                       (pdl_part.num.decl <= sl(i).ir.qual.num.decl)     and then
                       (pdl_key         = sl(i).ir.key)                   then
                        --PUT(" HIT  NUM    ");
                        if pdl_part.num.sort = x  then
                           --  If the entry is X, generate a CO from KEY
                           num_sort:= num_sort_from_key(pdl_key);
                        else
                           --  Otherwise, the dictionary entry has a unique CO, use it
                           num_sort := pdl_part.num.sort;
                        end if;
                        m := m + 1;
                        sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                   ir => (
                                          qual => (
                                                   pofs => num,
                                                   num => (
                                                           pdl_part.num.decl,
                                                           sl(i).ir.qual.num.cs,
                                                           sl(i).ir.qual.num.number,
                                                           sl(i).ir.qual.num.gender,
                                                           num_sort)  ),
                                          key => sl(i).ir.key,
                                          ending => sl(i).ir.ending,
                                          age => sl(i).ir.age,
                                          freq => sl(i).ir.freq),
                                   d_k => pdl(j).d_k,
                                   mnpc => mnpc_part);

                     elsif (pdl_part.pofs = adv)                          and then
                       ((pdl_part.adv.co   <= sl(i).ir.qual.adv.co  ) or
                          ((sl(i).ir.qual.adv.co = x)  or (pdl_part.adv.co = x)))    then
                        --PUT(" HIT  ADV   ");
                        --  Need to transfer the CO of the ADV dictionary item
                        if pdl_part.adv.co in pos..super  then
                           --  If the dictionary entry has a unique CO, use it
                           com := pdl_part.adv.co;
                        else
                           --  The entry is X and we need to generate a COMP from the KEY
                           com := adv_comp_from_key(pdl_key);
                        end if;
                        m := m + 1;
                        sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                   ir => (
                                          qual => (
                                                   pofs => adv,
                                                   adv => (
                                                           co => com)  ),
                                          key => sl(i).ir.key,
                                          ending => sl(i).ir.ending,
                                          age => sl(i).ir.age,
                                          freq => sl(i).ir.freq),
                                   d_k => pdl(j).d_k,
                                   mnpc => mnpc_part);

                     elsif (pdl_part.pofs = v)                         then
                        --TEXT_IO.PUT_LINE("V found, now check CON");
                        if sl(i).ir.qual.pofs = v     and then
                          (pdl_part.v.con <= sl(i).ir.qual.v.con) then
                           --TEXT_IO.PUT(" HIT  V     ");
                           m := m + 1;
                           sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                      ir => (
                                             qual => (
                                                      pofs => v,
                                                      v => (
                                                            pdl_part.v.con,
                                                            sl(i).ir.qual.v.tense_voice_mood,
                                                            sl(i).ir.qual.v.person,
                                                            sl(i).ir.qual.v.number )  ),
                                             key => sl(i).ir.key,
                                             ending => sl(i).ir.ending,
                                             age => sl(i).ir.age,
                                             freq => sl(i).ir.freq),
                                      d_k => pdl(j).d_k,
                                      mnpc => mnpc_part);

                        elsif sl(i).ir.qual.pofs = vpar   and then
                          (pdl_part.v.con <= sl(i).ir.qual.vpar.con)   then
                           --PUT(" HIT  VPAR  ");
                           m := m + 1;
                           sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                      ir => (
                                             qual => (
                                                      pofs => vpar,
                                                      vpar => (
                                                               pdl_part.v.con,
                                                               sl(i).ir.qual.vpar.cs,
                                                               sl(i).ir.qual.vpar.number,
                                                               sl(i).ir.qual.vpar.gender,
                                                               sl(i).ir.qual.vpar.tense_voice_mood )  ),
                                             key => sl(i).ir.key,
                                             ending => sl(i).ir.ending,
                                             age => sl(i).ir.age,
                                             freq => sl(i).ir.freq),
                                      d_k => pdl(j).d_k,
                                      mnpc => mnpc_part);

                        elsif sl(i).ir.qual.pofs = supine   and then
                          (pdl_part.v.con <= sl(i).ir.qual.supine.con)   then
                           --PUT(" HIT  SUPINE");
                           m := m + 1;
                           sxx(m) := (stem => subtract_prefix(sl(i).stem, prefix),
                                      ir => (
                                             qual => (
                                                      pofs => supine,
                                                      supine => (
                                                                 pdl_part.v.con,
                                                                 sl(i).ir.qual.supine.cs,
                                                                 sl(i).ir.qual.supine.number,
                                                                 sl(i).ir.qual.supine.gender)  ),
                                             key => sl(i).ir.key,
                                             ending => sl(i).ir.ending,
                                             age => sl(i).ir.age,
                                             freq => sl(i).ir.freq),
                                      d_k => pdl(j).d_k,
                                      mnpc => mnpc_part);

                        end if;

                     elsif pdl_part.pofs = prep and then
                       pdl_part.prep.obj = sl(i).ir.qual.prep.obj           then
                        --PUT(" HIT  PREP  ");
                        m := m + 1;
                        sxx(m) := (subtract_prefix(sl(i).stem, prefix), sl(i).ir,
                                   pdl(j).d_k, mnpc_part);

                     elsif pdl_part.pofs = conj                              then
                        --PUT(" HIT  CONJ  ");
                        m := m + 1;
                        sxx(m) := (subtract_prefix(sl(i).stem, prefix), sl(i).ir,
                                   pdl(j).d_k, mnpc_part);

                     elsif pdl_part.pofs = interj                            then
                        --PUT(" HIT  INTERJ ");
                        m := m + 1;
                        sxx(m) := (subtract_prefix(sl(i).stem, prefix), sl(i).ir,
                                   pdl(j).d_k, mnpc_part);

                     end if;

                     --TEXT_IO.NEW_LINE; PUT(SL(I).IR.QUAL); TEXT_IO.PUT("  --  ");
                     --TEXT_IO.PUT(PDL(J).DS.STEM); PUT(PDL_PART); TEXT_IO.NEW_LINE;

                  end if;
               end if;
            end loop on_sl;

        <<end_of_pdl_loop>> null;
         end loop on_pdl;
      end reduce_stem_list;

      procedure apply_prefix(sa : in stem_array_type; suffix : in suffix_item;
                                                      sx : in sal; sxx : in out sal;
                                                                   pa : in out parse_array; pa_last : in out integer) is
         --  Worry about the stem changing re-cipio from capio
         --  Correspondence of parts, need EFF for VPAR
         --  The prefixes should be ordered with the longest/most likely first
         ssa : stem_array;
         l : integer :=  0;

      begin
         --PUT_LINE("Entering APPLY_PREFIX");
         sxx := (others => null_parse_record);    --  !!!!!!!!!!!!!!!!!!!!!!!

         if words_mdev(use_prefixes)  then

            --PUT(NUMBER_OF_PREFIXES); PUT(INTEGER(SA'LENGTH)); PUT(SA'LAST); NEW_LINE;
            for i in 1..number_of_prefixes  loop       --  Loop through PREFIXES
               l :=  0;
               for j in sa'range  loop                  --  Loop through stem array
                                                        --PUT("J = "); PUT(J); PUT("   SA(J) = "); PUT(SA(J)); NEW_LINE;
                  if (sa(j)(1) = prefixes(i).fix(1))  then  --  Cuts down a little -- do better
                     if subtract_prefix(sa(j), prefixes(i)) /=
                       head(sa(j), max_stem_size)  then
                        l := l + 1;            --  We have a hit, make new stem array item
                        ssa(l) := head(subtract_prefix(sa(j), prefixes(i)),
                                       max_stem_size);  --  And that has prefix subtracted to match dict
                                                        --PUT("L = "); PUT(L); PUT("   "); PUT_LINE(SUBTRACT_PREFIX(SA(J), PREFIXES(I)));
                     end if;                               --  with prefix subtracted stems
                  end if;
               end loop;

               if l > 0  then                        --  There has been a prefix hit
                  search_dictionaries(ssa(1..l));      --  So run new dictionary search

                  if  pdl_index /= 0     then                  --  Dict search was successful
                                                               --PUT_LINE("IN APPLY_PREFIX -  PDL_INDEX not 0     after prefix  " & PREFIXES(I).FIX);

                     --PUT_LINE("REDUCE_STEM_LIST being called from APPLY_PREFIX  ----  SUFFIX = "
                     --& SUFFIX.FIX);
                     reduce_stem_list(sx, sxx, prefixes(i), suffix);

                     if sxx(1) /= null_parse_record  then   --  There is reduced stem result
                        pa_last := pa_last + 1;        --  So add prefix line to parse array
                        pa(pa_last).ir :=
                          ((prefix, null_prefix_record), 0, null_ending_record, x, x);
                        pa(pa_last).stem := head(prefixes(i).fix, max_stem_size);
                        pa(pa_last).mnpc := dict_io.count(prefixes(i).mnpc);
                        pa(pa_last).d_k  := addons;
                        exit;      --  Because we accept only one prefix
                     end if;

                  end if;
               end if;
            end loop;      --  Loop on I for PREFIXES
         end if;  --  On USE_PREFIXES
      end apply_prefix;

      procedure apply_suffix(sa : in stem_array_type;
                             sx : in sal; sxx : in out sal;
                                          pa : in out parse_array; pa_last : in out integer) is
         ssa : stem_array;
         l : integer :=  0;
         suffix_hit : integer := 0;
         --            use TEXT_IO;
         --            use INFLECTIONS_PACKAGE.INTEGER_IO;

      begin
         for i in 1..number_of_suffixes  loop       --  Loop through SUFFIXES
            l :=  0;                                 --  Take as many as fit

            for j in sa'range  loop                  --  Loop through stem array
               if subtract_suffix(sa(j), suffixes(i)) /=
                 head(sa(j), max_stem_size)  then
                  l := l + 1;            --  We have a hit, make new stem array item
                  ssa(l) := head(subtract_suffix(sa(j), suffixes(i)),
                                 max_stem_size);  --  And that has prefix subtracted to match dict
               end if;
            end loop;    --  Loop on J through SA

            if l > 0  then                        --  There has been a suffix hit
               search_dictionaries(ssa(1..l));     --  So run new dictionary search
                                                                       --  For suffixes we allow as many as match

               if  pdl_index /= 0     then                  --  Dict search was successful
                                                            --PUT_LINE("IN APPLY_SUFFIX -  PDL_INDEX not 0     after suffix  " & SUFFIXES(I).FIX);

                  suffix_hit := i;

                  reduce_stem_list(sx, sxx, null_prefix_item, suffixes(i));

                  if sxx(1) /= null_parse_record  then    --  There is reduced stem result
                     pa_last := pa_last + 1;        --  So add suffix line to parse array
                                                    --PUT_LINE("REDUCE_STEM_LIST is not null so add suffix to parse array");
                     pa(pa_last).ir :=
                       ((suffix, null_suffix_record), 0, null_ending_record, x, x);
                     pa(pa_last).stem := head(
                                              suffixes(suffix_hit).fix, max_stem_size);
                     --  Maybe it would better if suffix.fix was of stem size
                     pa(pa_last).mnpc := dict_io.count(suffixes(suffix_hit).mnpc);
                     --PUT("SUFFIX MNPC  "); PUT(SUFFIXES(SUFFIX_HIT).MNPC); NEW_LINE;
                     pa(pa_last).d_k  := addons;
                     ---
                     for i in sxx'range  loop
                        exit when sxx(i) = null_parse_record;
                        pa_last := pa_last + 1;
                        pa(pa_last) := sxx(i);
                     end loop;
                     ---
                  end if;

               else   --  there is suffix (L /= 0) but no dictionary hit
                  suffix_hit := i;
                  apply_prefix(ssa(1..l), suffixes(i), sx, sxx, pa, pa_last);
                  if sxx(1) /= null_parse_record  then    --  There is reduced stem result
                     pa_last := pa_last + 1;        --  So add suffix line to parse array
                                                    --PUT_LINE("REDUCE_STEM_LIST is not null so add suffix to parse array");
                     pa(pa_last).ir :=
                       ((suffix, null_suffix_record), 0, null_ending_record, x, x);
                     pa(pa_last).stem := head(
                                              suffixes(suffix_hit).fix, max_stem_size);
                     pa(pa_last).mnpc := dict_io.count(suffixes(suffix_hit).mnpc);
                     pa(pa_last).d_k  := addons;

                     for i in sxx'range  loop    --  Set this set of results
                        exit when sxx(i) = null_parse_record;
                        pa_last := pa_last + 1;
                        pa(pa_last) := sxx(i);
                     end loop;

                  end if;

               end if;
            end if;                               --  with suffix subtracted stems
         end loop;      --  Loop on I for SUFFIXES

      end apply_suffix;

      procedure prune_stems(input_word : string; sx : in sal; sxx : in out sal) is
         j : integer := 0;
         --SXX : SAL;

      begin
         if sx(1) = null_parse_record  then
            return; end if;

            -----------------------------------------------------------------

        generate_reduced_stem_array:
            begin
               j := 1;
               for z in 0..min(max_stem_size, len(input_word))  loop
                  if sa(z) /= not_a_stem  then
                     --PUT(Z); PUT(J); PUT("  "); PUT_LINE(SA(Z));
                     ssa(j) := sa(z);
                     ssa_max := j;
                     j := j + 1;
                  end if;
               end loop;
            end generate_reduced_stem_array;

            if not words_mdev(do_only_fixes)  then   --  Just bypass main dictionary search

               search_dictionaries(ssa(1..ssa_max));

            end if;

            if (((pa_last = 0)  and            --  No Uniques or Syncope
                   (pdl_index = 0))  --)   and then    --  No dictionary match
                or words_mdev(do_fixes_anyway))  and then
              words_mode(do_fixes)  then

               ----So try prefixes and suffixes, Generate a new SAA array, search again

               if sxx(1) = null_parse_record  then        --  We could not find a match with suffix
                  apply_prefix(ssa(1..ssa_max), null_suffix_item, sx, sxx, pa, pa_last);
               end if;
               --------------
               if sxx(1) = null_parse_record  then        --  We could not find a match with suffix
                  apply_suffix(ssa(1..ssa_max), sx, sxx, pa, pa_last);
                  if sxx(1) = null_parse_record  then        --  We could not find a match with suffix
                                                             ----So try prefixes, Generate a new SAA array, search again
                                                             ----Need to use the new SSA, modified to include suffixes
                     apply_prefix(ssa(1..ssa_max), null_suffix_item, sx, sxx, pa, pa_last);
                     --TEXT_IO.PUT_LINE("PREFIXES applied  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     --------------
                  end if;       --  Suffix failed
               end if;       --  Suffix failed
            else
               reduce_stem_list(sx, sxx, null_prefix_item, null_suffix_item);
               if pa_last = 0  and then  sxx(1) = null_parse_record  then
                  --------------
                  if words_mode(do_fixes)  then
                     apply_suffix(ssa(1..ssa_max), sx, sxx, pa, pa_last);
                     if sxx(1) = null_parse_record  then        --  We could not find a match with suffix
                                                                ----So try prefixes, Generate a new SAA array, search again
                                                                ----Need to use the new SSA, modified to include suffixes
                        apply_prefix(ssa(1..ssa_max), null_suffix_item,
                                     sx, sxx, pa, pa_last);
                     end if;   --  Suffix failed
                  end if;     --  If DO_FIXES then do
               end if;       --  First search passed but SXX null
            end if;         --  First search failed

      end prune_stems;

      procedure process_packons(input_word : string) is

         stem_length  : integer := 0;
         pr   : parse_record;
         m : integer := 1;
         de : dictionary_entry;
         mean : meaning_type;
         packon_first_hit : boolean := false;
         sl : sal := (others => null_parse_record);
         sl_nulls : constant sal := (others => null_parse_record);

      begin

     over_packons:
         for k in packons'range  loop    -- Do whole set, more than one may apply
                                         --TEXT_IO.PUT_LINE("OVER_PACKONS   K = "& INTEGER'IMAGE(K) & "   PACKON = " & PACKONS(K).TACK);
                                         --  PACKON if the TACKON ENTRY is PRON
        for_each_packon:
            declare
               xword : constant string := subtract_tackon(input_word, packons(k));
               word : string(1..xword'length) := xword;
               packon_length : constant integer := trim(packons(k).tack)'length;
               last_of_word : character := word(word'last);
               length_of_word   : constant integer := word'length;
            begin
               sl := sl_nulls;      --  Initialize SL to nulls
               if word  /= input_word  then
                  packon_first_hit := true;

                  if packons(k).tack(1..3) = "dam" and  last_of_word = 'n'  then
                     word(word'last) := 'm';   --  Takes care of the m - > n shift with dam
                     last_of_word := 'm';
                     --PUT_LINE("PACKON = dam   and LAST_OF_WORD = n    => " & WORD);
                  end if;

                  --  No blank endings in these pronouns
                  lel_section_io.read(inflections_sections_file, lel, 4);

                  m := 0;
              on_inflects:
                  for z in reverse 1..min(6, length_of_word)  loop   --  optimum for qu-pronouns
                                                                     --PUT("ON_INFLECTS  Z = "); PUT(Z); PUT("  "); PUT(WORD(1..Z)); NEW_LINE;
                     if pell(z, last_of_word) > 0  then   --  Any possible inflections at all
                        for i in pelf(z, last_of_word)..pell(z, last_of_word) loop

                           if (z <= length_of_word)  and then
                             ((equ(lel(i).ending.suf(1..z),
                                   word(word'last-z+1..word'last)))  and
                                (lel(i).qual.pron.decl <= packons(k).entr.base.pack.decl))  then
                              --  Have found an ending that is a possible match
                              --  And INFLECT agrees with PACKON.BASE
                              --PUT_LINE("INFLECTS HIT ------------------------------------------------------");

                              --  Add to list of possible ending records
                              stem_length := word'length - z;
                              pr := (head(word(word'first..stem_length), max_stem_size),
                                     lel(i), default_dictionary_kind, null_mnpc);
                              m := m + 1;
                              sl(m) := pr;
                              ssa(1) := head(word(word'first.. word'first+stem_length-1),
                                             max_stem_size);
                              --PUT_LINE("STEM_LENGTH = " & INTEGER'IMAGE(STEM_LENGTH));
                              --PUT_LINE("SSA(1) in PACKONS from real  INFLECTS ->" & SSA(1) & '|');
                              --  may get set several times
                           end if;
                        end loop;
                     end if;
                  end loop on_inflects;

                  --  Only one stem will emerge
                  pdl_index := 0;
                  search_dictionaries(ssa(1..1),
                                      pack_only);
                  --  Now have a PDL, scan for agreement

              pdl_loop:
                  for j in 1..pdl_index  loop  --  Go through all dictionary hits to see
                                               --PUT_LINE("PACKON  PDL_INDEX  "); PUT(PDL(J).DS.STEM); PUT(PDL(J).DS.PART); NEW_LINE;
                                               --  M used here wher I is used in REDUCE, maybe make consistent
                     m := 1;
                 sl_loop:
                     while sl(m) /= null_parse_record  loop  --  Over all inflection hits
                                                             --  if this stem is possible
                                                             --  call up the meaning to check for "(w/-"
                        dict_io.set_index(dict_file(pdl(j).d_k), pdl(j).ds.mnpc);
                        dict_io.read(dict_file(pdl(j).d_k), de);
                        mean := de.mean;
                        
                        -- there is no way this condition can be true;
                        -- packon_length - 1 /= packon_length
                        if (trim(mean)(1..4) = "(w/-" and then  --  Does attached PACKON agree
                            trim(mean)(5..4+packon_length) = trim(packons(k).tack))   then
                           
                           if (pdl(j).ds.part.pack.decl = sl(m).ir.qual.pron.decl)   then  --  or
                              if packon_first_hit then
                                 pa_last := pa_last + 1;
                                 pa(pa_last) := (packons(k).tack,
                                                 ((tackon, null_tackon_record), 0,
                                                  null_ending_record, x, x),
                                                 addons,
                                                 dict_io.count((packons(k).mnpc)));
                                 packon_first_hit := false;

                              end if;
                              pa_last := pa_last + 1;
                              pa(pa_last) := (stem => sl(m).stem,
                                              ir => (
                                                     qual => (
                                                              pofs => pron,
                                                              pron => (
                                                                       pdl(j).ds.part.pack.decl,
                                                                       sl(m).ir.qual.pron.cs,
                                                                       sl(m).ir.qual.pron.number,
                                                                       sl(m).ir.qual.pron.gender )),
                                                     key => sl(m).ir.key,
                                                     ending => sl(m).ir.ending,
                                                     age => sl(m).ir.age,
                                                     freq => sl(m).ir.freq),
                                              d_k => pdl(j).d_k,
                                              mnpc => pdl(j).ds.mnpc);
                              --end if;
                           end if;
                        end if;
                        m := m + 1;

                     end loop sl_loop;

                  end loop pdl_loop;

               end if;
            end for_each_packon;
            packon_first_hit := false;

         end loop over_packons;

      end process_packons;

      procedure process_qu_pronouns(input_word : string; qkey : stem_key_type := 0) is

         word : constant string := lower_case(trim(input_word));
         last_of_word : constant character := word(word'last);
         length_of_word   : constant integer := word'length;
         stem_length  : integer := 0;
         m : integer := 0;
         pr   : parse_record;
         sl : sal := (others => null_parse_record);

      begin
         --TEXT_IO.PUT_LINE("PROCESS_QU_PRONOUNS   " & INPUT_WORD);

         --  No blank endings in these pronouns
         lel_section_io.read(inflections_sections_file, lel, 4);

         --  M used here while I is used in REDUCE, maybe make consistent
         m := 0;
     on_inflects:
         for z in reverse 1..min(4, length_of_word)  loop     --  optimized for qu-pronouns
                                                              --PUT("ON_INFLECTS  "); PUT(Z); PUT("  "); PUT(LAST_OF_WORD); NEW_LINE;
            if pell(z, last_of_word) > 0  then   --  Any possible inflections at all
               for i in pelf(z, last_of_word)..pell(z, last_of_word) loop
                  --PUT(LEL(I)); PUT(WORD'LAST); PUT(WORD'LAST-Z+1); NEW_LINE;
                  if (z <= length_of_word)  and then
                    lel(i).key = qkey  and then
                    equ(lel(i).ending.suf(1..z),
                        word(word'last-z+1..word'last))    then
                     --  Have found an ending that is a possible match
                     --  Add to list of possible ending records
                     stem_length := word'length - z;
                     pr := (head(word(word'first..stem_length), max_stem_size),
                            lel(i), default_dictionary_kind, null_mnpc);
                     m := m + 1;
                     sl(m) := pr;
                     ssa(1) := head(word(word'first.. word'first+stem_length-1),
                                    max_stem_size);
                     --  may get set several times
                  end if;
               end loop;
            end if;
         end loop on_inflects;

         --  Only one stem will emerge
         pdl_index := 0;
         search_dictionaries(ssa(1..1),
                             qu_pron_only);
         --  Now have a PDL, scan for agreement

     pdl_loop:
         for j in 1..pdl_index  loop  --  Go through all dictionary hits to see
            m := 1;
        sl_loop:
            while sl(m) /= null_parse_record  loop  --  Over all inflection hits
               if (pdl(j).ds.part.pron.decl = sl(m).ir.qual.pron.decl)   then
                  pa_last := pa_last + 1;
                  pa(pa_last) := (stem => sl(m).stem,
                                  ir => (
                                         qual => (
                                                  pofs => pron,
                                                  pron => (
                                                           pdl(j).ds.part.pron.decl,
                                                           sl(m).ir.qual.pron.cs,
                                                           sl(m).ir.qual.pron.number,
                                                           sl(m).ir.qual.pron.gender )),
                                         key => sl(m).ir.key,
                                         ending => sl(m).ir.ending,
                                         age => sl(m).ir.age,
                                         freq => sl(m).ir.freq),
                                  d_k => pdl(j).d_k,
                                  mnpc => pdl(j).ds.mnpc);
               end if;
               m := m + 1;

            end loop sl_loop;
            -- PDL:= PDL.SUCC;
         end loop pdl_loop;

      end process_qu_pronouns;

      procedure try_tackons(input_word : string) is
         tackon_hit : boolean := false;
         tackon_on  : boolean := false;
         j : integer := 0;
         de : dictionary_entry := null_dictionary_entry;
         mean : meaning_type := null_meaning_type;
         entering_pa_last : constant integer := pa_last;
         start_of_loop : constant integer := 5;    --  4 enclitics     --  Hard number  !!!!!!!!!!!!!!!
         end_of_loop : constant integer := number_of_tackons;
      begin

     loop_over_tackons:
         for i in start_of_loop..end_of_loop  loop

        remove_a_tackon:
            declare
               less : constant string :=
                 subtract_tackon(input_word, tackons(i));
            begin
               --TEXT_IO.PUT_LINE("LESS = " & LESS);
               if less  /= input_word  then       --  LESS is less

                  word(less, pa, pa_last);

                  if pa_last > entering_pa_last  then      --  we have a possible word
                     if tackons(i).entr.base.pofs = x  then 
                        tackon_hit := true;
                        tackon_on  := false;

                     else

                        j := pa_last;

                        while j >= entering_pa_last+1  loop 
                           --  Sweep backwards over PA
                           --  Sweeping up inapplicable fixes,
                           --  although we only have TACKONs for X or PRON or ADJ - so far
                           --  and there are no fixes for PRON - so far

                           if ((pa(j).ir.qual.pofs = prefix) and then (tackon_on))  then
                              null;          --  check PART
                              tackon_on  := false;
                           elsif ((pa(j).ir.qual.pofs = suffix) and then (tackon_on))  then
                              --  check PART

                              null;
                              tackon_on  := false;

                           elsif pa(j).ir.qual.pofs = tackons(i).entr.base.pofs  then
                              dict_io.set_index(dict_file(pa(j).d_k), pa(j).mnpc);
                              dict_io.read(dict_file(pa(j).d_k), de);
                              mean := de.mean;

                              --  check PART
                              case tackons(i).entr.base.pofs is
                                 when n       =>
                                    if (pa(j).ir.qual.n.decl <=
                                          tackons(i).entr.base.n.decl)  then
                                       --  Ignore GEN and KIND
                                       tackon_hit := true;
                                       tackon_on  := true;
                                    end if;

                                 when pron    =>              --  Only one we have other than X
                                    if pa(j).ir.qual.pron.decl <=
                                      tackons(i).entr.base.pron.decl  --and then
                                    then
                                       tackon_hit := true;
                                       tackon_on  := true;
                                    else
                                       pa(j..pa_last-1) := pa(j+1..pa_last);
                                       pa_last := pa_last - 1;

                                    end if;

                                 when adj     =>
                                    --  Forego all checks, even on DECL of ADJ
                                    --  -cumque is the only one I have now
                                    --  if  .......
                                    tackon_hit := true;
                                    tackon_on  := true;
                                    --  else
                                    --    PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
                                    --    PA_LAST := PA_LAST - 1;
                                    --  end if;

                                    --when ADV     =>
                                    --when V       =>
                                 when others  =>
                                    pa(j..pa_last-1) := pa(j+1..pa_last);
                                    pa_last := pa_last - 1;

                              end case;

                           else                                          --  check PART
                              pa(j..pa_last-1) := pa(j+1..pa_last);
                              pa_last := pa_last - 1;
                              --PUT("J failed  J & PA_LAST = "); PUT(J); PUT("  "); PUT(PA_LAST); NEW_LINE;
                           end if;                                      --  check PART
                           j := j - 1;
                        end loop;                          --  loop sweep over PA

                     end if;                                      --  on PART (= X?)
                                                                  --PUT_LINE("End if on PART = X ?");

                     -----------------------------------------
                     if tackon_hit  then

                        pa_last := pa_last + 1;
                        pa(entering_pa_last+2..pa_last) :=
                          pa(entering_pa_last+1..pa_last-1);
                        pa(entering_pa_last+1) := (tackons(i).tack,
                                                   ((tackon, null_tackon_record), 0,
                                                    null_ending_record, x, x),
                                                   addons,
                                                   dict_io.count((tackons(i).mnpc)));
                        return;                 --  Be happy with one ???????
                     else
                        null;

                     end if;   --  TACKON_HIT

                  end if;                             --  we have a possible word
                                                      -----------------------------------------
               end if;                                     --  LESS is less
            end remove_a_tackon;
         end loop loop_over_tackons;
         --PUT_LINE("LEAVING TACKONS   *******************************************  ");
      end try_tackons;

   begin                           --  WORD
                                   --TEXT_IO.PUT_LINE("Starting WORD  INPUT = " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
      if trim(input_word) = ""  then
         return;
      end if;

      run_uniques(input_word, unique_found, pa, pa_last);

  qu:
      declare
         pa_qstart : constant integer := pa_last;
         pa_start : constant integer := pa_last;
         saved_mode_array : constant mode_array := words_mode;
         qkey : stem_key_type := 0;

      begin       --  QU
         tickons(number_of_tickons+1) := null_prefix_item;
         words_mode  := (others => false);

         for i in 1..number_of_tickons+1  loop
            declare
               q_word : constant string :=  trim(subtract_tickon(input_word, tickons(i)));
            begin
               pa_last := pa_qstart;
               pa(pa_last+1) := null_parse_record;
               if (i = number_of_tickons + 1)   or else  --  The prefix is a TICKON
                 (q_word /= input_word) then            --  and it matches the start of INPUT_WORD

                  if i <= number_of_tickons  then        --  Add to PA if
                                                         --TEXT_IO.PUT_LINE("ADDING TICKON PA    " & TICKONS(I).FIX);
                     pa_last := pa_last + 1;        --  So add prefix line to parse array
                     pa(pa_last).stem := head(tickons(i).fix, max_stem_size);
                     pa(pa_last).ir := ((prefix, null_prefix_record), 0, null_ending_record, x, x);
                     pa(pa_last).d_k  := addons;
                     pa(pa_last).mnpc := dict_io.count(tickons(i).mnpc);
                  end if;

                  if q_word'length >= 3   and then   --  qui is shortest QU_PRON
                    ((q_word(q_word'first..q_word'first+1) = "qu")  or
                       (q_word(q_word'first..q_word'first+1) = "cu"))  then
                     if q_word(q_word'first..q_word'first+1) = "qu"  then
                        qkey := 1;
                        process_qu_pronouns(q_word, qkey);
                     elsif q_word(q_word'first..q_word'first+1) = "cu"  then
                        qkey := 2;
                        process_qu_pronouns(q_word, qkey);
                     end if;
                     if pa_last <= pa_qstart + 1  and then
                       qkey > 0                    then    --  If did not find a PACKON
                        if q_word(q_word'first..q_word'first+1) = "qu"  then
                           process_packons(q_word);
                        elsif q_word(q_word'first..q_word'first+1) = "cu"  then
                           process_packons(q_word);
                        end if;
                     else
                        exit;
                     end if;
                     if pa_last > pa_qstart + 1  then
                        exit;
                     end if;

                  elsif input_word'length >= 6  then   --  aliqui as aliQU_PRON
                     if input_word(input_word'first..input_word'first+4) = "aliqu"  then
                        process_qu_pronouns(input_word, 1);
                     elsif input_word(input_word'first..input_word'first+4) = "alicu"  then
                        process_qu_pronouns(input_word, 2);

                     end if;

                  end if;

                  if pa_last = pa_start + 1  then    --  Nothing found
                     pa_last := pa_start;             --  Reset PA_LAST
                  else
                     exit;
                  end if;

               end if;
            end;
         end loop;

         words_mode := saved_mode_array;

      exception
         when others =>
            words_mode := saved_mode_array;
      end qu;

      --==========================================================
      run_inflections(input_word, ss);
      prune_stems(input_word, ss, sss);
      if sss(1) /= null_parse_record   then
         order_stems(sss);
         array_stems(sss, pa, pa_last);
         sss(1) := null_parse_record;
      end if;
      --==========================================================

      if pa_last = pa_save  then
         try_tackons(input_word);
      end if;

   exception
      when storage_error =>
         text_io.put_line(text_io.standard_output,
                          "STORAGE_ERROR exception in WORD while processing =>"
                            & raw_word);
         pa_last := pa_save;
         if words_mode(write_unknowns_to_file)  then
            text_io.put(unknowns, raw_word);
            text_io.set_col(unknowns, 21);
            text_io.put_line(unknowns, "========   STORAGE_ERROR  ");
         end if;
      when others =>
         if words_mode(write_unknowns_to_file)  then
            text_io.put(unknowns, raw_word);
            text_io.set_col(unknowns, 21);
            text_io.put_line(unknowns, "========   ERROR  ");
         end if;
         pa_last := pa_save;
   end word;

   procedure initialize_word_package is
   begin                                  --  Initializing WORD_PACKAGE

      establish_inflections_section;

      lel_section_io.open(inflections_sections_file, lel_section_io.in_file,
                          inflections_sections_name);

      try_to_load_dictionary(general);

      try_to_load_dictionary(special);

  load_local:
      begin
         --  First check if there is a LOC dictionary
     check_for_local_dictionary:
         declare
            dummy : text_io.file_type;
         begin
            text_io.open(dummy, text_io.in_file,
                         add_file_name_extension(dictionary_file_name,
                                                 "LOCAL"));
            --  Failure to OPEN will raise an exception, to be handled below
            text_io.close(dummy);
         end check_for_local_dictionary;
         --  If the above does not exception out, we can load LOC
         preface.put("LOCAL ");
         dict_loc := null_dictionary;
         load_dictionary(dict_loc,
                         add_file_name_extension(dictionary_file_name, "LOCAL"));
         --  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
         load_stem_file(local);
         dictionary_available(local) := true;
      exception
         when others  =>
            dictionary_available(local) := false;
      end load_local;

      load_uniques(unq, uniques_full_name);

      load_addons(addons_full_name);

      load_bdl_from_disk;

      if not (dictionary_available(general)  or
                dictionary_available(special)  or
                dictionary_available(local))  then
         preface.put_line("There are no main dictionaries - program will not do much");
         preface.put_line("Check that there are dictionary files in this subdirectory");
         preface.put_line("Except DICT.LOC that means DICTFILE, INDXFILE, STEMFILE");
      end if;

  try_to_load_english_words:
      begin
         english_dictionary_available(general) := false;
         ewds_direct_io.open(ewds_file, ewds_direct_io.in_file, "EWDSFILE.GEN");

         english_dictionary_available(general) := true;

      exception
         when others  =>
            preface.put_line("No English available");
            english_dictionary_available(general) := false;
      end try_to_load_english_words;

   end initialize_word_package;

end word_package;
