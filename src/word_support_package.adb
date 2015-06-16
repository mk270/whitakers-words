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

with latin_file_names; use latin_file_names;
with strings_package; use strings_package;
with config;
with preface;
package body word_support_package is

   function len(s : string) return integer is
   begin
      return trim(s)'length;
   end len;

   function eff_part(part : part_of_speech_type) return part_of_speech_type is
   begin
      if part = vpar   then
         return v;
      elsif part = supine  then
         return v;
      else
         return part;
      end if;
   end eff_part;

   function adj_comp_from_key(key : stem_key_type) return comparison_type is
   begin
      case key is
         when 0 | 1 | 2  => return pos;
         when 3          => return comp;
         when 4          => return super;
         when others     => return x;
      end case;
   end adj_comp_from_key;

   function adv_comp_from_key(key : stem_key_type) return comparison_type is
   begin
      case key is
         when 1  => return pos;
         when 2  => return comp;
         when 3  => return super;
         when others  => return x;
      end case;
   end adv_comp_from_key;

   function num_sort_from_key(key : stem_key_type) return numeral_sort_type is
   begin
      case key is
         when 1  => return card;
         when 2  => return ord;
         when 3  => return dist;
         when 4  => return adverb;
         when others  => return x;
      end case;
   end num_sort_from_key;

   function first_index(input_word : string;
                        d_k : dictionary_file_kind := default_dictionary_file_kind)
                       return stem_io.count is
      wd : constant string := trim(input_word);  --  string may not start at 1
   begin
      if d_k = local  then
         return ddlf(wd(wd'first), 'a', d_k);
      elsif wd'length < 2 then
         return   0; --  BDLF(WD(WD'FIRST), ' ', D_K);
      else
         return ddlf(wd(wd'first), wd(wd'first+1), d_k);
      end if;
   end first_index;

   function  last_index(input_word : string;
                        d_k : dictionary_file_kind := default_dictionary_file_kind)
                       return stem_io.count is
      wd : constant string := trim(input_word);
   begin        --  remember the string may not start at 1
      if d_k = local  then
         return ddll(wd(wd'first), 'a', d_k);
      elsif wd'length < 2 then
         return   0; --  BDLL(WD(WD'FIRST), ' ', D_K);
      else
         return ddll(wd(wd'first), wd(wd'first+1), d_k);
      end if;
   end  last_index;

   procedure load_bdl_from_disk is
      use stem_io;
      ds : dictionary_stem;
      index_first,
      index_last  : stem_io.count := 0;
      k : integer := 0;
   begin

      if dictionary_available(general)  then
         --  The blanks are on the GENERAL dictionary
     loading_bdl_from_disk:
         declare
            d_k : constant dictionary_kind := general;
         begin
            if not is_open(stem_file(d_k))  then
               open(stem_file(d_k), stem_io.in_file,
                    add_file_name_extension(stem_file_name,
                                            dictionary_kind'image(d_k)));
            end if;
            index_first := bblf(' ', ' ', d_k);
            index_last  := bbll(' ', ' ', d_k);

            set_index(stem_file(d_k), stem_io.positive_count(index_first));
            for j in index_first..index_last  loop
               read(stem_file(d_k), ds);
               k := k + 1;
               bdl(k) := ds;
            end loop;
            close(stem_file(d_k));
         exception
            when name_error =>
               text_io.put_line("LOADING BDL FROM DISK had NAME_ERROR on " &
                                  add_file_name_extension(stem_file_name,
                                                          dictionary_kind'image(d_k)));
               text_io.put_line("The will be no blank stems loaded");
            when use_error =>
               text_io.put_line("LOADING BDL FROM DISK had USE_ERROR on " &
                                  add_file_name_extension(stem_file_name,
                                                          dictionary_kind'image(d_k)));
               text_io.put_line("There will be no blank stems loaded");
         end loading_bdl_from_disk;
      end if;

      --  Now load the stems of just one letter
      for d_k in general..dictionary_kind'last loop
         if dictionary_available(d_k)  then
            exit when d_k = local;
            --TEXT_IO.PUT_LINE("OPENING BDL STEMFILE " & EXT(D_K));
            if not is_open(stem_file(d_k))  then
               --PUT_LINE("LOADING_BDL is going to OPEN " &
               --ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
               --DICTIONARY_KIND'IMAGE(D_K)));
               open(stem_file(d_k), stem_io.in_file,
                    add_file_name_extension(stem_file_name,
                                            dictionary_kind'image(d_k)));
               --STEMFILE." & EXT(D_K));
               --PUT_LINE("OPENing was successful");
            end if;
            for i in character range 'a'..'z'  loop
               index_first := bdlf(i, ' ', d_k);
               index_last  := bdll(i, ' ', d_k);
               if index_first > 0  then
                  set_index(stem_file(d_k), stem_io.positive_count(index_first));
                  for j in index_first..index_last  loop
                     read(stem_file(d_k), ds);
                     k := k + 1;
                     bdl(k) := ds;
                  end loop;
               end if;
            end loop;
            --TEXT_IO.PUT_LINE("Single letters LOADED FROM DISK   K = " & INTEGER'IMAGE(K));
            close(stem_file(d_k));
         end if;

      end loop;
      bdl_last := k;

      --TEXT_IO.PUT("FINISHED LOADING BDL FROM DISK     BDL_LAST = ");
      --TEXT_IO.PUT(INTEGER'IMAGE(BDL_LAST));
      --TEXT_IO.NEW_LINE;

   end load_bdl_from_disk;

   procedure load_indices_from_indx_file(d_k : dictionary_kind) is
      use text_io;
      use inflections_package.integer_io;
      use stem_io;
      use count_io;
      ch : string(1..2);
      m, n : stem_io.count;
      number_of_blank_stems,
      number_of_non_blank_stems : stem_io.count := 0;
      s : string(1..100) := (others => ' ');
      last, l : integer := 0;

      function max(a, b : stem_io.count) return stem_io.count is
      begin
         if a >= b then  return a; end if; return b;
      end max;

   begin
      open(indx_file(d_k), text_io.in_file,
           add_file_name_extension(indx_file_name,
                                   dictionary_kind'image(d_k)));
      --"INDXFILE." & EXT(D_K)); --  $$$$$$$$$$$$

      preface.put(dictionary_kind'image(d_k));
      preface.put(" Dictionary loading");

      if d_k = general  then
         get_line(indx_file(d_k), s, last);
         ch := s(1..2);
         get(s(4..last), m, l);
         bblf(ch(1), ch(2), d_k) := m;
         get(s(l+1..last), n, l);
         bbll(ch(1), ch(2), d_k) := n;
         number_of_blank_stems := max(number_of_blank_stems, n);
      end if;

      while not end_of_file(indx_file(d_k))  loop
         get_line(indx_file(d_k), s, last);
         exit when last = 0;
         ch := s(1..2);
         get(s(4..last), m, l);
         if ch(2) = ' '  then
            bdlf(ch(1), ch(2), d_k) := m;
         else
            ddlf(ch(1), ch(2), d_k) := m;
         end if;
         get(s(l+1..last), n, l);
         if ch(2) = ' '  then
            bdll(ch(1), ch(2), d_k) := n;
            number_of_blank_stems := max(number_of_blank_stems, n);
         else
            ddll(ch(1), ch(2), d_k) := n;
            number_of_non_blank_stems := max(number_of_non_blank_stems, n);
         end if;
      end loop;
      close(indx_file(d_k));
      preface.set_col(33); preface.put("--  ");
      if not config.suppress_preface  then
         put(number_of_non_blank_stems, 6);
      end if;   --  Kludge for when TEXT_IO.COUNT too small
      preface.put(" stems");
      preface.set_col(55); preface.put_line("--  Loaded correctly");
   end load_indices_from_indx_file;

end word_support_package;
