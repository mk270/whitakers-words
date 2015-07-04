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

with Latin_File_Names; use Latin_File_Names;
with Strings_Package; use Strings_Package;
with config;
with preface;
package body word_support_package is

   function len(s : String) return Integer is
   begin
      return Trim (s)'Length;
   end len;

   function eff_part(part : Part_Of_Speech_Type) return Part_Of_Speech_Type is
   begin
      if part = vpar   then
         return v;
      elsif part = supine  then
         return v;
      else
         return part;
      end if;
   end eff_part;

   function adj_comp_from_key(key : Stem_Key_Type) return Comparison_Type is
   begin
      case key is
         when 0 | 1 | 2  => return pos;
         when 3          => return comp;
         when 4          => return super;
         when others     => return x;
      end case;
   end adj_comp_from_key;

   function adv_comp_from_key(key : Stem_Key_Type) return Comparison_Type is
   begin
      case key is
         when 1  => return pos;
         when 2  => return comp;
         when 3  => return super;
         when others  => return x;
      end case;
   end adv_comp_from_key;

   function num_sort_from_key(key : Stem_Key_Type) return Numeral_Sort_Type is
   begin
      case key is
         when 1  => return card;
         when 2  => return ord;
         when 3  => return dist;
         when 4  => return adverb;
         when others  => return x;
      end case;
   end num_sort_from_key;

   function first_index(Input_word : String;
                        d_k : dictionary_file_kind := default_dictionary_file_kind)
                       return stem_io.Count is
      wd : constant String := Trim (Input_word);  --  String may not start at 1
   begin
      if d_k = local  then
         return ddlf(wd(wd'First), 'a', d_k);
      elsif wd'Length < 2 then
         return   0; --  BDLF(WD(WD'FIRST), ' ', D_K);
      else
         return ddlf(wd(wd'First), wd(wd'First+1), d_k);
      end if;
   end first_index;

   function  last_index(Input_word : String;
                        d_k : dictionary_file_kind := default_dictionary_file_kind)
                       return stem_io.Count is
      wd : constant String := Trim (Input_word);
   begin        --  remember the String may not start at 1
      if d_k = local  then
         return ddll(wd(wd'First), 'a', d_k);
      elsif wd'Length < 2 then
         return   0; --  BDLL(WD(WD'FIRST), ' ', D_K);
      else
         return ddll(wd(wd'First), wd(wd'First+1), d_k);
      end if;
   end  last_index;

   procedure load_bdl_from_disk is
      use stem_io;
      ds : dictionary_stem;
      index_first,
      index_last  : stem_io.Count := 0;
      k : Integer := 0;
   begin

      if dictionary_available(general)  then
         --  The blanks are on the GENERAL dictionary
         loading_bdl_from_disk:
         declare
            d_k : constant Dictionary_Kind := general;
         begin
            if not Is_Open(stem_file(d_k))  then
               Open(stem_file(d_k), stem_io.In_File,
                    add_file_name_extension(stem_file_name,
                                            Dictionary_Kind'Image(d_k)));
            end if;
            index_first := bblf(' ', ' ', d_k);
            index_last  := bbll(' ', ' ', d_k);

            Set_Index(stem_file(d_k), stem_io.Positive_Count(index_first));
            for j in index_first..index_last  loop
               Read(stem_file(d_k), ds);
               k := k + 1;
               bdl(k) := ds;
            end loop;
            Close(stem_file(d_k));
         exception
            when Name_Error =>
               Text_IO.Put_Line("LOADING BDL FROM DISK had NAME_ERROR on " &
                                  add_file_name_extension(stem_file_name,
                                                          Dictionary_Kind'Image(d_k)));
               Text_IO.Put_Line("The will be no blank stems loaded");
            when Use_Error =>
               Text_IO.Put_Line("LOADING BDL FROM DISK had USE_ERROR on " &
                                  add_file_name_extension(stem_file_name,
                                                          Dictionary_Kind'Image(d_k)));
               Text_IO.Put_Line("There will be no blank stems loaded");
         end loading_bdl_from_disk;
      end if;

      --  Now load the stems of just one letter
      for d_k in general..Dictionary_Kind'Last loop
         if dictionary_available(d_k)  then
            exit when d_k = local;
            --TEXT_IO.PUT_LINE("OPENING BDL STEMFILE " & EXT(D_K));
            if not Is_Open(stem_file(d_k))  then
               --PUT_LINE("LOADING_BDL is going to OPEN " &
               --ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
               --DICTIONARY_KIND'IMAGE(D_K)));
               Open(stem_file(d_k), stem_io.In_File,
                    add_file_name_extension(stem_file_name,
                                            Dictionary_Kind'Image(d_k)));
               --STEMFILE." & EXT(D_K));
               --PUT_LINE("OPENing was successful");
            end if;
            for i in Character range 'a'..'z'  loop
               index_first := bdlf(i, ' ', d_k);
               index_last  := bdll(i, ' ', d_k);
               if index_first > 0  then
                  Set_Index(stem_file(d_k), stem_io.Positive_Count(index_first));
                  for j in index_first..index_last  loop
                     Read(stem_file(d_k), ds);
                     k := k + 1;
                     bdl(k) := ds;
                  end loop;
               end if;
            end loop;
            --TEXT_IO.PUT_LINE("Single letters LOADED FROM DISK   K = " & INTEGER'IMAGE(K));
            Close(stem_file(d_k));
         end if;

      end loop;
      bdl_last := k;

      --TEXT_IO.PUT("FINISHED LOADING BDL FROM DISK     BDL_LAST = ");
      --TEXT_IO.PUT(INTEGER'IMAGE(BDL_LAST));
      --TEXT_IO.NEW_LINE;

   end load_bdl_from_disk;

   procedure load_indices_from_indx_file(d_k : Dictionary_Kind) is
      use Text_IO;
      use Inflections_Package.Integer_IO;
      use stem_io;
      use Count_io;
      ch : String(1..2);
      m, n : stem_io.Count;
      number_of_blank_stems,
      number_of_non_blank_stems : stem_io.Count := 0;
      s : String(1..100) := (others => ' ');
      last, l : Integer := 0;

      function max(a, b : stem_io.Count) return stem_io.Count is
      begin
         if a >= b then
            return a;
         end if;
         return b;
      end max;

   begin
      Open(indx_file(d_k), Text_IO.In_File,
           add_file_name_extension(indx_file_name,
                                   Dictionary_Kind'Image(d_k)));
      --"INDXFILE." & EXT(D_K)); --  $$$$$$$$$$$$

      preface.Put(Dictionary_Kind'Image(d_k));
      preface.Put(" Dictionary loading");

      if d_k = general  then
         Get_Line(indx_file(d_k), s, last);
         ch := s(1..2);
         Get(s(4..last), m, l);
         bblf(ch(1), ch(2), d_k) := m;
         Get(s(l+1..last), n, l);
         bbll(ch(1), ch(2), d_k) := n;
         number_of_blank_stems := max(number_of_blank_stems, n);
      end if;

      while not End_Of_File(indx_file(d_k))  loop
         Get_Line(indx_file(d_k), s, last);
         exit when last = 0;
         ch := s(1..2);
         Get(s(4..last), m, l);
         if ch(2) = ' '  then
            bdlf(ch(1), ch(2), d_k) := m;
         else
            ddlf(ch(1), ch(2), d_k) := m;
         end if;
         Get(s(l+1..last), n, l);
         if ch(2) = ' '  then
            bdll(ch(1), ch(2), d_k) := n;
            number_of_blank_stems := max(number_of_blank_stems, n);
         else
            ddll(ch(1), ch(2), d_k) := n;
            number_of_non_blank_stems := max(number_of_non_blank_stems, n);
         end if;
      end loop;
      Close(indx_file(d_k));
      preface.Set_Col(33); preface.Put("--  ");
      if not config.suppress_preface  then
         Put(number_of_non_blank_stems, 6);
      end if;   --  Kludge for when TEXT_IO.COUNT too small
      preface.Put(" stems");
      preface.Set_Col(55); preface.Put_Line("--  Loaded correctly");
   end load_indices_from_indx_file;

end word_support_package;
