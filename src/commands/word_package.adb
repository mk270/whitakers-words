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

with Support_Utils.Addons_Package; use Support_Utils.Addons_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Config;  use Latin_Utils.Config;
with Support_Utils.Uniques_Package; use Support_Utils.Uniques_Package;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Latin_Utils.Preface;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
with english_support_package; use english_support_package;
package body word_package is

   inflections_sections_file : lel_section_io.File_Type;

   procedure pause (Output : Ada.Text_IO.File_Type) is
      pause_line : String (1 .. 300);
      pause_last : Integer := 0;
   begin
      if words_mdev (pause_in_screen_Output)  then
         if method = interactive  then
            if Ada.Text_IO.Name (Output) =
              Ada.Text_IO.Name (Ada.Text_IO.Standard_Output)
            then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output,
               "                          MORE - hit RETURN/ENTER to continue");
               Ada.Text_IO.Get_Line
                 (Ada.Text_IO.Standard_Input, pause_line, pause_last);
            end if;
         elsif method = Command_Line_Input  then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output,
              "                          MORE - hit RETURN/ENTER to continue");
            Ada.Text_IO.Get_Line (Ada.Text_IO.Standard_Input,
              pause_line, pause_last);
         elsif method = Command_Line_files  then
            null;                       --  Do not PAUSE
         end if;
      end if;
   exception
      when others  =>
         Ada.Text_IO.Put_Line ("Unexpected exception in PAUSE");
   end pause;

   function min (a, b : Integer) return Integer is
   begin
      if a <= b  then
         return a;
      end if;
      return b;
   end min;

   function ltu (c, d : Character) return Boolean is
   begin
      if d = 'v' then
         if c < 'u' then
            return True;
         else
            return False;
         end if;
      elsif d = 'j' then
         if c < 'i' then
            return True;
         else
            return False;
         end if;
      elsif d = 'V' then
         if c < 'U' then
            return True;
         else
            return False;
         end if;
      elsif d = 'J' then
         if c < 'I' then
            return True;
         else
            return False;
         end if;
      else
         return c < d;
      end if;
   end ltu;

   function equ (c, d : Character) return Boolean is
   begin
      if (d = 'u') or (d = 'v')  then
         if (c = 'u') or (c = 'v')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'i') or (d = 'j')  then
         if (c = 'i') or (c = 'j')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'U') or (d = 'V')  then
         if (c = 'U') or (c = 'V')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'I') or (d = 'J')  then
         if (c = 'I') or (c = 'J')  then
            return True;
         else
            return False;
         end if;
      else
         return c = d;
      end if;
   end equ;

   function gtu (c, d : Character) return Boolean is
   begin
      if d = 'u' then
         if c > 'v' then
            return True;
         else
            return False;
         end if;
      elsif d = 'i' then
         if c > 'j' then
            return True;
         else
            return False;
         end if;
      elsif d = 'U' then
         if c > 'V' then
            return True;
         else
            return False;
         end if;
      elsif d = 'I' then
         if c > 'J' then
            return True;
         else
            return False;
         end if;
      else
         return c > d;
      end if;
   end gtu;

   function ltu (s, t : String) return Boolean is
   begin
      for i in 1 .. s'Length  loop   --  Not TRIMed, so same length
         if equ (s (s'First + i - 1), t (t'First + i - 1))  then
            null;
         elsif gtu (s (s'First + i - 1), t (t'First + i - 1))  then
            return False;
         elsif ltu (s (s'First + i - 1), t (t'First + i - 1))  then
            return True;
         end if;
      end loop;
      return False;
   end ltu;

   function gtu (s, t : String) return Boolean is
   begin
      for i in 1 .. s'Length  loop   --  Not TRIMed, so same length
         if equ (s (s'First + i - 1), t (t'First + i - 1))  then
            null;
         elsif ltu (s (s'First + i - 1), t (t'First + i - 1))  then
            return False;
         elsif gtu (s (s'First + i - 1), t (t'First + i - 1))  then
            return True;
         end if;
      end loop;
      return False;
   end gtu;

   function equ (s, t : String) return Boolean is
   begin
      if s'Length /= t'Length  then
         return False;
      end if;

      for i in 1 .. s'Length  loop
         if not equ (s (s'First + i - 1), t (t'First + i - 1))  then
            return False;
         end if;
      end loop;

      return True;
   end equ;

   procedure run_uniques
     (s : in String;
      unique_found : out Boolean;
      pa : in out Parse_Array; pa_last : in out Integer)
   is
      sl : constant String        --  BAD NAME!!!!!!!!!!!!!!!!!!
        := Lower_Case (Trim (s));
      st : constant Stem_Type := Head (sl, Max_Stem_Size);
      unql : unique_list;   --  Unique list for a letter
   begin
      unique_found := False;
      if sl (sl'First) = 'v'  then
         unql := unq ('u');   --  Unique list for a letter
      elsif sl (sl'First) = 'j'  then
         unql := unq ('i');   --  Unique list for a letter
      else
         unql := unq (sl (sl'First));   --  Unique list for a letter
      end if;

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE ("Called UNIQUES with =>" & SL & "|");

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE ("UNQL ");

      while unql /= null  loop
         --  If there is a match, add to PA
         --TEXT_IO.PUT_LINE ("UNIQUE =>" & UNQL.PR.STEM);
         --if ST = LOWER_CASE (UNQL.PR.STEM)  then
         if equ (st, Lower_Case (unql.stem)) then
            pa_last := pa_last + 1;
            pa (pa_last) := (unql.stem,
              (unql.qual,
              0,
              null_ending_record,
              x,
              x),
              unique,
              unql.MNPC);

            unique_found := True;
         end if;
         unql := unql.succ;
      end loop;

   end run_uniques;

   procedure run_inflections
     (s : in String;
      sl : in out sal;
      restriction : dict_restriction := regular)
   is
      --  Trys all possible inflections against the Input word in S
      --  and constructs a STEM_LIST of those that survive SL
      use lel_section_io;
      use Inflection_Record_IO;
      word : constant String := Lower_Case (Trim (s));
      last_of_word : constant Character := word (word'Last);
      length_of_word   : constant Integer := word'Length;
      stem_length  : Integer := 0;
      pr   : Parse_Record;
      m : Integer := 1;

   begin
      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE ("Called RUN_INFLECTIONS with =>" & WORD & "|");
      if word'Length = 0  then
         sl (m) := Null_Parse_Record;
         return;
      end if;

      sa := not_a_stem_array;

      --  Add all of these to list of possible ending records
      --  since the blank ending agrees with everything
      --  PACK/PRON have no blank endings
      if ((restriction /= pack_only) and (restriction /= qu_pron_only))
        and then (word'Length <= Max_Stem_Size)
      then
         for i in belf (0, ' ') .. bell (0, ' ')  loop
            pr := (word & Null_Stem_Type
              (length_of_word + 1 .. Stem_Type'Length),
              bel (i), Default_Dictionary_Kind, Null_MNPC);
            sl (m) := pr;
            m := m + 1;
         end loop;

         --  Is always a possibility (null ending)
         sa (length_of_word) := pr.Stem;
      end if;

      --  Here we read in the INFLECTIONS_SECTION that is applicable
      if restriction = regular  then
         case last_of_word is
            when 'a' | 'c' | 'd' | 'e' | 'i'  =>
               Read (inflections_sections_file, lel, 1);
            when 'm' | 'n' | 'o' | 'r'  =>
               Read (inflections_sections_file, lel, 2);
            when 's'  =>
               Read (inflections_sections_file, lel, 3);
            when 't' | 'u'  =>
               Read (inflections_sections_file, lel, 4);
            when others  =>
               --PUT_LINE ("Only blank inflections are found");
               return;
         end case;
      elsif restriction = pack_only  or restriction = qu_pron_only  then
         Read (inflections_sections_file, lel, 4);
      end if;

      --  Now do the non-blank endings      --  Only go to LENGTH_OF_WORD
      for z in reverse 1 .. min (max_ending_size, length_of_word)  loop

         --  Check if Z agrees with a PDL SIZE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
         --  Maybe make PDL on size, if it has to be a list,
         --  or order by size if array
         if lell (z, last_of_word) > 0  then   --  Any likely inflections at all

            for i in lelf (z, last_of_word) .. lell (z, last_of_word) loop
               if equ (Lower_Case (lel (i).ending.suf (1 .. z)),
                 Lower_Case (word (word'Last - z + 1 .. word'Last)))
               then
                  --  Add to list of possible ending records
                  --STEM_LENGTH := WORD'LENGTH - LEL (I).ENDING.SIZE;
                  stem_length := word'Length - z;

                  if stem_length <= Max_Stem_Size  then
                     --  Reject too long words
                     --  Check if LEL IR agrees with PDL IR  !!!!!!!!
                     pr := (word (word'First .. stem_length) &
                       Null_Stem_Type (stem_length + 1 .. Max_Stem_Size),
                       lel (i), Default_Dictionary_Kind, Null_MNPC);
                     sl (m) := pr;
                     m := m + 1;

                     sa (stem_length) := pr.Stem;
                     --  Gets set dozens of times
                     --  Could order the endings by length (suffix sort)
                     --  so length changes slowly

                     --PUT_LINE ("LENGTH = " & INTEGER'IMAGE (STEM_LENGTH)
                     --& "   SA =>" & PR.STEM & "|");
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end run_inflections;

   procedure try_to_load_dictionary (d_k : Dictionary_Kind) is
   begin
      stem_io.Open (stem_file (d_k), stem_io.In_File,
        add_file_name_extension (stem_file_name,
        Dictionary_Kind'Image (d_k)));
      Dict_IO.Open (Dict_File (d_k), Dict_IO.In_File,
        add_file_name_extension (dict_file_name,
        Dictionary_Kind'Image (d_k)));
      load_indices_from_indx_file (d_k);
      Dictionary_Available (d_k) := True;

   exception
      when others  =>
         Dictionary_Available (d_k) := False;
   end try_to_load_dictionary;

   procedure dictionary_search (ssa : stem_array_type;
                                d_k : Dictionary_Kind;
                                restriction : dict_restriction := regular) is
      --  Prepares a PDL list of possible dictionary hits
      --  Search a dictionary (D_K) looking for all stems that match
      --  any of the stems that are physically possible with Latin inflections
      use stem_io;

      --type NAT_32 is Range 0 .. 2**31-1;   --###############
      j, j1, j2, jj : stem_io.Count := 0;

      index_on : constant String := ssa (ssa'Last);
      index_first, index_last : stem_io.Count := 0;
      ds : dictionary_stem;
      first_try, second_try : Boolean := True;

      function first_two (w : String) return String is
         --  'v' could be represented by 'u', like the
         --  new Oxford Latin Dictionary

         --  Fixes the first two letters of a word/stem which can be done right
         s : constant String := Lower_Case (w);
         ss : String (w'Range) := w;

         function ui (c : Character) return Character  is
         begin
            if c = 'v' then
               return 'u';
            elsif c = 'V' then
               return 'U';
            elsif c = 'j' then
               return 'i';
            elsif c = 'J' then
               return 'I';
            else
               return c;
            end if;
         end ui;

      begin

         if s'Length = 1  then
            ss (s'First) := ui (w (s'First));
         else
            ss (s'First)   := ui (w (s'First));
            ss (s'First + 1) := ui (w (s'First + 1));
         end if;

         return ss;
      end first_two;

      procedure load_pdl is
      begin
         case restriction is
            when regular    =>
               if not (ds.part.pofs = Pack  or
                 (ds.part.pofs = Pron  and then
                 (ds.part.Pron.Decl.Which = 1)))
               then
                  pdl_index := pdl_index + 1;
                  pdl (pdl_index) := pruned_dictionary_item'(ds, d_k);
               end if;

            when pack_only  =>
               if ds.part.pofs = Pack  then
                  pdl_index := pdl_index + 1;
                  pdl (pdl_index) := pruned_dictionary_item'(ds, d_k);
               end if;

            when qu_pron_only  =>
               if ds.part.pofs = Pron  and then
                 (ds.part.Pron.Decl.Which = 1)
               then
                  pdl_index := pdl_index + 1;
                  pdl (pdl_index) := pruned_dictionary_item'(ds, d_k);
               end if;

            when others =>
               pdl_index := pdl_index + 1;
               pdl (pdl_index) := pruned_dictionary_item'(ds, d_k);
         end case;

      end load_pdl;

   begin
      --  Now go through the dictionary list DL for the first letters
      --  and make a reduced dictionary list PDL

      if d_k = local  then
         index_first := first_index ((first_two (index_on)(1), 'a'), d_k);
         index_last  := last_index ((first_two (index_on)(1), 'a'), d_k);
      else
         index_first := first_index (first_two (index_on), d_k);
         index_last  := last_index (first_two (index_on), d_k);
      end if;

      if index_first > 0  and then index_first <= index_last then

         j1 := index_first;    --######################
         j2 := index_last;

         stem_array_loop :
         for k in ssa'Range  loop
            if Trim (ssa (k))'Length > 1  then
               --  This may be checking for 0 and 1 letter SSAs which
               --  are done elsewhere

               if d_k = local  then
                  --  Special processing for unordered DICT.LOC
                  for j in j1 .. j2  loop
                     --  Sweep exaustively through the scope
                     Set_Index (stem_file (d_k), stem_io.Count (j));
                     Read (stem_file (d_k), ds);

                     if equ (Lower_Case (ds.stem), ssa (k))  then
                        load_pdl;
                     end if;
                  end loop;
               else                     --  Regular dictionaries
                  first_try := True;

                  second_try := True;

                  j := (j1 + j2) / 2;

                  binary_search :
                  loop
                     if (j1 = j2 - 1) or (j1 = j2) then
                        if first_try  then
                           j := j1;
                           first_try := False;
                        elsif second_try  then
                           j := j2;
                           second_try := False;
                        else
                           jj := j;
                           exit binary_search;
                        end if;
                     end if;

                     Set_Index (stem_file (d_k), j);
                     Read (stem_file (d_k), ds);

                     if  ltu (Lower_Case (ds.stem), ssa (k))  then
                        j1 := j;
                        j := (j1 + j2) / 2;
                     elsif  gtu (Lower_Case (ds.stem), ssa (k))  then
                        j2 := j;
                        j := (j1 + j2) / 2;
                     else
                        for i in reverse j1 .. j  loop
                           Set_Index (stem_file (d_k), stem_io.Count (i));
                           Read (stem_file (d_k), ds);

                           if equ (Lower_Case (ds.stem), ssa (k))  then
                              jj := i;
                              load_pdl;

                           else
                              exit;
                           end if;
                        end loop;

                        for i in j + 1 .. j2  loop
                           Set_Index (stem_file (d_k), stem_io.Count (i));
                           Read (stem_file (d_k), ds);

                           if equ (Lower_Case (ds.stem), ssa (k))  then
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

   procedure search_dictionaries (ssa : in stem_array_type;
                                  restriction : dict_restriction := regular) is
      use stem_io;
      fc : Character := ' ';
   begin
      pdl := (others => null_pruned_dictionary_item);
      pdl_index := 0;
      --PUT_LINE ("Search for blank stems");
      --  BDL is always used, so it is loaded initially and not called from disk
      --  Check all stems of the dictionary entry against the reduced stems

      --  Determine if there is a pure blank "  " stem
      if len (ssa (ssa'First)) = 0    then
         --  a size would help?
         --PUT ("HIT on blank stem   I = ");PUT ('1');
         --PUT ("  STEM = ");PUT_LINE (BDL (1).STEM);
         --PDL := new PRUNED_DICTIONARY_ITEM'(BDL (1), GENERAL, PDL);
         pdl_index := pdl_index + 1;
         pdl (pdl_index) := pruned_dictionary_item'(bdl (1), general);
      end if;
      --  Now there is only one blank stem (2 of to_be),
      --  but need not always be so

      --  Determine if there is a blank stem  (SC = ' ')
      --  Prepare for the posibility that one stem is short but there are others
      fc := ' ';
      if ssa (ssa'First)(1) = ' ' then
         if ssa'Length > 1  and then ssa (ssa'First + 1)(2) = ' '  then
            fc := ssa (ssa'First + 1)(1);
         end if;
      elsif ssa (ssa'First)(2) = ' '  then
         fc := ssa (ssa'First)(1);
      end if;

      --  If there is a single letter stem  (FC /= ' ') then
      if fc /= ' '  then
         for i in 2 .. bdl_last  loop
            --  Check all stems of the dictionary entry against the
            --  reduced stems
            --if LOWER_CASE (BDL (I).STEM (1)) = FC  then
            if equ (Lower_Case (bdl (i).stem (1)),  fc)  then
               pdl_index := pdl_index + 1;
               pdl (pdl_index) := pruned_dictionary_item'(bdl (i), general);
            end if;
         end loop;
      end if;

      if ssa'Length = 0  then
         --        PUT_LINE ("Empty stem array, don't bother searching");
         return;
         --      elsif LEN (SSA (SSA'LAST)) <= 1  then
         --        PUT_LINE ("No two letter stems, have done searching");
         --      else
         --        PUT_LINE ("Searching Dictionaries");
      end if;

      for d_k in Dictionary_Kind  loop
         if Dictionary_Available (d_k)  then
            if not Is_Open (stem_file (d_k))  then
               Open (stem_file (d_k), stem_io.In_File,
                 add_file_name_extension (stem_file_name,
                 Dictionary_Kind'Image (d_k)));
            end if;
            dictionary_search (ssa, d_k, restriction);
            Close (stem_file (d_k));  --??????
         end if;
      end loop;

   end search_dictionaries;

   procedure change_language (c : Character) is
   begin  if Upper_Case (c) = 'L'  then
      language := latin_to_english;
      Preface.Put_Line
        ("Language changed to " & language_type'Image (language));
   elsif Upper_Case (c) = 'E'  then
      if English_Dictionary_Available (general)  then
         language := english_to_latin;
         Preface.Put_Line
           ("Language changed to " & language_type'Image (language));
         Preface.Put_Line
           ("InPut a single English word (+ part of speech - " &
            "N, ADJ, V, PREP, . .. )");
      else
         Preface.Put_Line ("No English dictionary available");
      end if;
   else
      Preface.Put_Line
        ("Bad LANGAUGE Input - no change, remains " &
         language_type'Image (language));
   end if;
   exception
      when others  =>
         Preface.Put_Line ("Bad LANGAUGE Input - no change, remains " &
           language_type'Image (language));
   end change_language;

   procedure word (raw_word : in String;
                   pa : in out Parse_Array; pa_last : in out Integer) is

      Input_word : constant String := Lower_Case (raw_word);
      pa_save : constant Integer := pa_last;

      unique_found : Boolean := False;

      ss, sss : sal := (others => Null_Parse_Record);

      procedure order_stems (sx : in out sal) is
         use Inflection_Record_IO;
         use Dict_IO;
         hits : Integer := 0;
         sl : sal := sx;
         sl_last : Integer := 0;
         sm : Parse_Record;
      begin
         if sx (1) = Null_Parse_Record  then
            return;
         end if;
         --PUT_LINE ("ORDERing_STEMS");

         for i in sl'Range  loop
            exit when sl (i) = Null_Parse_Record;
            sl_last := sl_last + 1;
         end loop;
         --PUT_LINE ("In ORDER  SL_LAST = " & INTEGER'IMAGE (SL_LAST));

         --  Bubble sort since this list should usually be very small (1-5)
         hit_loop :
         loop
            hits := 0;

            switch :
            begin
               --  Need to remove duplicates in ARRAY_STEMS
               --  This sort is very sloppy
               --  One problem is that it can mix up some of the order of
               --  PREFIX, XXX, LOC; I ought to do this for every set of
               --  results from different approaches not just in one fell
               --  swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               inner_loop :
               for i in 1 .. sl_last - 1  loop
                  if sl (i + 1) /= Null_Parse_Record  then
                     -- the following condition is absurd and should be
                     -- rewritten
                     if (sl (i + 1).MNPC < sl (i).MNPC) or else
                       (sl (i + 1).MNPC = sl (i).MNPC and then
                       sl (i + 1).IR.ending.size <
                         sl (i).IR.ending.size) or else
                       (sl (i + 1).MNPC = sl (i).MNPC and then
                       sl (i + 1).IR.ending.size =
                         sl (i).IR.ending.size and then
                       sl (i + 1).IR.qual < sl (i).IR.qual) or else
                       (sl (i + 1).MNPC = sl (i).MNPC and then
                       sl (i + 1).IR.ending.size =
                         sl (i).IR.ending.size and then
                       sl (i + 1).IR.qual = sl (i).IR.qual and then
                       sl (i + 1).D_K  < sl (i).D_K)
                     then
                        sm := sl (i);
                        sl (i) := sl (i + 1);
                        sl (i + 1) := sm;
                        hits := hits + 1;
                     end if;
                  else
                     exit inner_loop;
                  end if;
               end loop inner_loop;
            end switch;

            exit hit_loop when hits = 0;
         end loop hit_loop;
         sx := sl;
      end order_stems;

      procedure array_stems
        (sx : in sal;
         pa : in out Parse_Array; pa_last : in out Integer)
      is
         sl : constant sal := sx;
         opr : Parse_Record := Null_Parse_Record;
      begin

         if sl (1) = Null_Parse_Record  then
            return;
         else

            opr := Null_Parse_Record;
            for i in sl'Range  loop
               if sl (i) /= Null_Parse_Record  then
                  --PUT ('*'); PUT (SL (I)); NEW_LINE;

                  supress_key_check :
                  declare
                     function "<=" (a, b : Parse_Record) return Boolean is
                        use Dict_IO;
                     begin  --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                        if a.IR.qual = b.IR.qual and then
                          a.MNPC = b.MNPC
                        then
                           return True;
                        else
                           return False;
                        end if;
                     end "<=";
                  begin
                     if sl (i) <= opr then
                        --  Get rid of duplicates, if ORDER is OK
                        --PUT ('-'); PUT (SL (I)); NEW_LINE;
                        null;
                     else
                        pa_last := pa_last + 1;
                        pa (pa_last) := sl (i);
                        opr := sl (i);
                     end if;
                  end supress_key_check;
               else
                  exit;
               end if;
            end loop;
         end if;
      end array_stems;

      procedure reduce_stem_list
        (sl : in sal;
         sxx : in out sal;
         --  Need in out if want to print it at the end
         --procedure REDUCE_STEM_LIST (SL : in SAL; SXX : out SAL;
         prefix : in prefix_item := null_prefix_item;
         suffix : in suffix_item := null_suffix_item)
      is
         MNPC_part : MNPC_Type := Null_MNPC;
         pdl_part : Part_Entry;
         com : Comparison_Type := X;
         num_sort : Numeral_Sort_Type := X;
         ls : Integer := 0;
         m : Integer := 0;

         pdl_key : Stem_Key_Type;
         pdl_p   : Part_Of_Speech_Type;
         --sl_key  : Stem_Key_Type;
         --sl_p    : Part_Of_Speech_Type;

         function "<=" (left, right : Part_Of_Speech_Type) return Boolean is
         begin
            if right = left  or else
              (left = Pack and right = Pron)  or else
              right = X
            then
               return True;
            else
               return False;
            end if;
         end "<=";

         function "<=" (left, right : Gender_Type)   return Boolean is
         begin
            if right = left               or else
              (right = C and left /= N)  or else
              right = X
            then
               return True;
            else
               return False;
            end if;
         end "<=";

         function "<=" (left, right : Stem_Key_Type)   return Boolean is
         begin
            if right = left or else right = 0 then
               return True;
            else
               return False;
            end if;
         end "<=";

      begin
         sxx := (others => Null_Parse_Record);
         --  Essentially initializing
         --  For the reduced dictionary list PDL
         m := 0;

         on_pdl :
         for j in 1 .. pdl_index  loop

            pdl_part := pdl (j).ds.part;
            pdl_key := pdl (j).ds.key;
            MNPC_part := pdl (j).ds.MNPC;

            --  Is there any point in going through the process for this PDL
            pdl_p  := pdl (j).ds.part.pofs;  --  Used only for FIX logic below

            --  If there is no SUFFIX then carry on
            if suffix = null_suffix_item then
               --  No suffix working, fall through
               null;
            elsif
              --  No suffix for abbreviations
              (pdl_p = N    and then pdl_part.N.Decl = (9, 8)) or
              (pdl_p = Adj  and then pdl_part.Adj.Decl = (9, 8))
            then
               --   Can be no suffix on abbreviation");
               goto end_of_pdl_loop;
            else
               --  There is SUFFIX, see if it agrees with PDL
               --  Does SUFFIX agree in ROOT
               if pdl_p <= suffix.entr.root  and then
                 ((pdl_key <= suffix.entr.root_key)  or else
                 ((pdl_key = 0) and then
                 ((pdl_p = N) or (pdl_p = Adj) or (pdl_p = V)) and then
                 ((suffix.entr.root_key = 1) or (suffix.entr.root_key = 2))))
               then
                  --  Transform PDL_PART to TARGET
                  case suffix.entr.Target.pofs is
                     when N =>
                        pdl_part := (N, suffix.entr.Target.n);
                     when Pron =>
                        pdl_part := (Pron, suffix.entr.Target.pron);
                     when Adj =>
                        pdl_part := (Adj, suffix.entr.Target.adj);
                     when Num =>
                        pdl_part := (Num, suffix.entr.Target.num);
                     when Adv =>
                        pdl_part := (Adv, suffix.entr.Target.adv);
                     when V =>
                        pdl_part := (V, suffix.entr.Target.v);
                     when others  =>
                        null;        --  No others so far, except X = all
                  end case;
                  pdl_key := suffix.entr.Target_key;
                  pdl_p  := pdl_part.pofs;  --  Used only for FIX logic below
               else
                  --PUT_LINE ("In REDUCE_STEM_LIST   There is no legal suffix");
                  --            exit;
                  goto end_of_pdl_loop;
               end if;
            end if;

            if prefix = null_prefix_item then      --  No PREFIX, drop through
               null;
            elsif
              --  No prefix for abbreviations
              (pdl_p = N    and then pdl_part.N.Decl = (9, 8)) or
              (pdl_p = Adj  and then pdl_part.Adj.Decl = (9, 8)) or
              (pdl_p = Interj  or pdl_p = Conj)  --  or INTERJ or CONJ
            then
               goto end_of_pdl_loop;
            else
               if (pdl_p = prefix.entr.root)  or    --  = ROOT
                 (pdl_part.pofs = prefix.entr.root)  --  or part mod by suf
               then
                  null;
               elsif prefix.entr.root = X then  --   or ROOT = X
                  null;
               else
                  goto end_of_pdl_loop;
               end if;
            end if;

            --  SUFFIX and PREFIX either agree or don't exist
            --  (agrees with everything)
            ls := len (add_suffix
              (add_prefix (pdl (j).ds.stem, prefix), suffix));
            on_sl :
            for i in sl'Range loop
               exit on_sl when sl (i) = Null_Parse_Record;

               if ls  = len (sl (i).Stem)  then

                  --  Scan through the whole unreduced stem list
                  --  Single out those stems that match (pruned) dictionary
                  --   entries
                  --^^^^^should be able to do this better with new arrangement

                  --sl_key := sl (i).ir.key;
                  --sl_p := sl (i).ir.qual.pofs;

                  if (
                    ((pdl_key <= sl (i).IR.key))  or else
                    ((pdl_key = 0)  and then
                    (((pdl_p = N) or (pdl_p = Adj) or (pdl_p = V)) and then
                    ((sl (i).IR.key = 1) or (sl (i).IR.key = 2))))
                     )  and then   --  and KEY
                    (pdl_part.pofs  = eff_part (sl (i).IR.qual.pofs))
                  then
                     if pdl_part.pofs = N                            and then
                       pdl_part.N.Decl <= sl (i).IR.qual.N.Decl      and then
                       pdl_part.N.Gender <= sl (i).IR.qual.N.Gender
                     then
                        --  Need to transfer the gender of the noun
                        --  dictionary item
                        m := m + 1;
                        sxx (m) :=
                          (Stem => subtract_prefix (sl (i).Stem, prefix),
                          IR => (
                          qual => (
                          pofs => N,
                          N => (
                          pdl_part.N.Decl,
                          sl (i).IR.qual.N.Of_Case,
                          sl (i).IR.qual.N.Number,
                          pdl_part.N.Gender)),
                          key => sl (i).IR.key,
                          ending => sl (i).IR.ending,
                          age => sl (i).IR.age,
                          freq => sl (i).IR.freq),
                          D_K => pdl (j).d_k,
                          MNPC => MNPC_part);

                     elsif pdl_part.pofs = Pron and then
                       pdl_part.Pron.Decl <= sl (i).IR.qual.Pron.Decl
                     then
                        --PUT (" HIT  PRON  ");
                        --  Need to transfer the kind of the pronoun
                        --  dictionary item
                        m := m + 1;
                        sxx (m) :=
                          (Stem => subtract_prefix (sl (i).Stem, prefix),
                          IR => (
                          qual => (
                          pofs => Pron,
                          Pron => (
                          pdl_part.Pron.Decl,
                          sl (i).IR.qual.Pron.Of_Case,
                          sl (i).IR.qual.Pron.Number,
                          sl (i).IR.qual.Pron.Gender)),
                          key => sl (i).IR.key,
                          ending => sl (i).IR.ending,
                          age => sl (i).IR.age,
                          freq => sl (i).IR.freq),
                          D_K => pdl (j).d_k,
                          MNPC => MNPC_part);

                     elsif (pdl_part.pofs = Adj) and then
                       (pdl_part.Adj.Decl <= sl (i).IR.qual.Adj.Decl) and then
                       ((sl (i).IR.qual.Adj.Comparison <= pdl_part.Adj.Co) or
                       ((sl (i).IR.qual.Adj.Comparison = X) or
                       (pdl_part.Adj.Co = X)))
                     then
                        --  Note the reversal on comparisom
                        --PUT (" HIT  ADJ   ");
                        --  Need to transfer the gender of the dictionary item
                        --  Need to transfer the CO of the ADJ dictionary item
                        if pdl_part.Adj.Co in Pos .. Super  then
                           --  If the dictionary entry has a unique CO, use it
                           com := pdl_part.Adj.Co;
                        else
                           --  Otherwise, the entry is X, generate a CO from KEY
                           com := adj_comp_from_key (pdl_key);
                        end if;
                        m := m + 1;
                        sxx (m) :=
                          (Stem => subtract_prefix (sl (i).Stem, prefix),
                          IR => (
                          qual => (
                          pofs => Adj,
                          Adj => (
                          pdl_part.Adj.Decl,
                          sl (i).IR.qual.Adj.Of_Case,
                          sl (i).IR.qual.Adj.Number,
                          sl (i).IR.qual.Adj.Gender,
                          com)),
                          key => sl (i).IR.key,
                          ending => sl (i).IR.ending,
                          age => sl (i).IR.age,
                          freq => sl (i).IR.freq),
                          D_K => pdl (j).d_k,
                          MNPC => MNPC_part);

                     elsif (pdl_part.pofs = Num) and then
                       (pdl_part.Num.Decl <= sl (i).IR.qual.Num.Decl) and then
                       (pdl_key         = sl (i).IR.key)
                     then
                        --PUT(" HIT  NUM    ");
                        if pdl_part.Num.Sort = X  then
                           --  If the entry is X, generate a CO from KEY
                           num_sort := num_sort_from_key (pdl_key);
                        else
                           --  Otherwise, the dictionary entry has a
                           --  unique CO, use it
                           num_sort := pdl_part.Num.Sort;
                        end if;
                        m := m + 1;
                        sxx (m) :=
                          (Stem => subtract_prefix (sl (i).Stem, prefix),
                          IR => (
                          qual => (
                          pofs => Num,
                          Num => (
                          pdl_part.Num.Decl,
                          sl (i).IR.qual.Num.Of_Case,
                          sl (i).IR.qual.Num.Number,
                          sl (i).IR.qual.Num.Gender,
                          num_sort)),
                          key => sl (i).IR.key,
                          ending => sl (i).IR.ending,
                          age => sl (i).IR.age,
                          freq => sl (i).IR.freq),
                          D_K => pdl (j).d_k,
                          MNPC => MNPC_part);

                     elsif (pdl_part.pofs = Adv) and then
                       ((pdl_part.Adv.Co <= sl (i).IR.qual.Adv.Comparison) or
                       ((sl (i).IR.qual.Adv.Comparison = X) or
                       (pdl_part.Adv.Co = X)))
                     then
                        --PUT (" HIT  ADV   ");
                        --  Need to transfer the CO of the ADV dictionary item
                        if pdl_part.Adv.Co in Pos .. Super  then
                           --  If the dictionary entry has a unique CO, use it
                           com := pdl_part.Adv.Co;
                        else
                           --  The entry is X and we need to generate
                           --  a COMP from the KEY
                           com := adv_comp_from_key (pdl_key);
                        end if;
                        m := m + 1;
                        sxx (m) :=
                          (Stem => subtract_prefix (sl (i).Stem, prefix),
                          IR => (
                          qual => (
                          pofs => Adv,
                          Adv => (
                          Comparison => com)),
                          key => sl (i).IR.key,
                          ending => sl (i).IR.ending,
                          age => sl (i).IR.age,
                          freq => sl (i).IR.freq),
                          D_K => pdl (j).d_k,
                          MNPC => MNPC_part);

                     elsif pdl_part.pofs = V then
                        --TEXT_IO.PUT_LINE ("V found, now check CON");
                        if sl (i).IR.qual.pofs = V     and then
                          (pdl_part.V.Con <= sl (i).IR.qual.V.Con)
                        then
                           --TEXT_IO.PUT (" HIT  V     ");
                           m := m + 1;
                           sxx (m) :=
                             (Stem => subtract_prefix (sl (i).Stem, prefix),
                             IR => (
                             qual => (
                             pofs => V,
                             V => (
                             pdl_part.V.Con,
                             sl (i).IR.qual.V.Tense_Voice_Mood,
                             sl (i).IR.qual.V.Person,
                             sl (i).IR.qual.V.Number)),
                             key => sl (i).IR.key,
                             ending => sl (i).IR.ending,
                             age => sl (i).IR.age,
                             freq => sl (i).IR.freq),
                             D_K => pdl (j).d_k,
                             MNPC => MNPC_part);

                        elsif sl (i).IR.qual.pofs = Vpar   and then
                          (pdl_part.V.Con <= sl (i).IR.qual.Vpar.Con)
                        then
                           --PUT (" HIT  VPAR  ");
                           m := m + 1;
                           sxx (m) :=
                             (Stem => subtract_prefix (sl (i).Stem, prefix),
                             IR => (
                             qual => (
                             pofs => Vpar,
                             Vpar => (
                             pdl_part.V.Con,
                             sl (i).IR.qual.Vpar.Of_Case,
                             sl (i).IR.qual.Vpar.Number,
                             sl (i).IR.qual.Vpar.Gender,
                             sl (i).IR.qual.Vpar.Tense_Voice_Mood)),
                             key => sl (i).IR.key,
                             ending => sl (i).IR.ending,
                             age => sl (i).IR.age,
                             freq => sl (i).IR.freq),
                             D_K => pdl (j).d_k,
                             MNPC => MNPC_part);

                        elsif sl (i).IR.qual.pofs = Supine   and then
                          (pdl_part.V.Con <= sl (i).IR.qual.Supine.Con)
                        then
                           --PUT (" HIT  SUPINE");
                           m := m + 1;
                           sxx (m) :=
                             (Stem => subtract_prefix (sl (i).Stem, prefix),
                             IR => (
                             qual => (
                             pofs => Supine,
                             Supine => (
                             pdl_part.V.Con,
                             sl (i).IR.qual.Supine.Of_Case,
                             sl (i).IR.qual.Supine.Number,
                             sl (i).IR.qual.Supine.Gender)),
                             key => sl (i).IR.key,
                             ending => sl (i).IR.ending,
                             age => sl (i).IR.age,
                             freq => sl (i).IR.freq),
                             D_K => pdl (j).d_k,
                             MNPC => MNPC_part);
                        end if;

                     elsif pdl_part.pofs = Prep and then
                       pdl_part.Prep.Obj = sl (i).IR.qual.Prep.Of_Case
                     then
                        --PUT (" HIT  PREP  ");
                        m := m + 1;
                        sxx (m) :=
                          (subtract_prefix (sl (i).Stem, prefix), sl (i).IR,
                          pdl (j).d_k, MNPC_part);

                     elsif pdl_part.pofs = Conj then
                        --PUT (" HIT  CONJ  ");
                        m := m + 1;
                        sxx (m) :=
                          (subtract_prefix (sl (i).Stem, prefix), sl (i).IR,
                          pdl (j).d_k, MNPC_part);

                     elsif pdl_part.pofs = Interj then
                        --PUT (" HIT  INTERJ ");
                        m := m + 1;
                        sxx (m) :=
                          (subtract_prefix (sl (i).Stem, prefix), sl (i).IR,
                          pdl (j).d_k, MNPC_part);

                     end if;

                  end if;
               end if;
            end loop on_sl;

         <<end_of_pdl_loop>> null;
         end loop on_pdl;
      end reduce_stem_list;

      procedure apply_prefix
        (sa : in stem_array_type;
         suffix : in suffix_item;
         sx : in sal;
         sxx : in out sal;
         pa : in out Parse_Array;
         pa_last : in out Integer)
      is
         --  Worry about the stem changing re-cipio from capio
         --  Correspondence of parts, need EFF for VPAR
         --  The prefixes should be ordered with the longest/most likely first
         ssa : stem_array;
         l : Integer :=  0;

      begin
         --PUT_LINE ("Entering APPLY_PREFIX");
         sxx := (others => Null_Parse_Record);    --  !!!!!!!!!!!!!!!!!!!!!!!

         if words_mdev (use_prefixes)  then

            for i in 1 .. number_of_prefixes  loop
               --  Loop through PREFIXES
               l :=  0;
               for j in sa'Range  loop
                  --  Loop through stem array
                  if sa (j)(1) = prefixes (i).fix (1) then
                     --  Cuts down a little -- do better
                     if subtract_prefix (sa (j), prefixes (i)) /=
                       Head (sa (j), Max_Stem_Size)
                     then
                        l := l + 1;
                        --  We have a hit, make new stem array item
                        ssa (l) := Head (subtract_prefix (sa (j), prefixes (i)),
                          Max_Stem_Size);
                        --  And that has prefix subtracted to match dict
                     end if; --  with prefix subtracted stems
                  end if;
               end loop;

               if l > 0  then
                  --  There has been a prefix hit
                  search_dictionaries (ssa (1 .. l));
                  --  So run new dictionary search

                  if  pdl_index /= 0     then
                     --  Dict search was successful
                     reduce_stem_list (sx, sxx, prefixes (i), suffix);

                     if sxx (1) /= Null_Parse_Record  then
                        --  There is reduced stem result
                        pa_last := pa_last + 1;
                        --  So add prefix line to parse array
                        pa (pa_last).IR :=
                          ((Prefix, Null_Prefix_Record), 0,
                          null_ending_record, x, x);
                        pa (pa_last).Stem :=
                          Head (prefixes (i).fix, Max_Stem_Size);
                        pa (pa_last).MNPC := Dict_IO.Count (prefixes (i).MNPC);
                        pa (pa_last).D_K  := addons;
                        exit;      --  Because we accept only one prefix
                     end if;
                  end if;
               end if;
            end loop;      --  Loop on I for PREFIXES
         end if;  --  On USE_PREFIXES
      end apply_prefix;

      procedure apply_suffix
        (sa : in stem_array_type;
         sx : in sal;
         sxx : in out sal;
         pa : in out Parse_Array;
         pa_last : in out Integer)
      is
         ssa : stem_array;
         l : Integer :=  0;
         suffix_hit : Integer := 0;
         --            use TEXT_IO;
         --            use INFLECTIONS_PACKAGE.INTEGER_IO;

      begin
         for i in 1 .. number_of_suffixes  loop       --  Loop through SUFFIXES
            l :=  0;                                 --  Take as many as fit

            for j in sa'Range  loop                  --  Loop through stem array
               if subtract_suffix (sa (j), suffixes (i)) /=
                 Head (sa (j), Max_Stem_Size)
               then
                  l := l + 1;
                  --  We have a hit, make new stem array item
                  ssa (l) := Head (subtract_suffix (sa (j), suffixes (i)),
                    Max_Stem_Size);
                  --  And that has prefix subtracted to match dict
               end if;
            end loop;    --  Loop on J through SA

            if l > 0  then
               --  There has been a suffix hit
               search_dictionaries (ssa (1 .. l));
               --  So run new dictionary search
               --  For suffixes we allow as many as match

               if pdl_index /= 0 then
                  --  Dict search was successful
                  suffix_hit := i;

                  reduce_stem_list (sx, sxx, null_prefix_item, suffixes (i));

                  if sxx (1) /= Null_Parse_Record  then
                     --  There is reduced stem result
                     pa_last := pa_last + 1;
                     --  So add suffix line to parse array
                     pa (pa_last).IR :=
                       ((Suffix, Null_Suffix_Record),
                       0, null_ending_record, x, x);
                     pa (pa_last).Stem :=
                       Head (suffixes (suffix_hit).fix, Max_Stem_Size);
                     --  Maybe it would better if suffix.fix was of stem size
                     pa (pa_last).MNPC :=
                       Dict_IO.Count (suffixes (suffix_hit).MNPC);
                     pa (pa_last).D_K  := addons;
                     ---
                     for i in sxx'Range  loop
                        exit when sxx (i) = Null_Parse_Record;
                        pa_last := pa_last + 1;
                        pa (pa_last) := sxx (i);
                     end loop;
                     ---
                  end if;

               else   --  there is suffix (L /= 0) but no dictionary hit
                  suffix_hit := i;
                  apply_prefix
                    (ssa (1 .. l), suffixes (i), sx, sxx, pa, pa_last);
                  if sxx (1) /= Null_Parse_Record  then
                     --  There is reduced stem result
                     pa_last := pa_last + 1;
                     --  So add suffix line to parse array
                     pa (pa_last).IR :=
                       ((Suffix, Null_Suffix_Record),
                       0, null_ending_record, x, x);
                     pa (pa_last).Stem := Head
                       (suffixes (suffix_hit).fix, Max_Stem_Size);
                     pa (pa_last).MNPC :=
                       Dict_IO.Count (suffixes (suffix_hit).MNPC);
                     pa (pa_last).D_K  := addons;

                     for i in sxx'Range  loop    --  Set this set of results
                        exit when sxx (i) = Null_Parse_Record;
                        pa_last := pa_last + 1;
                        pa (pa_last) := sxx (i);
                     end loop;
                  end if;
               end if;
            end if;             --  with suffix subtracted stems
         end loop;      --  Loop on I for SUFFIXES
      end apply_suffix;

      procedure prune_stems
        (Input_word : String;
         sx : in sal;
         sxx : in out sal)
      is
         j : Integer := 0;
         --SXX : SAL;

      begin
         if sx (1) = Null_Parse_Record  then
            return;
         end if;

         -----------------------------------------------------------------

         generate_reduced_stem_array :
         begin
            j := 1;
            for z in 0 .. min (Max_Stem_Size, len (Input_word))  loop
               if sa (z) /= not_a_stem  then
                  --PUT (Z); PUT (J); PUT ("  "); PUT_LINE (SA (Z));
                  ssa (j) := sa (z);
                  ssa_max := j;
                  j := j + 1;
               end if;
            end loop;
         end generate_reduced_stem_array;

         if not words_mdev (do_only_fixes)  then
            --  Just bypass main dictionary search

            search_dictionaries (ssa (1 .. ssa_max));

         end if;

         if (((pa_last = 0)  and            --  No Uniques or Syncope
           (pdl_index = 0))  --)   and then    --  No dictionary match
           or words_mdev (do_fixes_anyway))  and then
           words_mode (do_fixes)
         then

            ----So try prefixes and suffixes,
            --- Generate a new SAA array, search again

            if sxx (1) = Null_Parse_Record  then
               --  We could not find a match with suffix
               apply_prefix (ssa (1 .. ssa_max),
                 null_suffix_item, sx, sxx, pa, pa_last);
            end if;
            --------------
            if sxx (1) = Null_Parse_Record  then
               --  We could not find a match with suffix
               apply_suffix (ssa (1 .. ssa_max), sx, sxx, pa, pa_last);
               if sxx (1) = Null_Parse_Record  then
                  --  We could not find a match with suffix
                  ----So try prefixes, Generate a new SAA array, search again
                  ----Need to use the new SSA, modified to include suffixes
                  apply_prefix (ssa (1 .. ssa_max),
                    null_suffix_item, sx, sxx, pa, pa_last);
                  --------------
               end if;       --  Suffix failed
            end if;       --  Suffix failed
         else
            reduce_stem_list (sx, sxx, null_prefix_item, null_suffix_item);
            if pa_last = 0  and then  sxx (1) = Null_Parse_Record  then
               --------------
               if words_mode (do_fixes)  then
                  apply_suffix (ssa (1 .. ssa_max), sx, sxx, pa, pa_last);
                  if sxx (1) = Null_Parse_Record  then
                     --  We could not find a match with suffix
                     ----So try prefixes, Generate a new SAA array, search again
                     ----Need to use the new SSA, modified to include suffixes
                     apply_prefix (ssa (1 .. ssa_max), null_suffix_item,
                       sx, sxx, pa, pa_last);
                  end if;   --  Suffix failed
               end if;     --  If DO_FIXES then do
            end if;       --  First search passed but SXX null
         end if;         --  First search failed

      end prune_stems;

      procedure process_packons (Input_word : String) is

         stem_length  : Integer := 0;
         pr   : Parse_Record;
         m : Integer := 1;
         de : Dictionary_Entry;
         mean : Meaning_Type;
         packon_first_hit : Boolean := False;
         sl : sal := (others => Null_Parse_Record);
         sl_nulls : constant sal := (others => Null_Parse_Record);

      begin

         over_packons :
         for k in packons'Range  loop
            -- Do whole set, more than one may apply
            --  PACKON if the TACKON ENTRY is PRON

            for_each_packon :
            declare
               xword : constant String :=
                 subtract_tackon (Input_word, packons (k));
               word : String (1 .. xword'Length) := xword;
               packon_length : constant Integer :=
                 Trim (packons (k).tack)'Length;
               last_of_word : Character := word (word'Last);
               length_of_word   : constant Integer := word'Length;
            begin
               sl := sl_nulls;      --  Initialize SL to nulls
               if word  /= Input_word  then
                  packon_first_hit := True;

                  if packons (k).tack (1 .. 3) = "dam"
                    and  last_of_word = 'n'
                  then
                     --  Takes care of the m - > n shift with dam
                     word (word'Last) := 'm';
                     last_of_word := 'm';
                  end if;

                  --  No blank endings in these pronouns
                  lel_section_io.Read (inflections_sections_file, lel, 4);

                  m := 0;

                  on_inflects :
                  for z in reverse 1 .. min (6, length_of_word)  loop
                     --  optimum for qu-pronouns
                     if pell (z, last_of_word) > 0  then
                        --  Any possible inflections at all
                        for i in pelf
                          (z, last_of_word) .. pell (z, last_of_word) loop
                           if (z <= length_of_word)  and then
                             ((equ (lel (i).ending.suf (1 .. z),
                             word (word'Last - z + 1 .. word'Last)))  and
                             (lel (i).qual.Pron.Decl <=
                             packons (k).entr.base.pack.Decl))
                           then
                              --  Have found an ending that is a possible match
                              --  And INFLECT agrees with PACKON.BASE
                              --  Add to list of possible ending records
                              stem_length := word'Length - z;
                              pr := (Head (word (word'First .. stem_length),
                                Max_Stem_Size),
                                lel (i), Default_Dictionary_Kind, Null_MNPC);
                              m := m + 1;
                              sl (m) := pr;
                              ssa (1) := Head
                                (word
                                (word'First .. word'First + stem_length - 1),
                                Max_Stem_Size);
                              --  may Get set several times
                           end if;
                        end loop;
                     end if;
                  end loop on_inflects;

                  --  Only one stem will emerge
                  pdl_index := 0;
                  search_dictionaries (ssa (1 .. 1),
                    pack_only);
                  --  Now have a PDL, scan for agreement

                  pdl_loop :
                  for j in 1 .. pdl_index  loop
                     --  Go through all dictionary hits to see
                     --  M used here wher I is used in REDUCE,
                     --  maybe make consistent
                     m := 1;

                     sl_loop :
                     while sl (m) /= Null_Parse_Record  loop
                        --  Over all inflection hits
                        --  if this stem is possible
                        --  call up the meaning to check for "(w/-"
                        Dict_IO.Set_Index (Dict_File (pdl (j).d_k),
                          pdl (j).ds.MNPC);
                        Dict_IO.Read (Dict_File (pdl (j).d_k), de);
                        mean := de.Mean;

                        -- there is no way this condition can be True;
                        -- packon_length - 1 /= packon_length

                        --  Does attached PACKON agree
                        if Trim (mean)(1 .. 4) = "(w/-" and then
                          Trim (mean)(5 .. 4 + packon_length) =
                          Trim (packons (k).tack)
                        then
                           if pdl (j).ds.part.Pack.Decl =
                             sl (m).IR.qual.Pron.Decl
                           then  --  or
                              if packon_first_hit then
                                 pa_last := pa_last + 1;
                                 pa (pa_last) := (packons (k).tack,
                                   ((Tackon, Null_Tackon_Record), 0,
                                   null_ending_record, x, x),
                                   addons,
                                   Dict_IO.Count ((packons (k).MNPC)));
                                 packon_first_hit := False;
                              end if;
                              pa_last := pa_last + 1;
                              pa (pa_last) := (Stem => sl (m).Stem,
                                IR => (
                                qual => (
                                pofs => Pron,
                                Pron => (
                                pdl (j).ds.part.Pack.Decl,
                                sl (m).IR.qual.Pron.Of_Case,
                                sl (m).IR.qual.Pron.Number,
                                sl (m).IR.qual.Pron.Gender)),
                                key => sl (m).IR.key,
                                ending => sl (m).IR.ending,
                                age => sl (m).IR.age,
                                freq => sl (m).IR.freq),
                                D_K => pdl (j).d_k,
                                MNPC => pdl (j).ds.MNPC);
                              --end if;
                           end if;
                        end if;
                        m := m + 1;

                     end loop sl_loop;

                  end loop pdl_loop;

               end if;
            end for_each_packon;

            packon_first_hit := False;

         end loop over_packons;
      end process_packons;

      procedure process_qu_pronouns
        (Input_word : String;
         qkey : Stem_Key_Type := 0)
      is
         word : constant String := Lower_Case (Trim (Input_word));
         last_of_word : constant Character := word (word'Last);
         length_of_word   : constant Integer := word'Length;
         stem_length  : Integer := 0;
         m : Integer := 0;
         pr   : Parse_Record;
         sl : sal := (others => Null_Parse_Record);

      begin
         --TEXT_IO.PUT_LINE ("PROCESS_QU_PRONOUNS   " & INPUT_WORD);

         --  No blank endings in these pronouns
         lel_section_io.Read (inflections_sections_file, lel, 4);

         --  M used here while I is used in REDUCE, maybe make consistent
         m := 0;

         on_inflects :
         for z in reverse 1 .. min (4, length_of_word)  loop
            --  optimized for qu-pronouns
            if pell (z, last_of_word) > 0  then
               --  Any possible inflections at all
               for i in pelf (z, last_of_word) .. pell (z, last_of_word) loop
                  if (z <= length_of_word)  and then
                    lel (i).key = qkey  and then
                    equ (lel (i).ending.suf (1 .. z),
                    word (word'Last - z + 1 .. word'Last))
                  then
                     --  Have found an ending that is a possible match
                     --  Add to list of possible ending records
                     stem_length := word'Length - z;
                     pr := (Head (word (word'First .. stem_length),
                       Max_Stem_Size),
                       lel (i), Default_Dictionary_Kind, Null_MNPC);
                     m := m + 1;
                     sl (m) := pr;
                     ssa (1) :=
                       Head (word (word'First .. word'First + stem_length - 1),
                       Max_Stem_Size);
                     --  may Get set several times
                  end if;
               end loop;
            end if;
         end loop on_inflects;

         --  Only one stem will emerge
         pdl_index := 0;
         search_dictionaries (ssa (1 .. 1),
           qu_pron_only);
         --  Now have a PDL, scan for agreement

         pdl_loop :
         for j in 1 .. pdl_index  loop
            --  Go through all dictionary hits to see
            m := 1;

            sl_loop :
            while sl (m) /= Null_Parse_Record  loop
               --  Over all inflection hits
               if pdl (j).ds.part.Pron.Decl = sl (m).IR.qual.Pron.Decl then
                  pa_last := pa_last + 1;
                  pa (pa_last) := (Stem => sl (m).Stem,
                    IR => (
                    qual => (
                    pofs => Pron,
                    Pron => (
                    pdl (j).ds.part.Pron.Decl,
                    sl (m).IR.qual.Pron.Of_Case,
                    sl (m).IR.qual.Pron.Number,
                    sl (m).IR.qual.Pron.Gender)),
                    key => sl (m).IR.key,
                    ending => sl (m).IR.ending,
                    age => sl (m).IR.age,
                    freq => sl (m).IR.freq),
                    D_K => pdl (j).d_k,
                    MNPC => pdl (j).ds.MNPC);
               end if;
               m := m + 1;

            end loop sl_loop;
            -- PDL:= PDL.SUCC;
         end loop pdl_loop;

      end process_qu_pronouns;

      procedure try_tackons (Input_word : String) is
         tackon_hit : Boolean := False;
         tackon_on  : Boolean := False;
         j : Integer := 0;
         de : Dictionary_Entry := Null_Dictionary_Entry;
         entering_pa_last : constant Integer := pa_last;
         start_of_loop : constant Integer := 5;
         --  4 enclitics     --  Hard number  !!!!!!!!!!!!!!!
         end_of_loop : constant Integer := number_of_tackons;
      begin
         loop_over_tackons :
         for i in start_of_loop .. end_of_loop  loop

            remove_a_tackon :
            declare
               less : constant String :=
                 subtract_tackon (Input_word, tackons (i));
            begin
               --TEXT_IO.PUT_LINE ("LESS = " & LESS);
               if less  /= Input_word  then       --  LESS is less
                  word (less, pa, pa_last);

                  if pa_last > entering_pa_last  then
                     --  we have a possible word
                     if tackons (i).entr.base.pofs = X  then
                        tackon_hit := True;
                        tackon_on  := False;
                     else
                        j := pa_last;

                        while j >= entering_pa_last + 1  loop
                           --  Sweep backwards over PA
                           --  Sweeping up inapplicable fixes,
                           --  although we only have TACKONs for X
                           --    or PRON or ADJ - so far
                           --  and there are no fixes for PRON - so far

                           if pa (j).IR.qual.pofs = Prefix
                             and then tackon_on
                           then
                              null;          --  check PART
                              tackon_on  := False;
                           elsif pa (j).IR.qual.pofs = Suffix
                             and then tackon_on
                           then
                              --  check PART
                              null;
                              tackon_on  := False;
                           elsif pa (j).IR.qual.pofs =
                             tackons (i).entr.base.pofs
                           then
                              Dict_IO.Set_Index
                                (Dict_File (pa (j).D_K), pa (j).MNPC);
                              Dict_IO.Read (Dict_File (pa (j).D_K), de);

                              --  check PART
                              case tackons (i).entr.base.pofs is
                                 when N       =>
                                    if pa (j).IR.qual.N.Decl <=
                                      tackons (i).entr.base.n.Decl
                                    then
                                       --  Ignore GEN and KIND
                                       tackon_hit := True;
                                       tackon_on  := True;
                                    end if;
                                 when Pron    =>
                                    --  Only one we have other than X
                                    if pa (j).IR.qual.Pron.Decl <=
                                      tackons (i).entr.base.pron.Decl
                                    then
                                       tackon_hit := True;
                                       tackon_on  := True;
                                    else
                                       pa (j .. pa_last - 1) :=
                                         pa (j + 1 .. pa_last);
                                       pa_last := pa_last - 1;

                                    end if;
                                 when Adj     =>
                                    --  Forego all checks, even on DECL of ADJ
                                    --  -cumque is the only one I have now
                                    --  if  . .. .. ..
                                    tackon_hit := True;
                                    tackon_on  := True;
                                    --  else
                                    --    PA (J .. PA_LAST - 1) :=
                                    --       PA (J + 1 .. PA_LAST);
                                    --    PA_LAST := PA_LAST - 1;
                                    --  end if;

                                    --when ADV     =>
                                    --when V       =>
                                 when others  =>
                                    pa (j .. pa_last - 1) :=
                                      pa (j + 1 .. pa_last);
                                    pa_last := pa_last - 1;
                              end case;
                           else --  check PART
                              pa (j .. pa_last - 1) := pa (j + 1 .. pa_last);
                              pa_last := pa_last - 1;
                           end if; --  check PART
                           j := j - 1;
                        end loop; --  loop sweep over PA
                     end if; --  on PART (= X?)

                     -----------------------------------------
                     if tackon_hit  then
                        pa_last := pa_last + 1;
                        pa (entering_pa_last + 2 .. pa_last) :=
                          pa (entering_pa_last + 1 .. pa_last - 1);
                        pa (entering_pa_last + 1) := (tackons (i).tack,
                          ((Tackon, Null_Tackon_Record), 0,
                          null_ending_record, x, x),
                          addons,
                          Dict_IO.Count ((tackons (i).MNPC)));
                        return;                 --  Be happy with one ???????
                     else
                        null;
                     end if;   --  TACKON_HIT
                  end if;                       --  we have a possible word
               end if;                                     --  LESS is less
            end remove_a_tackon;
         end loop loop_over_tackons;
      end try_tackons;

   begin                           --  WORD
      if Trim (Input_word) = ""  then
         return;
      end if;

      run_uniques (Input_word, unique_found, pa, pa_last);

      qu :
      declare
         pa_qstart : constant Integer := pa_last;
         pa_start : constant Integer := pa_last;
         saved_mode_array : constant mode_array := words_mode;
         qkey : Stem_Key_Type := 0;

      begin       --  QU
         tickons (number_of_tickons + 1) := null_prefix_item;
         words_mode  := (others => False);

         for i in 1 .. number_of_tickons + 1  loop
            declare
               q_word : constant String :=
                 Trim (subtract_tickon (Input_word, tickons (i)));
            begin
               pa_last := pa_qstart;
               pa (pa_last + 1) := Null_Parse_Record;
               if (i = number_of_tickons + 1)   or else
                 --  The prefix is a TICKON
                 (q_word /= Input_word)
               --  and it matches the start of INPUT_WORD
               then

                  if i <= number_of_tickons  then        --  Add to PA if
                     pa_last := pa_last + 1;
                     --  So add prefix line to parse array
                     pa (pa_last).Stem := Head (tickons (i).fix, Max_Stem_Size);
                     pa (pa_last).IR := ((Prefix, Null_Prefix_Record),
                       0, null_ending_record, x, x);
                     pa (pa_last).D_K  := addons;
                     pa (pa_last).MNPC := Dict_IO.Count (tickons (i).MNPC);
                  end if;

                  if q_word'Length >= 3   and then   --  qui is shortest QU_PRON
                    ((q_word (q_word'First .. q_word'First + 1) = "qu")  or
                    (q_word (q_word'First .. q_word'First + 1) = "cu"))
                  then
                     if q_word (q_word'First .. q_word'First + 1) = "qu"  then
                        qkey := 1;
                        process_qu_pronouns (q_word, qkey);
                     elsif q_word
                       (q_word'First .. q_word'First + 1) = "cu"
                     then
                        qkey := 2;
                        process_qu_pronouns (q_word, qkey);
                     end if;
                     if pa_last <= pa_qstart + 1 and then qkey > 0 then
                        --  If did not find a PACKON
                        if q_word
                          (q_word'First .. q_word'First + 1) = "qu"
                        then
                           process_packons (q_word);
                        elsif q_word
                          (q_word'First .. q_word'First + 1) = "cu"
                        then
                           process_packons (q_word);
                        end if;
                     else
                        exit;
                     end if;
                     if pa_last > pa_qstart + 1  then
                        exit;
                     end if;

                  elsif Input_word'Length >= 6  then   --  aliqui as aliQU_PRON
                     if Input_word
                       (Input_word'First .. Input_word'First + 4) = "aliqu"
                     then
                        process_qu_pronouns (Input_word, 1);
                     elsif Input_word
                       (Input_word'First .. Input_word'First + 4) = "alicu"
                     then
                        process_qu_pronouns (Input_word, 2);
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
      run_inflections (Input_word, ss);
      prune_stems (Input_word, ss, sss);
      if sss (1) /= Null_Parse_Record   then
         order_stems (sss);
         array_stems (sss, pa, pa_last);
         sss (1) := Null_Parse_Record;
      end if;
      --==========================================================

      if pa_last = pa_save  then
         try_tackons (Input_word);
      end if;
   exception
      when Storage_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output,
           "STORAGE_ERROR exception in WORD while processing =>"
           & raw_word);
         pa_last := pa_save;
         if words_mode (Write_unknowns_to_file)  then
            Ada.Text_IO.Put (unknowns, raw_word);
            Ada.Text_IO.Set_Col (unknowns, 21);
            Ada.Text_IO.Put_Line (unknowns, "========   STORAGE_ERROR  ");
         end if;
      when others =>
         if words_mode (Write_unknowns_to_file)  then
            Ada.Text_IO.Put (unknowns, raw_word);
            Ada.Text_IO.Set_Col (unknowns, 21);
            Ada.Text_IO.Put_Line (unknowns, "========   ERROR  ");
         end if;
         pa_last := pa_save;
   end word;

   procedure initialize_word_package is
   begin                                  --  Initializing WORD_PACKAGE

      establish_inflections_section;

      lel_section_io.Open (inflections_sections_file, lel_section_io.In_File,
        inflections_sections_name);

      try_to_load_dictionary (general);

      try_to_load_dictionary (special);

      load_local :
      begin
         --  First check if there is a LOC dictionary
         check_for_local_dictionary :
         declare
            dummy : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Open (dummy, Ada.Text_IO.In_File,
              add_file_name_extension (dictionary_file_name,
              "LOCAL"));
            --  Failure to OPEN will raise an exception, to be handled below
            Ada.Text_IO.Close (dummy);
         end check_for_local_dictionary;
         --  If the above does not exception out, we can load LOC
         Preface.Put ("LOCAL ");
         dict_loc := null_dictionary;
         load_dictionary (dict_loc,
           add_file_name_extension (dictionary_file_name, "LOCAL"));
         --  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
         load_stem_file (local);
         Dictionary_Available (local) := True;
      exception
         when others  =>
            Dictionary_Available (local) := False;
      end load_local;

      load_uniques (unq, uniques_full_name);

      load_addons (addons_full_name);

      load_bdl_from_disk;

      if not (Dictionary_Available (general)  or
        Dictionary_Available (special)  or
        Dictionary_Available (local))
      then
         Preface.Put_Line
           ("There are no main dictionaries - program will not do much");
         Preface.Put_Line
           ("Check that there are dictionary files in this subdirectory");
         Preface.Put_Line
           ("Except DICT.LOC that means DICTFILE, INDXFILE, STEMFILE");
      end if;

      try_to_load_english_words :
      begin
         English_Dictionary_Available (general) := False;
         ewds_direct_io.Open
           (ewds_file, ewds_direct_io.In_File, "EWDSFILE.GEN");

         English_Dictionary_Available (general) := True;
      exception
         when others  =>
            Preface.Put_Line ("No English available");
            English_Dictionary_Available (general) := False;
      end try_to_load_english_words;

   end initialize_word_package;

end word_package;
