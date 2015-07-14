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

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with word_parameters; use word_parameters;
with addons_package; use addons_package;
with uniques_package; use uniques_package;
with word_support_package; use word_support_package;
with developer_parameters; use developer_parameters;
with word_package; use word_package;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Char_Utils;
with dictionary_form;
with Put_example_line;
with list_sweep;
with Put_stat;
use Latin_Utils;
package body list_package is

   subtype xons is Part_Of_Speech_Type range Tackon .. Suffix;

   type dictionary_MNPC_record is record
      d_k  : Dictionary_Kind := Default_Dictionary_Kind;
      MNPC : MNPC_Type := Null_MNPC;
      de   : Dictionary_Entry := Null_Dictionary_Entry;
   end record;
   null_dictionary_MNPC_record : constant dictionary_MNPC_record
     := (x, Null_MNPC, Null_Dictionary_Entry);

   max_meaning_print_size : constant := 79;
   mm : Integer := Max_Meaning_Size;
   i : Integer := 0;

   inflection_frequency : constant array (Frequency_Type) of String (1 .. 8) :=
     ("        ",  --  X
     "mostfreq",  --  A
     "sometime",  --  B
     "uncommon",  --  C
     "infreq  ",  --  D
     "rare    ",  --  E
     "veryrare",  --  F
     "inscript",  --  I
     "        ",  --  Not used
     "        ");
   inflection_age : constant array (Age_Type) of String (1 .. 8) :=
     ("Always  ",   --  X
     "Archaic ",   --  A
     "Early   ",   --  B
     "Classic ",   --  C
     "Late    ",   --  D
     "Later   ",   --  E
     "Medieval",   --  F
     "Scholar ",   --  G
     "Modern  "); --  H

   dictionary_frequency : constant array (Frequency_Type) of String (1 .. 8) :=
     ("        ",  --  X
     "veryfreq",  --  A
     "frequent",  --  B
     "common  ",  --  C
     "lesser  ",  --  D
     "uncommon",  --  E
     "veryrare",  --  F
     "inscript",  --  I
     "graffiti",  --  J
     "Pliny   "); --  N

   dictionary_age : constant array (Age_Type) of String (1 .. 8) :=
     ("        ",   --  X
     "Archaic ",   --  A
     "Early   ",   --  B
     "Classic ",   --  C
     "Late    ",   --  D
     "Later   ",   --  E
     "Medieval",   --  F
     "NeoLatin",   --  G
     "Modern  "); --  H

   procedure Put_dictionary_flags (Output : Ada.Text_IO.File_Type;
                                   de     : Dictionary_Entry;
                                   hit    : out Boolean) is
   begin

      if words_mode (show_age)   or
        (Trim (dictionary_age (de.Tran.Age))'Length /= 0)  --  Not X
      then
         Ada.Text_IO.Put (Output, "  " & Trim (dictionary_age (de.Tran.Age)));
         hit := True;
      end if;
      if (words_mode (show_frequency) or
        (de.Tran.Freq >= d))  and
        (Trim (dictionary_frequency (de.Tran.Freq))'Length /= 0)
      then
         Ada.Text_IO.Put (Output, "  " &
           Trim (dictionary_frequency (de.Tran.Freq)));
         hit := True;
      end if;
   end Put_dictionary_flags;

   procedure Put_dictionary_form (Output : Ada.Text_IO.File_Type;
                                  d_k    : Dictionary_Kind;
                                  MNPC   : Dict_IO.Count;
                                  de     : Dictionary_Entry)
   is
      chit, dhit, ehit, fhit, lhit : Boolean := False; --  Things on this line?
      dictionary_line_number : constant Integer := Integer (MNPC);
   begin                               --  PUT_DICTIONARY_FORM
      if words_mode (do_dictionary_forms)  then
         if words_mdev (do_pearse_codes) then
            Ada.Text_IO.Put (Output, "02 ");
            dhit := True;
         end if;
         if dictionary_form (de)'Length /= 0  then
            Ada.Text_IO.Put (Output, dictionary_form (de) & "  ");
            dhit := True;
         end if;
      end if;

      if words_mdev (show_dictionary_codes) and then
        de.Part.pofs not in xons
      then
         Ada.Text_IO.Put (Output, " [");
         -- FIXME: Why noy Translation_Record_IO.Put ?
         Age_Type_IO.Put (Output, de.Tran.Age);
         Area_Type_IO.Put (Output, de.Tran.Area);
         Geo_Type_IO.Put (Output, de.Tran.Geo);
         Frequency_Type_IO.Put (Output, de.Tran.Freq);
         Source_Type_IO.Put (Output, de.Tran.Source);
         Ada.Text_IO.Put (Output, "]  ");
         chit := True;
      end if;

      if words_mdev (show_dictionary) then
         Ada.Text_IO.Put (Output, Ext (d_k) & ">");
         ehit := True;
      end if;

      if words_mdev (show_dictionary_line)  then
         if dictionary_line_number > 0  then
            Ada.Text_IO.Put (Output, "("
              & Trim (Integer'Image (dictionary_line_number)) & ")");
            lhit := True;
         end if;
      end if;

      Put_dictionary_flags (Output, de, fhit);

      if chit or dhit or ehit or fhit or lhit then
         Ada.Text_IO.New_Line (Output);
      end if;
      --end if;

   end Put_dictionary_form;

   procedure list_stems (configuration : configuration_type;
                         Output   : Ada.Text_IO.File_Type;
                         raw_word : String;
                         Input_Line : String;
                         pa       : in out Parse_Array;
                         pa_last  : in out Integer) is
      use Ada.Text_IO;
      use Dict_IO;

      --  The main WORD processing has been to produce an array of PARSE_RECORD
      --      type PARSE_RECORD is
      --        record
      --          STEM  : STEM_TYPE := NULL_STEM_TYPE;
      --          IR    : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      --          D_K   : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
      --          MNPC  : DICT_IO.COUNT := NULL_MNPC;
      --        end record;
      --  This has involved STEMFILE and INFLECTS, no DICTFILE

      --  PARSE_RECORD is Put through the LIST_SWEEP procedure that does TRIMing
      --  Then, for processing for Output, the data is converted to arrays of
      --      type STEM_INFLECTION_RECORD is
      --        record
      --          STEM : STEM_TYPE          := NULL_STEM_TYPE;
      --          IR   : INFLECTION_RECORD  := NULL_INFLECTION_RECORD;
      --        end record;
      --  and
      --      type DICTIONARY_MNPC_RECORD is
      --        record
      --          D_K  : DICTIONARY_KIND;
      --          MNPC : MNPC_TYPE;
      --          DE   : DICTIONARY_ENTRY;
      --        end record;
      --  containing the same data plus the DICTFILE data DICTIONARY_ENTRY
      --  but breaking it into two arrays allows different manipulation
      --  These are only within this routine, used to clean up the Output

      type Stem_Inflection_Record is
         record
            stem : Stem_Type          := Null_Stem_Type;
            ir   : Inflection_Record  := Null_Inflection_Record;
         end record;
      null_Stem_Inflection_Record : Stem_Inflection_Record;

      stem_inflection_array_size       : constant := 10;
      stem_inflection_array_array_size : constant := 40;
      type stem_inflection_array is
        array (Integer range <>) of Stem_Inflection_Record;
      type stem_inflection_array_array is array (Integer range <>)
        of stem_inflection_array (1 .. stem_inflection_array_size);

      osra : stem_inflection_array (1 .. stem_inflection_array_size)
        := (others => (Null_Stem_Type, Null_Inflection_Record));
      sra, null_sra :
        constant stem_inflection_array (1 .. stem_inflection_array_size)
        := (others => (Null_Stem_Type, Null_Inflection_Record));
      sraa : stem_inflection_array_array (1 .. stem_inflection_array_array_size)
        := (others => null_sra);
      null_sraa :
        constant stem_inflection_array_array
        (1 .. stem_inflection_array_array_size)
        := (others => null_sra);

      --      type DICTIONARY_MNPC_RECORD is record
      --        D_K  : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
      --        MNPC : MNPC_TYPE := NULL_MNPC;
      --        DE   : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
      --      end record;
      --      NULL_DICTIONARY_MNPC_RECORD : DICTIONARY_MNPC_RECORD
      --                            := (X, NULL_MNPC, NULL_DICTIONARY_ENTRY);
      dm, odm : dictionary_MNPC_record := null_dictionary_MNPC_record;

      dictionary_MNPC_array_size : constant := 40;

      type dictionary_MNPC_array is array (1 .. dictionary_MNPC_array_size)
        of dictionary_MNPC_record;
      dma, null_dma : dictionary_MNPC_array;

      --MEANING_ARRAY_SIZE : constant := 5;
      --MEANING_ARRAY : array (1 .. MEANING_ARRAY_SIZE) of MEANING_TYPE;

      dea : Dictionary_Entry := Null_Dictionary_Entry;

      w : constant String := raw_word;
      j, j1, j2, k : Integer := 0;
      there_is_an_adverb : Boolean := False;

      procedure  Put_inflection (sr : Stem_Inflection_Record;
                                 dm : dictionary_MNPC_record) is
         --  Handles Putting ONLY_MEAN, PEARSE_CODES, CAPS, QUAL, V_KIND, FLAGS
         procedure Put_inflection_flags is
         begin
            if (words_mode (show_age)   or
              (sr.ir.age /= x))  and     --  Warn even if not to show AGE
              Trim (inflection_age (sr.ir.age))'Length /= 0
            then
               Ada.Text_IO.Put (Output, "  " & inflection_age (sr.ir.age));
            end if;
            if (words_mode (show_frequency)  or
              (sr.ir.freq >= c))  and    --  Warn regardless
              Trim (inflection_frequency (sr.ir.freq))'Length /= 0
            then
               Ada.Text_IO.Put (Output, "  " &
                 inflection_frequency (sr.ir.freq));
            end if;
         end Put_inflection_flags;

      begin
         --TEXT_IO.PUT_LINE ("PUT_INFLECTION ");
         if not words_mode (do_only_meanings) and
           not (configuration = only_meanings)
         then
            Ada.Text_IO.Set_Col (Output, 1);
            if words_mdev (do_pearse_codes) then
               if dm.d_k = addons  then
                  Ada.Text_IO.Put (Output, "05 ");
               elsif dm.d_k in xxx .. yyy  then
                  Ada.Text_IO.Put (Output, "06 ");
               else
                  Ada.Text_IO.Put (Output, "01 ");
               end if;
            end if;

            --TEXT_IO.PUT (OUTPUT, CAP_STEM (TRIM (SR.STEM)));
            Ada.Text_IO.Put (Output, (Trim (sr.stem)));
            if sr.ir.ending.size > 0  then
               Ada.Text_IO.Put (Output, ".");
               --TEXT_IO.PUT (OUTPUT, TRIM (CAP_ENDING (SR.IR.ENDING.SUF)));
               Ada.Text_IO.Put (Output, Trim ((sr.ir.ending.suf)));
            end if;

            if words_mdev (do_pearse_codes) then
               Ada.Text_IO.Set_Col (Output, 25);
            else
               Ada.Text_IO.Set_Col (Output, 22);
            end if;

            if sr.ir /= Null_Inflection_Record  then

               print_modified_qual :
               declare
                  out_String : String (1 .. quality_record_io.Default_Width);
                  passive_start  : constant Integer :=
                    Part_Of_Speech_Type_IO.Default_Width + 1 +
                    Decn_Record_IO.Default_Width + 1 +
                    Tense_Type_IO.Default_Width + 1;
                  passive_finish : constant Integer :=
                    passive_start +
                    Voice_Type_IO.Default_Width;
                  ppl_start      : constant Integer :=
                    Part_Of_Speech_Type_IO.Default_Width + 1 +
                    Decn_Record_IO.Default_Width + 1 +
                    Case_Type_IO.Default_Width + 1 +
                    Number_Type_IO.Default_Width + 1 +
                    Gender_Type_IO.Default_Width + 1 +
                    Tense_Type_IO.Default_Width + 1;
                  ppl_finish : constant Integer :=
                    ppl_start +
                    Voice_Type_IO.Default_Width;
                  passive_blank :
                    constant String (1 .. Voice_Type_IO.Default_Width) :=
                    (others => ' ');
               begin

                  quality_record_io.Put (out_String, sr.ir.qual);
                  if dm.d_k in general .. local then  --  UNIQUES has no DE

                     if (sr.ir.qual.pofs = V)    and then
                       (dm.de.Part.V.Kind = Dep)       and then
                       (sr.ir.qual.V.tense_voice_mood.Mood in Ind .. Inf)
                     then
                        --TEXT_IO.PUT_LINE ("START PRINT MODIFIED QUAL   V");
                        out_String (passive_start + 1 .. passive_finish) :=
                          passive_blank;
                     elsif (sr.ir.qual.pofs = Vpar)    and then
                       (dm.de.Part.V.Kind = Dep)    and then
                       (sr.ir.qual.Vpar.tense_voice_mood.Mood = Ppl)
                     then
                        --TEXT_IO.PUT_LINE ("START PRINT MODIFIED QUAL   VPAR");
                        out_String (ppl_start + 1 .. ppl_finish) :=
                          passive_blank;
                     end if;
                  end if;

                  Ada.Text_IO.Put (Output, out_String);
                  --TEXT_IO.PUT_LINE ("PRINT MODIFIED QUAL 4");
               end print_modified_qual;

               --               if ((SR.IR.QUAL.POFS = NUM)  and
               --                          -- Don't want on inflection
               --                   (DM.D_K in GENERAL .. UNIQUE))  and then
               --                   (DM.DE.KIND.NUM_VALUE > 0)  then
               --                 TEXT_IO.PUT (OUTPUT, "  ");
               --                 INFLECTIONS_PACKAGE.INTEGER_IO.PUT
               --                    (OUTPUT, DM.DE.KIND.NUM_VALUE);
               --               end if;
               Put_inflection_flags;
               Ada.Text_IO.New_Line (Output);
               Put_example_line (configuration, Output, sr.ir, dm.de);
               --  Only full when DO_EXAMPLES
            else
               Ada.Text_IO.New_Line (Output);
            end if;
         end if;
      end Put_inflection;

      procedure Put_form (sr : Stem_Inflection_Record;
                          dm : dictionary_MNPC_record) is
         --  Handles PEARSE_CODES and DICTIONARY_FORM (which has FLAGS) and D_K
         --  The Pearse 02 is handled in PUT_DICTIONARY_FORM
      begin
         if (sr.ir.qual.pofs not in xons)  and
           (dm.d_k in general .. unique)
         then
            --DICTIONARY_ENTRY_IO.PUT (DM.DE);
            Put_dictionary_form (Output, dm.d_k, dm.MNPC, dm.de);
         end if;
      end Put_form;

      function Trim_bar (s : String) return String is
         --  Takes vertical bars from begining of MEAN and TRIMs
      begin
         if s'Length > 3  and then s (s'First .. s'First + 3) = "||||"  then
            return Trim (s (s'First + 4 .. s'Last));
         elsif s'Length > 2  and then s (s'First .. s'First + 2) = "|||"  then
            return Trim (s (s'First + 3 .. s'Last));
         elsif s'Length > 1  and then  s (s'First .. s'First + 1) = "||"  then
            return Trim (s (s'First + 2 .. s'Last));
         elsif s (s'First) = '|'  then
            return Trim (s (s'First + 1 .. s'Last));
         else
            return Trim (s);
         end if;
      end Trim_bar;

      procedure Put_meaning (Output : Ada.Text_IO.File_Type;
                             raw_meaning : String) is
         --  Handles the MM screen line limit and TRIM_BAR, then TRIMs
      begin
         Ada.Text_IO.Put (Output, Trim (Head (Trim_bar (raw_meaning), mm)));
      end Put_meaning;

      function constructed_meaning
        (sr : Stem_Inflection_Record;
         dm  : dictionary_MNPC_record)
        return String
      is
         --  Constructs the meaning for NUM from NUM.SORT and NUM_VALUE
         s : String (1 .. Max_Meaning_Size) := Null_Meaning_Type;
         n : Integer := 0;
      begin
         if dm.de.Part.pofs = Num  then
            n := dm.de.Part.Num.Value;
            if sr.ir.qual.pofs = Num  then    --  Normal parse
               case sr.ir.qual.Num.Sort is
                  when Card  =>
                     s := Head (Integer'Image (n) &
                       " - (CARD answers 'how many');", Max_Meaning_Size);
                  when Ord   =>
                     s := Head (Integer'Image (n) &
                       "th - (ORD, 'in series'); (a/the)" & Integer'Image (n) &
                       "th (part) (fract w/pars?);", Max_Meaning_Size);
                  when Dist  =>
                     s := Head (Integer'Image (n) &
                       " each/apiece/times/fold/toGether/at a time" &
                       " - 'how many each'; by " &
                       Integer'Image (n) & "s; ", Max_Meaning_Size);
                  when Adverb =>
                     s := Head (Integer'Image (n) &
                       " times, on" & Integer'Image (n) &
                       " occasions - (ADVERB answers 'how often');",
                       Max_Meaning_Size);
                  when others =>
                     null;
               end case;
            else  -- there is fix so POFS is not NUM
               s := Head ("Number " & Integer'Image (n), Max_Meaning_Size);
            end if;
         end if;

         return s;
      end constructed_meaning;

      procedure Put_meaning_line (sr : Stem_Inflection_Record;
                                  dm  : dictionary_MNPC_record) is
      begin
         if dm.d_k not in addons .. ppp  then
            if words_mdev (do_pearse_codes) then
               Ada.Text_IO.Put (Output, "03 ");
            end if;
            if dm.de.Part.pofs = Num  and then dm.de.Part.Num.Value > 0  then
               Ada.Text_IO.Put_Line (Output, constructed_meaning (sr, dm));
               --  Constructed MEANING
            elsif dm.d_k = unique  then
               Put_meaning (Output, uniques_de (dm.MNPC).Mean);
               Ada.Text_IO.New_Line (Output);
            else
               Put_meaning (Output, Trim_bar (dm.de.Mean));
               Ada.Text_IO.New_Line (Output);
            end if;
         else
            if dm.d_k = rrr  then
               if rrr_meaning /= Null_Meaning_Type   then
                  --PUT_DICTIONARY_FLAGS;
                  if words_mdev (do_pearse_codes) then
                     Ada.Text_IO.Put (Output, "03 ");
                  end if;
                  Put_meaning (Output, rrr_meaning);      --  Roman Numeral
                  rrr_meaning := Null_Meaning_Type;
                  Ada.Text_IO.New_Line (Output);
               end if;
            elsif dm.d_k = nnn then
               if nnn_meaning /= Null_Meaning_Type  then
                  --PUT_DICTIONARY_FLAGS;
                  if words_mdev (do_pearse_codes) then
                     Ada.Text_IO.Put (Output, "03 ");
                  end if;
                  Put_meaning (Output, nnn_meaning);  --  Unknown Name
                  nnn_meaning := Null_Meaning_Type;
                  Ada.Text_IO.New_Line (Output);
               end if;
            elsif dm.d_k = xxx  then
               if xxx_meaning /= Null_Meaning_Type  then
                  if words_mdev (do_pearse_codes) then
                     Ada.Text_IO.Put (Output, "06 ");
                  end if;
                  Put_meaning (Output, xxx_meaning);  --  TRICKS
                  xxx_meaning := Null_Meaning_Type;
                  Ada.Text_IO.New_Line (Output);
               end if;
            elsif dm.d_k = yyy  then
               if yyy_meaning /= Null_Meaning_Type  then
                  if words_mdev (do_pearse_codes) then
                     Ada.Text_IO.Put (Output, "06 ");
                  end if;
                  Put_meaning (Output, yyy_meaning);  --  Syncope
                  yyy_meaning := Null_Meaning_Type;
                  Ada.Text_IO.New_Line (Output);
               end if;
            elsif dm.d_k = ppp  then
               if ppp_meaning /= Null_Meaning_Type  then
                  if words_mdev (do_pearse_codes) then
                     Ada.Text_IO.Put (Output, "06 ");
                  end if;
                  Put_meaning (Output, ppp_meaning); --  Compounds
                  ppp_meaning := Null_Meaning_Type;
                  Ada.Text_IO.New_Line (Output);
               end if;
            elsif dm.d_k = addons  then
               if words_mdev (do_pearse_codes) then
                  Ada.Text_IO.Put (Output, "06 ");
               end if;
               Put_meaning (Output, means (Integer (dm.MNPC)));
               Ada.Text_IO.New_Line (Output);
            end if;
         end if;
      end Put_meaning_line;

   begin
      Trimmed := False;

      --  Since this procedure weeds out possible parses, if it weeds out all
      --  (or all of a class) it must fix up the rest of the parse array,
      --  e.g., it must clean out dangling prefixes and suffixes

      if Ada.Text_IO.Name (Output) =
        Ada.Text_IO.Name (Ada.Text_IO.Standard_Output)
      then
         --  to keep from overflowing screen line or even adding blank line
         mm := max_meaning_print_size;
      else
         mm := Max_Meaning_Size;
      end if;

      -------  The gimick of adding an ADV if there is only ADJ VOC  ----
      --TEXT_IO.PUT_LINE ("About to do the ADJ -> ADV kludge");
      for i in pa'First .. pa_last  loop
         if pa (i).IR.qual.pofs = Adv   then
            there_is_an_adverb := True;
            exit;
         end if;
      end loop;

      if (not there_is_an_adverb) and (words_mode (do_fixes))  then
         --TEXT_IO.PUT_LINE ("In the ADJ -> ADV kludge  There is no ADV");
         for i in reverse pa'First .. pa_last  loop
            if pa (i).IR.qual.pofs = Adj and then
              (pa (i).IR.qual.Adj = ((1, 1), Voc, S, M, Pos)    or
              ((pa (i).IR.qual.Adj.Of_Case = Voc)   and
              (pa (i).IR.qual.Adj.Number = S)   and
              (pa (i).IR.qual.Adj.Gender = M)   and
              (pa (i).IR.qual.Adj.Comparison = Super)))
            then
               j := i;

               while j >=  pa'First  loop  --Back through other ADJ cases
                  if pa (j).IR.qual.pofs /= Adj  then
                     j2 := j;
                     --  J2 is first (reverse) that is not ADJ
                     exit;
                  end if;
                  j := j - 1;
               end loop;
               while j >=  pa'First  loop  --  Sweep up associated fixes
                  if pa (j).IR.qual.pofs not in xons  then
                     j1 := j;
                     --  J1 is first (reverse) that is not XONS
                     exit;
                  end if;
                  j := j - 1;
               end loop;

               for j in j1 + 1 .. j2  loop
                  pa (pa_last + j - j1 + 1) := pa (j);
               end loop;

               pa_last := pa_last + j2 - j1 + 1;
               pa (pa_last) := pa (j2 + 1);

               pa (pa_last) := ("e                 ",
                 ((Suffix, null_suffix_record), 0, null_ending_record, x, b),
                 ppp, Null_MNPC);
               --PARSE_RECORD_IO.PUT (PA (PA_LAST)); TEXT_IO.NEW_LINE;
               pa_last := pa_last + 1;
               if pa (j2 + 1).IR.qual.Adj.Comparison = Pos   then

                  pa (pa_last) := (pa (j2 + 1).Stem,
                    ((pofs => Adv,
                      adv => (Comparison =>
                              pa (j2 + 1).IR.qual.Adj.Comparison)),
                    key => 0, ending => (1, "e      "), age => x, freq => b),
                    pa (j2 + 1).D_K,
                    pa (j2 + 1).MNPC);
                  --PARSE_RECORD_IO.PUT (PA (PA_LAST)); TEXT_IO.NEW_LINE;
                  ppp_meaning :=
                    Head ("-ly; -ily;  Converting ADJ to ADV",
                    Max_Meaning_Size);

               elsif pa (j2 + 1).IR.qual.Adj.Comparison = Super  then
                  pa (pa_last) := (pa (j2 + 1).Stem,
                    ((pofs => Adv,
                      adv => (Comparison =>
                              pa (j2 + 1).IR.qual.Adj.Comparison)),
                    key => 0, ending => (2, "me     "), age => x, freq => b),
                    pa (j2 + 1).D_K,
                    pa (j2 + 1).MNPC);
                  ppp_meaning :=
                    Head ("-estly; -estily; most -ly, very -ly" &
                    "  Converting ADJ to ADV",
                    Max_Meaning_Size);
               end if;
            end if;           --  PA (I).IR.QUAL.POFS = ADJ
         end loop;
      end if;           --  not THERE_IS_AN_ADVERB

      list_sweep (pa (1 .. pa_last), pa_last);

      if  words_mdev (Write_statistics_file)    then
         --  Omit rest of Output
         for i in 1 .. pa_last  loop                       --  Just to PUT_STAT
            if pa (i).D_K = addons then
               if pa (i).IR.qual.pofs = Prefix  then
                  Put_stat ("ADDON PREFIX at "
                    & Head (Integer'Image (line_number), 8) &
                    Head (Integer'Image (word_number), 4)
                    & "   " & Head (w, 20) & "   "  & pa (i).Stem &
                    "  " & Integer'Image (Integer (pa (i).MNPC)));
               elsif pa (i).IR.qual.pofs = Suffix  then
                  Put_stat ("ADDON SUFFIX at "
                    & Head (Integer'Image (line_number), 8) &
                    Head (Integer'Image (word_number), 4)
                    & "   " & Head (w, 20) & "   "  & pa (i).Stem &
                    "  " & Integer'Image (Integer (pa (i).MNPC)));
               elsif pa (i).IR.qual.pofs = Tackon  then
                  Put_stat ("ADDON TACKON at "
                    & Head (Integer'Image (line_number), 8) &
                    Head (Integer'Image (word_number), 4)
                    & "   " & Head (w, 20) & "   "  & pa (i).Stem &
                    "  " & Integer'Image (Integer (pa (i).MNPC)));
               end if;
            end if;
         end loop;
      end if;

      --  Convert from PARSE_RECORDs to DICTIONARY_MNPC_RECORD
      ---   and STEM_INFLECTION_RECORD
      i := 1;           --  I cycles on PA
      j := 0;           --  J indexes the number of DMA arrays  --  Initialize
      sraa := null_sraa;
      dma := null_dma;

      cycle_over_pa :
      while i <= pa_last  loop
         --  I cycles over full PA array

         --TEXT_IO.PUT_LINE ("Starting loop for I    I = " & INTEGER'IMAGE (I));
         odm := null_dictionary_MNPC_record;

         if pa (i).D_K = unique  then
            j := j + 1;
            sraa (j)(1) := (pa (i).Stem, pa (i).IR);

            dm := null_dictionary_MNPC_record;
            dm.d_k := unique;
            dm.MNPC := pa (i).MNPC;
            dm.de := uniques_de (pa (i).MNPC);
            dma (j) := dm;
            i := i + 1;
         else
            case pa (i).IR.qual.pofs  is
               when N =>
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while (pa (i).IR.qual.pofs = N) and (i <= pa_last) loop

                     if pa (i).MNPC  /= odm.MNPC  then
                        --  Encountering new MNPC
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize

                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA

                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        Dict_IO.Set_Index (Dict_File (pa (i).D_K), pa (i).MNPC);
                        Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     else
                        k := k + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                     end if;

                     i := i + 1;              --  I cycles over full PA array
                  end loop;

               when Pron =>
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while pa (i).IR.qual.pofs = Pron   and
                    i <= pa_last                   loop
                     if pa (i).MNPC  /= odm.MNPC  then
                        --  Encountering new MNPC
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize
                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        Dict_IO.Set_Index (Dict_File (pa (i).D_K), pa (i).MNPC);
                        Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     else
                        k := k + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                     end if;

                     i := i + 1;              --  I cycles over full PA array
                  end loop;

               when Pack =>
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while pa (i).IR.qual.pofs = Pack and i <= pa_last loop
                     if pa (i).MNPC  /= odm.MNPC  then
                        --  Encountering new MNPC
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize
                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        Dict_IO.Set_Index (Dict_File (pa (i).D_K), pa (i).MNPC);
                        Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     else
                        k := k + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                     end if;

                     i := i + 1;              --  I cycles over full PA array
                  end loop;

               when Adj =>
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while pa (i).IR.qual.pofs = Adj and i <= pa_last loop
                     --TEXT_IO.PUT_LINE ("SRAA - ADJ");
                     if pa (i).MNPC  /= odm.MNPC  then
                        --  Encountering new MNPC
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize
                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        Dict_IO.Set_Index (Dict_File (pa (i).D_K), pa (i).MNPC);
                        Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     else
                        k := k + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                     end if;
                     --TEXT_IO.PUT_LINE ("SRAA  + ADJ");
                     i := i + 1;              --  I cycles over full PA array
                  end loop;

               when Num  =>
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while pa (i).IR.qual.pofs = Num   and
                    i <= pa_last                   loop
                     if pa (i).D_K = rrr then        --  Roman numeral
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize
                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);

                        dea := Null_Dictionary_Entry;
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     elsif pa (i).MNPC /= odm.MNPC then
                        --  Encountering new MNPC
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize
                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        Dict_IO.Set_Index (Dict_File (pa (i).D_K), pa (i).MNPC);
                        Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     else
                        k := k + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                     end if;

                     i := i + 1;              --  I cycles over full PA array
                  end loop;

               when V | Vpar | Supine  =>
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while (pa (i).IR.qual.pofs = V      or
                    pa (i).IR.qual.pofs = Vpar   or
                    pa (i).IR.qual.pofs = Supine)   and
                    i <= pa_last                   loop
                     if (pa (i).MNPC  /= odm.MNPC) and
                       (pa (i).D_K /= ppp)
                     then   --  Encountering new MNPC
                        osra := sra;  --  But not for compound
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize

                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA

                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        if pa (i).D_K /= ppp  then
                           Dict_IO.Set_Index
                             (Dict_File (pa (i).D_K), pa (i).MNPC);
                           Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                        end if;     --  use previous DEA
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                     else
                        k := k + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                     end if;

                     i := i + 1;              --  I cycles over full PA array
                  end loop;

               when others  =>
                  --TEXT_IO.PUT_LINE ("Others");
                  osra := null_sra;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while i <= pa_last                   loop
                     if (odm.d_k  /= pa (i).D_K)  or
                       (odm.MNPC /= pa (i).MNPC)
                     then   --  Encountering new single (K only 1)
                        osra := sra;
                        k := 1;
                        --  K indexes within the MNPCA array --  Initialize

                        j := j + 1;
                        --  J indexes the number of MNPCA arrays - Next MNPCA

                        sraa (j)(k) := (pa (i).Stem, pa (i).IR);
                        if pa (i).MNPC /= Null_MNPC  then
                           if pa (i).D_K = addons  then
                              dea :=  Null_Dictionary_Entry;
                              --  Fix for ADDONS in MEANS, not DICT_IO
                           else
                              Dict_IO.Set_Index (Dict_File (pa (i).D_K),
                                pa (i).MNPC);
                              Dict_IO.Read (Dict_File (pa (i).D_K), dea);
                           end if;
                        else                       --  Has no dictionary to read
                           dea := Null_Dictionary_Entry;
                        end if;
                        dm := (pa (i).D_K, pa (i).MNPC, dea);
                        dma (j) := dm;
                        odm := dm;
                        --else
                        --  K := K + 1;
                        --  K indexes within the MNPCA array  - Next MNPC
                        --  SRAA (J)(K) := (PA (I).STEM, PA (I).IR);
                     end if;

                     i := i + 1;              --  I cycles over full PA array
                     exit;
                     --  Since Other is only one, don't loop
                  end loop;

            end case;

         end if;

      end loop cycle_over_pa;

      --  Sets + if capitalized
      --  Strangely enough, it may enter LIST_STEMS with PA_LAST /= 0
      --  but be weeded and end up with no parse after
      --                    LIST_SWEEP  -  PA_LAST = 0
      if pa_last = 0  then
         --  WORD failed
         if words_mode (ignore_unknown_names)  and capitalized  then
            nnn_meaning := Head (
              "Assume this is capitalized proper name/abbr," &
              " under MODE IGNORE_UNKNOWN_NAME ",
              Max_Meaning_Size);
            pa (1) := (Head (raw_word, Max_Stem_Size),
              ((N, ((0, 0), X, X, X)), 0, null_ending_record, x, x),
              nnn, Null_MNPC);
            pa_last := 1;    --  So LIST_NEIGHBORHOOD will not be called
            sraa := null_sraa;
            dma := null_dma;
            sraa (1)(1) := (pa (1).Stem, pa (1).IR);
            dma (1) := (nnn, 0, Null_Dictionary_Entry);
         elsif  words_mode (ignore_unknown_caps)  and all_caps  then
            nnn_meaning := Head (
              "Assume this is capitalized proper name/abbr," &
              " under MODE IGNORE_UNKNOWN_CAPS ",
              Max_Meaning_Size);
            pa (1) := (Head (raw_word, Max_Stem_Size),
              ((N, ((0, 0), X, X, X)), 0, null_ending_record, x, x),
              nnn, Null_MNPC);
            pa_last := 1;
            sraa := null_sraa;
            dma := null_dma;
            sraa (1)(1) := (pa (1).Stem, pa (1).IR);
            dma (1) := (nnn, 0, Null_Dictionary_Entry);
         end if;
      end if;

      if pa_last = 0   then

         if  words_mode (Write_Output_to_file)      then
            if words_mdev (do_pearse_codes) then
               Ada.Text_IO.Put (Output, "04 ");
            end if;
            Ada.Text_IO.Put (Output, raw_word);
            Ada.Text_IO.Set_Col (Output, 30);
            Inflections_Package.Integer_IO.Put (Output, line_number, 7);
            Inflections_Package.Integer_IO.Put (Output, word_number, 7);
            Ada.Text_IO.Put_Line (Output, "    ========   UNKNOWN    ");
            --TEXT_IO.NEW_LINE (OUTPUT);
         else              --  Just screen Output
            if words_mdev (do_pearse_codes) then
               Ada.Text_IO.Put ("04 ");
            end if;
            Ada.Text_IO.Put (raw_word);
            Ada.Text_IO.Set_Col (30);
            Ada.Text_IO.Put_Line ("    ========   UNKNOWN    ");
            --TEXT_IO.NEW_LINE;
         end if;

         if words_mode (Write_unknowns_to_file)  then
            if words_mdev (include_unknown_context) or
              words_mdev (do_only_initial_word)
            then
               Ada.Text_IO.Put_Line (Input_Line);
               Ada.Text_IO.Put_Line (unknowns, Input_Line);
            end if;
            if words_mdev (do_pearse_codes) then
               Ada.Text_IO.Put (unknowns, "04 ");
            end if;
            Ada.Text_IO.Put (unknowns, raw_word);
            Ada.Text_IO.Set_Col (unknowns, 30);
            Inflections_Package.Integer_IO.Put (unknowns, line_number, 7);
            Inflections_Package.Integer_IO.Put (unknowns, word_number, 7);
            Ada.Text_IO.Put_Line (unknowns, "    ========   UNKNOWN    ");
         end if;
      end if;

      if pa_last = 0   then
         if words_mode (do_stems_for_unknown)   then
            if  words_mode (Write_Output_to_file)  and then
              not words_mode (Write_unknowns_to_file)
            then
               list_neighborhood (Output, raw_word);
            elsif  words_mode (Write_Output_to_file)  and then
              words_mode (Write_unknowns_to_file)
            then
               list_neighborhood (Output, raw_word);
               list_neighborhood (unknowns, raw_word);
            elsif Name (Current_Input) = Name (Standard_Input) then
               list_neighborhood (Output, raw_word);
            end if;
         end if;
      end if;

      if pa_last = 0 then
         if words_mdev (update_local_dictionary)  and
           -- Don't if reading from file
           (Name (Current_Input) = Name (Standard_Input))
         then
            update_local_dictionary_file;
            word (raw_word, pa, pa_last);
            --  Circular if you dont update!!!!!
         end if;
      end if;

      --  Exit if UNKNOWNS ONLY (but had to do STATS above)
      if  words_mode (do_unknowns_only)    then      --  Omit rest of Output
         return;
      end if;

      --TEXT_IO.PUT_LINE ("PUTting INFLECTIONS");
      j := 1;
      osra := null_sra;

      Output_loop :
      while  dma (j) /= null_dictionary_MNPC_record  loop
         --  Skips one identical SRA no matter what comes next
         if sraa (j) /= osra  then

            Put_inflection_array_j :
            for k in sraa (j)'Range loop
               exit Put_inflection_array_j when sraa (j)(k) =
                 null_Stem_Inflection_Record;

               Put_inflection (sraa (j)(k), dma (j));
               if sraa (j)(k).stem (1 .. 3) = "PPL"  then
                  Ada.Text_IO.Put_Line (Output, Head (ppp_meaning, mm));
               end if;
            end loop Put_inflection_array_j;
            osra := sraa (j);
         end if;

         --TEXT_IO.PUT_LINE ("PUTting FORM");

         Putting_form :
         begin
            if j = 1  or else
              dictionary_form (dma (j).de) /= dictionary_form (dma (j - 1).de)
            then
               --  Put at first chance, skip duplicates
               Put_form (sraa (j)(1), dma (j));
            end if;
         end Putting_form;

         --TEXT_IO.PUT_LINE ("PUTting MEANING");
         Putting_meaning :
         begin
            if dma (j).d_k in general .. unique then
               if dma (j).de.Mean /= dma (j + 1).de.Mean then
                  --  This if handles simple multiple MEAN with same IR and FORM
                  --  by anticipating duplicates and waiting until change
                  Put_meaning_line (sraa (j)(1), dma (j));
               end if;
            else
               Put_meaning_line (sraa (j)(1), dma (j));
            end if;
         end Putting_meaning;

         do_pause :
         begin
            if i = pa_last  then
               Ada.Text_IO.New_Line (Output);
            elsif Integer (Ada.Text_IO.Line (Output)) >
              scroll_line_number + Output_screen_size
            then
               pause (Output);
               scroll_line_number := Integer (Ada.Text_IO.Line (Output));
            end if;
         end do_pause;

         j := j + 1;
      end loop Output_loop;
      --TEXT_IO.PUT_LINE ("Finished OUTPUT_LOOP");

      if Trimmed then
         Put (Output, '*');
      end if;
      Ada.Text_IO.New_Line (Output);

   exception
      when others  =>
         Ada.Text_IO.Put_Line
           ("Unexpected exception in LIST_STEMS processing " & raw_word);
         Put_stat ("EXCEPTION LS at "
           & Head (Integer'Image (line_number), 8) &
             Head (Integer'Image (word_number), 4)
           & "   " & Head (w, 20) & "   "  & pa (i).Stem);
   end list_stems;

   procedure list_entry (Output   : Ada.Text_IO.File_Type;
                         d_k      : Dictionary_Kind;
                         mn       : Dict_IO.Count) is
      de : Dictionary_Entry;
   begin
      Dict_IO.Read (Dict_File (d_k), de, mn);
      Ada.Text_IO.Put (Output, "=>  ");
      --TEXT_IO.PUT_LINE (OUTPUT, DICTIONARY_FORM (DE));
      Put_dictionary_form (Output, d_k, mn, de);
      Ada.Text_IO.Put_Line (Output,
        Trim (Head (de.Mean, mm)));  --  so it wont line wrap/Put CR

   end list_entry;

   procedure unknown_search (unknown       :  in String;
                             unknown_count : out Dict_IO.Count) is

      use stem_io;

      d_k : constant Dictionary_Kind := general;
      j, j1, j2, jj : stem_io.Count := 0;

      index_on : constant String := unknown;
      index_first, index_last : stem_io.Count := 0;
      ds : dictionary_stem;
      first_try, second_try : Boolean := True;

      function first_two (w : String) return String is
         --  'v' could be represented by 'u'
         --  like the new Oxford Latin Dictionary
         --  Fixes the first two letters of a word/stem which can be done right
         s : constant String := Lower_Case (w);
         ss : String (w'Range) := w;

      begin
         if s'Length = 1  then
            ss (s'First) := Char_Utils.V_To_U_And_J_To_I (w (s'First));
         else
            ss (s'First)   := Char_Utils.V_To_U_And_J_To_I (w (s'First));
            ss (s'First + 1) := Char_Utils.V_To_U_And_J_To_I (w (s'First + 1));
         end if;
         return ss;
      end first_two;

   begin

      if Dictionary_Available (d_k)  then
         if not Is_Open (stem_file (d_k))  then
            Open (stem_file (d_k), stem_io.In_File,
              add_file_name_extension (stem_file_name,
              Dictionary_Kind'Image (d_k)));
         end if;

         index_first := first_index (first_two (index_on), d_k);
         index_last  := last_index (first_two (index_on), d_k);

         if index_first > 0  and then index_first <= index_last then

            j1 := index_first;    --######################
            j2 := index_last;

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

               if  ltu (Lower_Case (ds.stem), unknown)  then
                  j1 := j;
                  j := (j1 + j2) / 2;
               elsif  gtu (Lower_Case (ds.stem), unknown)  then
                  j2 := j;
                  j := (j1 + j2) / 2;
               else
                  for i in reverse j1 .. j  loop
                     Set_Index (stem_file (d_k), stem_io.Count (i));
                     Read (stem_file (d_k), ds);

                     if equ (Lower_Case (ds.stem), unknown)  then
                        jj := i;

                     else
                        exit;
                     end if;
                  end loop;

                  for i in j + 1 .. j2  loop
                     Set_Index (stem_file (d_k), stem_io.Count (i));
                     Read (stem_file (d_k), ds);

                     if equ (Lower_Case (ds.stem), unknown)  then
                        jj := i;

                     else
                        exit binary_search;
                     end if;
                  end loop;
                  exit binary_search;

               end if;
            end loop binary_search;
            j1 := jj;
            j2 := index_last;

         end if;
         unknown_count := ds.MNPC;

         Close (stem_file (d_k));  --??????
      end if;
      --TEXT_IO.PUT_LINE ("Leaving LIST_NEIGHBORHOOD    UNKNOWN_SEARCH");
   end unknown_search;

   procedure list_neighborhood (Output : Ada.Text_IO.File_Type;
                                Input_word : String) is

      d_k : constant Dictionary_Kind := general;
      unk_MNPC : Dict_IO.Count;

   begin
      --TEXT_IO.PUT_LINE ("Entering LIST_NEIGHBORHOOD");

      if Ada.Text_IO.Name (Output) =
        Ada.Text_IO.Name (Ada.Text_IO.Standard_Output)
      then
         mm := max_meaning_print_size;
         --  to keep from overflowing screen line
      else
         mm := Max_Meaning_Size;
      end if;

      unknown_search (Head (Input_word, Max_Stem_Size), unk_MNPC);
      --TEXT_IO.PUT_LINE ("UNK_MNPC = " & INTEGER'IMAGE (INTEGER (UNK_MNPC)));
      if Integer (unk_MNPC) > 0  then
         Ada.Text_IO.Put_Line (Output,
           "----------  " &
           "Entries in GENEAL Dictionary around the UNKNOWN" &
           "  ----------");
         pause (Output);
         for mn in Dict_IO.Count (Integer (unk_MNPC) - 5) ..
           Dict_IO.Count (Integer (unk_MNPC) + 3)  loop
            list_entry (Output, d_k, mn);

         end loop;
      end if;

      --TEXT_IO.PUT_LINE ("Leaving LIST_NEIGHBORHOOD");

   end list_neighborhood;

end list_package;
