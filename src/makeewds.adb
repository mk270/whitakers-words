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

with Text_IO;
with Strings_Package; use Strings_Package;
with Latin_File_Names; use Latin_File_Names;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
with english_support_package; use english_support_package;
with weed;
with weed_all;
with dictionary_form;
with Utils.General;
procedure makeewds is
   package Integer_IO is new Text_IO.Integer_IO(Integer);
   use Text_IO;
   use Integer_IO;
   use Stem_Key_Type_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Part_Of_Speech_Type_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;
   use ewds_record_io;

   porting  : constant Boolean := False;
   checking : constant Boolean := True;

   d_k : Dictionary_Kind := xxx;       --  ######################

   start_stem_1  : constant := 1;
   start_stem_2  : constant := start_stem_1 + Max_Stem_Size + 1;
   start_stem_3  : constant := start_stem_2 + Max_Stem_Size + 1;
   start_stem_4  : constant := start_stem_3 + Max_Stem_Size + 1;
   start_part    : constant := start_stem_4 + Max_Stem_Size + 1;

   line_number : Integer := 0;

   subtype line_type is String(1..400);

   n : Integer := 0;

   Input, Output, check : Text_IO.File_Type;
   de : Dictionary_Entry;

   s, line : line_type := (others => ' ');
   blank_line : constant line_type := (others => ' ');
   l, last : Integer := 0;

   ewa : ewds_array(1..40) := (others => null_ewds_record);

   ewr : ewds_record := null_ewds_record;

   --  First we supplement MEAN with singles of any hyphenated words
   --  In principle this could be done in the main EXTRACT, much same logic/code
   --  However this is difficult code for an old man, EXTRACT was hard when I was a bit younger
   --  And I cannot remember anything about it.  Separating them out makes it much easier to test

   function add_hyphenated(s : String) return String is

      --------  I tried to do something with hyphenated but so far it does not work  ----------

      --  Find hyphenated words and add them to MEAN with a / connector, right before the parse
      --  so one has both the individual words (may be more than two) and a single combined word
      --  counting-board -> counting board/countingboard
      t : String (1..Max_Meaning_Size*2 + 20) := (others => ' ');   --  Cannot be bigger
      word_start : Integer := 1;
      word_end   : Integer := 0;
      i, j, jmax : Integer := 0;
      hyphenated : Boolean := False;

   begin
      --PUT_LINE("S    " & INTEGER'IMAGE(LINE_NUMBER) & "   " & INTEGER'IMAGE(S'FIRST) & "  " & INTEGER'IMAGE(S'LAST));
      --PUT_LINE(S);
      while i < s'Last  loop
         i := i + 1;
         j := j + 1;
         word_end := 0;
         --PUT(INTEGER'IMAGE(I) & "-");

         --  First clear away or ignore all the non-words stuff
         if s(i) = '|'   then     --  Skip continuation |'s
            word_start := i + 1;
            t(j) := s(i);
            j := j + 1;
            jmax := jmax + 1;
            null;
            i := i + 1;
            --PUT_LINE("|||    " & INTEGER'IMAGE(LINE_NUMBER) & "    " & S(I) & '_' & S(WORD_START..S'LAST));
         elsif s(i) = '"'   then     --  Skip "'s
            word_start := i + 1;
            t(j) := s(i);
            j := j + 1;
            jmax := jmax + 1;
            null;
            i := i + 1;
            --PUT_LINE('"' &   "   " & INTEGER'IMAGE(LINE_NUMBER) & "    ->" & S(WORD_START..S'LAST));
         else
            if s(i) = '('  then    --  (...) not to be parsed
               t(j) := s(i);
               j := j + 1;
               jmax := jmax + 1;
               i := i + 1;
               while s(i) /= ')'  loop
                  t(j) := s(i);
                  j := j + 1;
                  jmax := jmax + 1;
                  i := i + 1;
               end loop;
               word_start := i + 2;   --  Skip };
               word_end   := 0;
            elsif s(i) = '['  then    --  (...) not to be parsed
               t(j) := s(i);
               j := j + 1;
               jmax := jmax + 1;
               i := i + 1;
               while s(i-1..i) /= "=>"  loop
                  t(j) := s(i);
                  j := j + 1;
                  jmax := jmax + 1;
                  i := i + 1;
               end loop;
               word_start := i + 2;
               word_end   := 0;

            end if;
            --  Finished with the non-word stuff

            if s(i) = '-' then
               word_end := i - 1;

               --              if  (I /= S'FIRST)  and then    --  Not -word
               --               ( (S(I-1) /= ' ') and
               --                 (S(I-1) /= '/')  )  then
               --                 HYPHENATED := TRUE;
               --              end if;
               --PUT_LINE("---    " & INTEGER'IMAGE(LINE_NUMBER) & "   " & INTEGER'IMAGE(I) &
               --"  " & INTEGER'IMAGE(WORD_START) & "  " & INTEGER'IMAGE(WORD_END) & "   ->" & S(WORD_START..WORD_END));
            end if;

            if
              s(i) = ' '  or
              s(i) = '/'  or
              s(i) = ','  or
              s(i) = ';'  or
              s(i) = '!'  or
              s(i) = '?'  or
              s(i) = '+'  or
              s(i) = '*'  or
              s(i) = '"'  or
              s(i) = '('
            then
               word_end := i - 1;

               --PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "    NNN " & S(I) & "  " & INTEGER'IMAGE(I) & "  "
               --& INTEGER'IMAGE(WORD_START)& "   " & INTEGER'IMAGE(WORD_END) & "    " & S(WORD_START..WORD_END));
               if hyphenated  then
                  t(j) := '/';
                  j := j + 1;
                  jmax := jmax + 1;
                  for k in word_start..word_end loop
                     if s(k) /= '-'  then
                        t(j) := s(k);
                        j := j + 1;
                        jmax := jmax + 1;
                     end if;
                  end loop;
                  hyphenated := False;
               end if;

            end if;

            if  --WORD_END /= 0  and then
              s(i) = ' ' or s(i) = '/'
            then
               word_start := i + 1;
               word_end   := 0;
            end if;

            --PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "    TTT " & S(I) & "  " &  INTEGER'IMAGE(I) &
            --"  " & INTEGER'IMAGE(WORD_START) & "   " & INTEGER'IMAGE(WORD_END) & "    " & S(WORD_START..WORD_END));

         end if;  --  On '|'

         --  Set up the Output to return
         --PUT('|' & INTEGER'IMAGE(J) & '/' & INTEGER'IMAGE(I));
         t(j) := s(i);
         jmax := jmax + 1;

      end loop;  --  Over S'RANGE

      --PUT_LINE("RRR    ->" & INTEGER'IMAGE(LINE_NUMBER) & "   " & T(1..JMAX));
      return t(1..jmax);

   exception
      when others =>
         Put_Line("ADD_HYPHENATED  Exception    LINE = " &
                    Integer'Image(line_number));
         Put_Line(s);
         Put(de); New_Line;
         return t(1..jmax);
   end add_hyphenated;

   procedure extract_words (s : in String;
                            pofs : in Part_Of_Speech_Type;
                            n : out Integer;
                            ewa : out ewds_array) is
      -- i, j, js, k, l, m, im, ic : Integer := 0;
      j, k, l, m, im, ic : Integer := 0;
      end_semi : constant Integer := 1;
      --  Have to expand type to take care of hyphenated
      subtype x_Meaning_Type is String(1..Max_Meaning_Size*2+20);
      null_x_Meaning_Type : constant x_Meaning_Type := (others => ' ');
      semi, comma : x_Meaning_Type := null_x_Meaning_Type;

      ww : Integer := 0;    --  For debug
   begin
      --NEW_LINE(2);
      --PUT_LINE("MEAN  " & INTEGER'IMAGE(LINE_NUMBER) & "  =>" & S);
      --PUT_LINE("MEAN=>" & INTEGER'IMAGE(S'FIRST) & "  " & INTEGER'IMAGE(S'LAST) & "|::::::::");
      -- i := 1;    --  Element Position in line, per SEMI
      j := 1;    --  Position in word
      k := 0;    --  SEMI - Division in line
      l := 1;    --  Position in MEAN, for EXTRACTing SEMI
      m := 1;    --  COMMA in SEMI
      n := 1;    --  Word number
      im := 0;   --  Position in SEMI
      ic := 0;   --  Position in COMMA

      ewa(n) := null_ewds_record;

      -- Slightly disparage extension
      if s(s'First) = '|' then
         k := 3;
      end if;

      while l <= s'Last loop  --  loop over MEAN
         if s(l) = ' ' then  --  Clear initial blanks
            l := l + 1;
         end if;

         semi := null_x_Meaning_Type;
         im := 1;
         extract_semi:
         loop
            --PUT('/');
            --PUT(S(L));
            if s(l) = '|' then
               null;          --  Ignore continuation flag | as word
            elsif s(l) in '0'..'9'  then
               null;         --  Ignore numbers
            elsif s(l) = ';'  then     --  Division Terminator
                                       --PUT(':');
               k := k + 1;
               --PUT('+');
               l := l + 1;  --  Clear ;
               exit extract_semi;
            elsif s(l) = '('  then  --  Skip (...)  !
            --PUT('[');
               while s(l) /= ')'   loop
                  --PUT('+');
                  --PUT(INTEGER'IMAGE(L));
                  --PUT(S(L));
                  exit when l = s'Last;  -- Run out
                  l := l + 1;
               end loop;
               --              L := L + 1;    --  Clear the ')'
               --PUT('^');
               --PUT(INTEGER'IMAGE(L));
               --PUT(S(L));
               if l > s'Last then
                  l := s'Last;
               else
                  if  s(l) = ';'  then  --  );
                     exit extract_semi;
                  end if;
               end if;
               --PUT(']');

               if l >= s'Last then  --  Ends in )
                  --  PUT('!');
                  exit extract_semi;
               end if;
               --PUT('+');
               --L := L + 1;    --  Clear the ')'
            elsif l = s'Last then
               --PUT('|');
               l := l + 1;     --  To end the loop
               exit extract_semi;

            else
               semi(im) := s(l);
               im := im + 1;
            end if;
            --PUT('+');
            --IM := IM + 1;  --  To next Character
            l := l + 1;  --  To next Character
         end loop extract_semi;

         ww := 10;

         --if LINE_NUMBER = 8399  then
         --NEW_LINE;
         --PUT_LINE("NEW SEMI=>" & SEMI(SM1..SM2) & "|::::::::");
         --PUT_LINE("NEW SEMI INDEX=>" & INTEGER'IMAGE(SM1) & "  " & INTEGER'IMAGE(SM2) & "|::::::::");
         --end if;

         process_semi:
         declare
            st : constant String := Trim (semi);
            sm : constant String(st'First..st'Last) := st;
         begin
            if st'Length > 0  then
               comma := null_x_Meaning_Type;
               im := sm'First;
               m := 0;

               --I := SM'FIRST;
               --while  I <= ST'LAST  loop

               --PUT(S(I));
               --PUT('*');

               --COMMA := NULL_X_MEANING_TYPE;

               ic := 1;
               loop_over_semi:

               while im <= sm'Last loop
                  comma := null_x_Meaning_Type;
                  ww := 20;
                  find_comma:
                  loop
                     --PUT(INTEGER'IMAGE(IM) & " ( " & SM(IM));
                     if sm(im) = '('  then  --  Skip (...)  !
                        while sm(im) /= ')'   loop
                           im := im + 1;
                        end loop;
                        im := im + 1;    --  Clear the ')'
                        --        IM := IM + 1;    --  Go to next Character
                        --PUT_LINE("Cleared (+" & "  IM = " & INTEGER'IMAGE(IM));
                        if im >= end_semi  then
                           --PUT_LINE("exit on SM'LAST  "  & INTEGER'IMAGE(SM'LAST) & "  I = " & INTEGER'IMAGE(IM));
                           exit find_comma;
                        end if;
                        --PUT_LINE("No exit on SM'LAST  "  & INTEGER'IMAGE(SM'LAST) & "  I = " & INTEGER'IMAGE(IM) & "|" & SM(IM) & "|");
                        if  (sm(im) = ';') or (sm(im) = ',')   then
                           --PUT_LINE("Found ;, COMMA  IM = " & INTEGER'IMAGE(IM));
                           --  Foumd COMMA
                           m := m + 1;
                           ic := 1;
                           im := im + 1;       --  Clear ;,
                           exit find_comma;
                        elsif sm(im) = ' '  then
                           --PUT_LINE("Found blank -  IM = " & INTEGER'IMAGE(IM));
                           im := im + 1;
                           --PUT_LINE("Found blank +  IM = " & INTEGER'IMAGE(IM));
                        end if;
                        --PUT_LINE("------------------------");
                     end if;

                     if sm(im) = '['  then  --  Take end of [=>]
                        while sm(im) /= '>'  loop
                           exit when sm(im) = ']'; --  If no >
                           im := im + 1;
                        end loop;
                        im := im + 1;    --  Clear the '>' or ']'
                        if  sm(im) = ';'  then
                           --  Foumd COMMA
                           m := m + 1;
                           ic := 1;
                           im := im + 1;       --  Clear ;
                           exit find_comma;
                        elsif sm(im) = ' '  then
                           im := im + 1;
                        end if;
                     end if;          --  But could be 2 =>!
                     --PUT_LINE("Through ()[] I = " & INTEGER'IMAGE(I));
                     exit find_comma when im > sm'Last;

                     --PUT(INTEGER'IMAGE(IM) & " ) " & SM(IM));
                     if sm(im) = ','  then
                        --  Foumd COMMA
                        m := m + 1;
                        ic := 1;
                        im := im + 1;       --  Clear ,
                        exit find_comma;
                     elsif im >= sm'Last or im = s'Last then
                        --  Foumd COMMA
                        comma(ic) := sm(im);
                        m := m + 1;
                        ic := 1;
                        exit find_comma;
                     else
                        comma(ic) := sm(im);
                        im := im + 1;
                        ic := ic + 1;

                     end if;
                     --PUT(INTEGER'IMAGE(IM) & " ! " & SM(IM));

                  end loop find_comma;
                  --PUT_LINE("COMMA " & INTEGER'IMAGE(LINE_NUMBER) & INTEGER'IMAGE(IM) &  "=>" & TRIM(COMMA));
                  im := im + 1;

                  ww := 30;

                  process_comma:
                  declare
                     ct : constant String := Trim (comma);
                     cs : String(ct'First..ct'Last) := ct;
                     pure : Boolean := True;
                     w_start, w_end : Integer := 0;
                  begin
                     ww := 31;
                     --PUT_LINE("PROCESS COMMA " & INTEGER'IMAGE(LINE_NUMBER) & INTEGER'IMAGE(CT'FIRST) & INTEGER'IMAGE(CT'LAST) &  "=>" & TRIM(COMMA));
                     if ct'Length > 0 then    --  Is COMMA non empty
                                              --  Are there any blanks?
                                              --  If not then it is a pure word
                                              --  Or words with /
                        for ip in cs'Range loop
                           if cs(ip) = ' '  then
                              pure := False;
                           end if;
                        end loop;

                        ww := 32;

                        --  Check for WEED words and eliminate them
                        w_start := cs'First;
                        w_end   := cs'Last;
                        for iw in cs'Range loop
                           --PUT('-');
                           --PUT(CS(IW));
                           if (cs(iw) = '(')  or
                              (cs(iw) = '[')
                           then
                              ww := 33;
                              w_start := iw + 1;
                           else
                              ww := 34;
                              if (cs(iw) = ' ')  or
                                 (cs(iw) = '_')  or
                                 (cs(iw) = '-')  or
                                 (cs(iw) = ''')  or
                                 (cs(iw) = '!')  or
                                 (cs(iw) = '/')  or
                                 (cs(iw) = ':')  or
                                 (cs(iw) = '.')  or
                                 (cs(iw) = '!')  or
                                 (cs(iw) = ')')  or
                                 (cs(iw) = ']')  or
                                 (iw = cs'Last)
                              then
                                 --PUT_LINE("HIT  "  & CS(IW) & "  IW = " & INTEGER'IMAGE(IW) & "  CS'LAST = " & INTEGER'IMAGE(CS'LAST));
                                 ww := 35;
                                 if iw = cs'Last then
                                    w_end := iw;
                                 elsif iw /= cs'First then
                                    w_end := iw - 1;
                                 end if;

                                 ww := 36;
                                 --  KLUDGE
                                 if cs(w_start) = '"' then
                                    ww := 361;
                                    w_start := w_start + 1;
                                    ww := 362;
                                 elsif cs(w_end) = '"' then
                                    ww := 364;
                                    w_end := w_end - 1;
                                    ww := 365;
                                 end if;

                                 ww := 37;

                                 --PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "WEEDing " &
                                 --INTEGER'IMAGE(W_START) & "  " & INTEGER'IMAGE(W_END)
                                 --& "  " & CS(W_START..W_END)
                                 --);
                                 weed_all(cs(w_start..w_end));

                                 if not pure then
                                    weed(cs(w_start..w_end), pofs);
                                 end if;
                                 w_start := iw + 1;
                              end if;
                              ww := 38;
                           end if;
                           ww := 39;
                        end loop;          --  On CS'RANGE

                        --PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "WEED done");

                        ww := 40;
                        --  Main process of COMMA
                        ic := 1;
                        j := 1;
                        while  ic <= cs'Last  loop
                           --PUT(CS(IC));

                           if cs(ic) = '"'  or      --  Skip all "
                              cs(ic) = '('  or      --  Skip initial (
                              --CS(IC) = '-'  or      --  Skip hyphen -> one word!
                              cs(ic) = '?'  or      --  Ignore ?
                              cs(ic) = '~'  or      --  Ignore about ~
                              cs(ic) = '*'  or
                              cs(ic) = '%'  or      --  Ignore percent unless word
                              cs(ic) = '.'  or      --  Ignore ...
                              cs(ic) = '\'  or      --  Ignore weed
                              (cs(ic) in '0'..'9')
                           then --  Skip numbers
                              ic := ic + 1;
                              ww := 50;
                              ----PUT('-');
                           else
                              if
                              --            S(IC) = ','  or       --  Terminators
                              --            S(IC) = '.'  or       --  Would be typo
                              --            S(IC) = ';'  or       --  Should catch at SEMI
                                 cs(ic) = '/'  or
                                 cs(ic) = ' '  or
                                 cs(ic) = '''  or    --  Ignore all ' incl 's  ???
                                 cs(ic) = '-'  or    --  Hyphen causes 2 words  XXX
                                 cs(ic) = '+'  or    --  Plus causes 2 words
                                 cs(ic) = '_'  or    --  Underscore causes 2 words
                                 cs(ic) = '='  or    --  = space/terminates
                                 cs(ic) = '>'  or
                                 cs(ic) = ')'  or
                                 cs(ic) = ']'  or
                                 cs(ic) = '!'  or
                                 cs(ic) = '?'  or
                                 cs(ic) = '+'  or
                                 cs(ic) = ':'  or
                                 cs(ic) = ']'
                              then  --  Found word
                                 ww := 60;
                                 --PUT('/');
                                 ewa(n).semi := k;
                                 if pure  then
                                    if k = 1  then
                                       ewa(n).kind := 15;
                                    else
                                       ewa(n).kind := 10;
                                    end if;
                                 else
                                    ewa(n).kind := 0;
                                 end if;
                                 ww := 70;
                                 --PUT_LINE("====1  K J = " & INTEGER'IMAGE(K) & "  " & INTEGER'IMAGE(J) & "  ." & EWA(N).W(1..J-1) & ".");
                                 n := n + 1;       --  Start new word in COMMA
                                 ic := ic + 1;
                                 j := 1;
                                 ewa(n) := null_ewds_record;

                              elsif ic = cs'Last then --  Order of if important
                                                      --  End, Found word
                                                      --PUT('!');
                                 ewa(n).w(j) := cs(ic);
                                 ewa(n).semi := k;
                                 if pure  then
                                    if k = 1  then
                                       ewa(n).kind := 15;
                                    else
                                       ewa(n).kind := 10;
                                    end if;
                                 else
                                    ewa(n).kind := 0;
                                 end if;
                                 --PUT_LINE("====2  K J = " & INTEGER'IMAGE(K) & "  " & INTEGER'IMAGE(J) & "  ." & EWA(N).W(1..J) & ".");
                                 n := n + 1;       --  Start new word/COMMA

                                 ewa(n) := null_ewds_record;
                                 exit;
                              else
                                 ww := 80;
                                 --PUT('+');
                                 ewa(n).w(j) := cs(ic);
                                 j := j + 1;
                                 ic := ic + 1;
                              end if;
                           end if;
                           ww := 90;
                        end loop;

                     end if;  -- On COMMA being empty
                  end process_comma;
                  --PUT_LINE("COMMA Processed ");

               end loop loop_over_semi;

               --PUT_LINE("LOOP OVER SEMI Processed ");

            end if;    -- On ST'LENGTH > 0
            --PUT_LINE("LOOP OVER SEMI after ST'LENGTH  0 ");
         end process_semi;

         --PUT_LINE("SEMI Processed ");
         -- I = "  & INTEGER'IMAGE(I)
         --& "  S(I) = " & S(I)
         --);
         if (l < s'Last) and then (s(l) = ';') then             --  ??????
            --PUT_LINE("Clear  L = " & INTEGER'IMAGE(L));
            l := l + 1;
         end if;

         -- investigate this:
         -- js := l;    --  Odd but necessary    ?????
         for j in l .. s'Last loop
            exit when j = s'Last;
            if s(j) = ' ' then
               l := l + 1;
            else
               exit;
            end if;
         end loop;

         --PUT_LINE("SEMI Processed Completely   L = "  & INTEGER'IMAGE(L)  & "  S'LAST = " & INTEGER'IMAGE(S'LAST));

         exit when l >= s'Last;
      end loop;   --  loop over MEAN

      --PUT_LINE("SEMI loop Processed");
      if ewa(n) = null_ewds_record  then
         n := n -1;   --  Clean up danglers
      end if;
      if ewa(n) = null_ewds_record  then   --  AGAIN!!!!!!
         n := n -1;   --  Clean up danglers
      end if;
   exception
      when others =>
         if (s(s'Last) /= ')') or  (s(s'Last) /= ']') then    --  KLUDGE
            New_Line;
            Put_Line("Extract Exception    WW = " & Integer'Image(ww) & "    LINE = " &
                       Integer'Image(line_number));
            Put_Line(s);
            Put(de); New_Line;
         end if;
   end extract_words;

begin
   Put_Line ("Takes a DICTLINE.D_K and produces a EWDSLIST.D_K ");
   Utils.General.Load_Dictionary (line, last, d_k);

   --PUT_LINE("OPENING   " &
   --     ADD_FILE_NAME_EXTENSION(DICT_LINE_NAME, DICTIONARY_KIND'IMAGE(D_K)));

   Open(Input, In_File, add_file_name_extension(dict_line_name,
                                                Dictionary_Kind'Image(d_k)));
   --PUT_LINE("OPEN");

   if not porting  then
      --PUT_LINE("CREATING");

      Create(Output, Out_File, add_file_name_extension("EWDSLIST",
                                                       Dictionary_Kind'Image(d_k)));

      if checking  then
         Create (check, Out_File, "CHECKEWD.");
      end if;

      --PUT_LINE("CREATED");
   end if;

   --  Now do the rest
   over_lines:
   while not End_Of_File(Input) loop
      s := blank_line;
      Get_Line(Input, s, last);
      if Trim (s(1..last)) /= ""  then       --  If non-blank line
         l := 0;

         form_de:
         begin
            de.Stems(1) := s(start_stem_1..Max_Stem_Size);
            --NEW_LINE; PUT(DE.STEMS(1));
            de.Stems(2) := s(start_stem_2..start_stem_2+Max_Stem_Size-1);
            de.Stems(3) := s(start_stem_3..start_stem_3+Max_Stem_Size-1);
            de.Stems(4) := s(start_stem_4..start_stem_4+Max_Stem_Size-1);
            --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('@');
            Get(s(start_part..last), de.Part, l);
            --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('&'); PUT(S(L+1..LAST)); PUT('3');
            --GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
            -- FIXME: Why not Translation_Record_IO.Put ?
            Get(s(l+1..last), de.Tran.Age, l);
            Get(s(l+1..last), de.Tran.Area, l);
            Get(s(l+1..last), de.Tran.Geo, l);
            Get(s(l+1..last), de.Tran.Freq, l);
            Get(s(l+1..last), de.Tran.Source, l);
            de.Mean := Head (s(l+2..last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING, not ENUM

         exception
            when others =>
               New_Line;
               Put_Line("GET Exception  LAST = " & Integer'Image(last));
               Put_Line(s(1..last));
               Integer_IO.Put(line_number); New_Line;
               Put(de); New_Line;
         end form_de;

         line_number := line_number + 1;

         if de.Part.pofs = v and then de.Part.v.Con.which = 8 then
            --  V 8 is a kludge for variant forms of verbs that have regular forms elsewhere
            null;
         else
            --  Extract words
            extract_words(add_hyphenated(Trim (de.Mean)), de.Part.pofs, n, ewa);

            --      EWORD_SIZE    : constant := 38;
            --      AUX_WORD_SIZE : constant := 9;
            --      LINE_NUMBER_WIDTH : constant := 10;
            --
            --      type EWDS_RECORD is
            --        record
            --          POFS : PART_OF_SPEECH_TYPE := X;
            --          W    : STRING(1..EWORD_SIZE);
            --          AUX  : STRING(1..AUX_WORD_SIZE);
            --          N    : INTEGER;
            --        end record;

            for i in 1..n  loop
               if Trim (ewa(i).w)'Length /= 0 then
                  ewr.w := Head (Trim (ewa(i).w), eword_size);
                  ewr.aux := Head ("",  aux_word_size);
                  ewr.n := line_number;
                  ewr.pofs := de.Part.pofs;
                  ewr.freq := de.Tran.Freq;
                  ewr.semi := ewa(i).semi;
                  ewr.kind := ewa(i).kind;
                  ewr.rank := 80-Frequency_Type'Pos(ewr.freq)*10 + ewr.kind + (ewr.semi-1)*(-3);
                  if ewr.freq = Inflections_Package.n  then
                     ewr.rank := ewr.rank + 25;
                  end if;

                  --PUT(EWA(I)); NEW_LINE;
                  --PUT(EWR); NEW_LINE;
                  Put(Output, ewr);

                  -- SET_COL(OUTPUT, 71);
                  -- INTEGER_IO.PUT(OUTPUT, I, 2);

                  New_Line(Output);

                  if checking  then
                     --  Now make the CHECK file

                     Put(check, ewr.w);
                     Set_Col(check, 25);
                     declare
                        df : constant String := dictionary_form(de);
                        ii : Integer := 1;
                     begin
                        if df'Length > 0  then
                           while df(ii) /= ' '  and
                              df(ii) /= '.'     and
                              df(ii) /= ','
                           loop
                              Put(check, df(ii));
                              ii := ii+ 1;
                              exit when ii = 19;
                           end loop;
                        end if;
                     end;

                     Set_Col(check, 44);
                     Put(check, ewr.n, 6);
                     Put(check, ' ');
                     Put(check, ewr.pofs);
                     Put(check, ' ');
                     Put(check, ewr.freq);
                     Put(check, ' ');
                     Put(check, ewr.semi, 5);
                     Put(check, ' ');
                     Put(check, ewr.kind, 5);
                     Put(check, ' ');
                     Put(check, ewr.rank, 5);
                     Put(check, ' ');
                     Put(check, de.Mean);

                     New_Line(check);
                  end if;
               end if;
            end loop;
         end if;  --  If non-blank line
      end if;
   end loop over_lines;

   Put_Line("NUMBER_OF_LINES = " & Integer'Image(line_number));

   if not porting  then
      Close(Output);
      if checking  then
         Close(check);
      end if;
   end if;

exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line(s(1..last));
      Integer_IO.Put(line_number); New_Line;
      Close(Output);
      if checking  then
         Close(check);
      end if;
end makeewds;
