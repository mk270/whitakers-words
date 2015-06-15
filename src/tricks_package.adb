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
with word_parameters; use word_parameters;
with developer_parameters; use developer_parameters;
with inflections_package; use inflections_package;
with word_support_package; use word_support_package;
with word_package; use word_package;
with put_stat;
package body tricks_package is

   function is_a_vowel(c : character) return boolean is
   begin
      if lower_case(c) = 'a'  or
        lower_case(c) = 'e'  or
        lower_case(c) = 'i'  or
        lower_case(c) = 'o'  or
        lower_case(c) = 'u'  or
        lower_case(c) = 'y'  then
         return true;
      else
         return false;
      end if;
   end is_a_vowel;

   function a_roman_digit(char : character) return boolean is
   begin
      case char is
         when 'M' | 'm'  =>
            return true;
         when 'D' | 'd'  =>
            return true;
         when 'C' | 'c'  =>
            return true;
         when 'L' | 'l'  =>
            return true;
         when 'X' | 'x'  =>
            return true;
            --when 'U' | 'u'  => return TRUE;  --  possible but unlikely
         when 'V' | 'v'  =>
            return true;
         when 'I' | 'i'  =>
            return true;
         when others =>
            return false;
      end case;
   end a_roman_digit;

   function value(char : character) return natural is
   begin
      case char is
         when 'M' | 'm'  =>
            return 1000;
         when 'D' | 'd'  =>
            return  500;
         when 'C' | 'c'  =>
            return  100;
         when 'L' | 'l'  =>
            return   50;
         when 'X' | 'x'  =>
            return   10;
            --when 'U' | 'u'  => return    5;  --  possible but unlikely
         when 'V' | 'v'  =>
            return    5;
         when 'I' | 'i'  =>
            return    1;
         when others =>
            return    0;
      end case;
   end value;

   function only_roman_digits(s : string) return boolean is
   begin

      for i in s'range  loop
         if not a_roman_digit(s(i))  then
            return false;
         end if;
      end loop;
      return true;
   end only_roman_digits;

   function roman_number(st : string) return natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid

      use text_io;
      total : natural := 0;
      invalid : exception;
      j : integer := 0;
      s : constant string := upper_case(st);

   begin
      if only_roman_digits(s)  then

         --
         --NUMERALS IN A STRING ARE ADDED: CC = 200 ; CCX = 210.
         --ONE NUMERAL TO THE LEFT of A LARGER NUMERAL IS SUBTRACTED FROM THAT NUMBER: IX = 9
         --
         --SUBTRACT ONLY A SINGLE LETTER FROM A SINGLE NUMERAL.
         --VIII FOR 8, NOT IIX; 19 IS XIX, NOT IXX.
         --
         --SUBTRACT ONLY POWERS of TEN, SUCH AS I, X, or C.
         --NOT VL FOR 45, BUT XLV.
         --
         --DON'T SUBTRACT A LETTER FROM ANOTHER LETTER MORE THAN TEN TIMES GREATER.
         --ONLY SUBTRACT I FROM V or X, and X FROM L or C.
         --NOT IL FOR 49, BUT XLIX. MIM is ILLEGAL.
         --
         --ONLY IF ANY NUMERAL PRECEEDING IS AT LEAST TEN TIMES LARGER.
         --NOT VIX FOR 14, BUT XIV.
         --NOT  IIX, BUT VIII.
         --ONLY IF ANY NUMERAL FOLLOWING IS SMALLER.
         --NOT XCL FOR 140, BUT CXL.
         --
         j := s'last;

         evaluate:
             while j >= s'first  loop
                --
                --Legal in the Ones position
                --  I
                --  II
                --  III
                --  IIII    IV
                --  V
                --  VI
                --  VII
                --  VIII
                --  VIIII   IX
                --
                --
                --  Ones
                if s(j) = 'I' then
                   total := total + 1;
                   j := j - 1;
                   exit evaluate when j < s'first;
                   while s(j) = 'I'  loop
                      total := total + 1;
                      if total >= 5  then raise invalid; end if;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end loop;
                end if;

                if s(j) = 'V'  then
                   total := total + 5;
                   j := j - 1;
                   exit evaluate when j < s'first;
                   if s(j) = 'I'  and total = 5  then
                      total := total - 1;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;

                   if s(j) = 'I' or s(j) = 'V'  then raise invalid; end if;
                end if;

                --
                --Legal in the tens position
                --  X
                --  XX
                --  XXX
                --  XXXX    XL
                --  L
                --  LX
                --  LXX
                --  LXXX
                --  LXXXX   XC
                --

                --  Tens
                if s(j) = 'X'  then
                   total := total + 10;
                   j := j - 1;
                   exit evaluate when j < s'first;
                   while s(j) = 'X'  loop
                      total := total + 10;
                      if total >= 50  then raise invalid; end if;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end loop;
                   if s(j) = 'I'  and total = 10  then
                      total := total - 1;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'I' or s(j) = 'V'  then
                      raise invalid;
                   end if;
                end if;

                if s(j) = 'L'  then
                   total := total + 50;
                   j := j - 1;
                   exit evaluate when j < s'first;

                   if s(j) = 'X'  and total <= 59  then
                      total := total - 10;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L'  then raise invalid; end if;

                   if s(j) = 'C'  then
                      total := total + 100;
                      j := j - 1;
                      exit evaluate when j < s'first;
                      if s(j) = 'X'  and total = 100  then
                         total := total - 10;
                         j := j - 1;
                         exit evaluate when j < s'first;
                      end if;
                   end if;

                   if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L'  then raise invalid; end if;
                end if;

                if s(j) = 'C'  then
                   total := total + 100;
                   j := j - 1;
                   exit evaluate when j < s'first;
                   while s(j) = 'C'  loop
                      total := total + 100;
                      if total >= 500  then raise invalid; end if;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end loop;
                   if s(j) = 'X'  and total <= 109  then
                      total := total - 10;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L'  then raise invalid; end if;
                end if;

                if s(j) = 'D'  then
                   total := total + 500;
                   j := j - 1;
                   exit evaluate when j < s'first;
                   if s(j) = 'C'  and total <= 599  then
                      total := total - 100;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'M'  then
                      total := total + 1000;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'C'  and total <= 1099  then
                      total := total - 100;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L' or s(j) = 'C' or s(j) = 'D'  then raise invalid; end if;
                end if;

                if s(j) = 'M'  then
                   total := total + 1000;
                   j := j - 1;
                   exit evaluate when j < s'first;
                   while s(j) = 'M'  loop
                      total := total + 1000;
                      if total >= 5000  then raise invalid; end if;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end loop;
                   if s(j) = 'C'  and total <= 1099  then
                      total := total - 100;
                      j := j - 1;
                      exit evaluate when j < s'first;
                   end if;
                   if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L' or s(j) = 'C' or s(j) = 'D'  then raise invalid; end if;
                end if;

             end loop evaluate;

      end if;  --  On Only Roman digits

      return total;
   exception
      when invalid  =>
         return 0;
      when constraint_error  =>
         return 0;
   end roman_number;

   procedure roman_numerals(input_word : string;
                            pa : in out parse_array; pa_last : in out integer) is

      w : constant string := trim(input_word);
      roman_number_w : constant integer := roman_number(w);

   begin
      if only_roman_digits(w) and then (roman_number_w /= 0)  then
         pa_last := pa_last + 1;
         pa(pa_last) := ( stem => head(w, max_stem_size),
           ir => (
           qual => (
           pofs => num,
           num => (
           decl   => (2, 0),
           cs     => x,
           number => x,
           gender => x,
           sort   => card) ),

           key => 0,
           ending => null_ending_record,
           age => x,
           freq => a),
           d_k => rrr,
           mnpc => null_mnpc);
         rrr_meaning := head(integer'image(roman_number_w) & "  as a ROMAN NUMERAL;",
           max_meaning_size);
      else
         null;    --  Is not ROMAN NUMERAL, so go on and try something else
      end if;
   end roman_numerals;

   function bad_roman_number(s : string) return natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid
      --  This seems to allow all of Caesar's.   Actually there are no rules
      --  if you look at some of the 12-15 century stuff
      use text_io;
      total : integer := 0;
      decremented_from : integer := 0;

   begin

      --  Already known that all the characters may be valid numerals
      --  Loop over the string to check validity, start with second place
      --PUT_LINE(" In function BAD_ROMAN_NUMBER ");
      --PUT_LINE(" BEFORE LOOP      S = " & S);
      total := value(s(s'last));
      decremented_from := value(s(s'last));
      for i in reverse s'first..s'last-1  loop

         if value(s(i)) < value(s(i+1))  then
            --  Decrement
            total := total - value(s(i));
            decremented_from := value(s(i+1));
         elsif value(s(i)) = value(s(i+1))  then
            if  value(s(i)) < decremented_from  then
               total := total - value(s(i));   --  IIX = 8 !
            else
               total := total + value(s(i));
            end if;
         elsif  value(s(i)) > value(s(i+1))  then
            total := total + value(s(i));
            decremented_from := value(s(i+1));
         end if;
      end loop;
      if total > 0  then
         return total;
      else
         return 0;
      end if;

   exception
      when others  =>
         return 0;
   end bad_roman_number;

   procedure syncope(w : string;
                     pa : in out parse_array; pa_last : in out integer) is
      s  : constant string(1..w'length) := lower_case(w);
      pa_save : constant integer := pa_last;
      syncope_inflection_record : constant inflection_record := null_inflection_record;
      --     ((V, ((0, 0), (X, X, X), 0, X, X)), 0, NULL_ENDING_RECORD, X, A);
   begin

      --  Syncopated forms (see Gildersleeve and Lodge, 131)

      yyy_meaning := null_meaning_type;

      --  This one has to go first --  special for 3 4
      --  ivi  => ii ,  in perfect  (esp. for V 3 4)
      --  This is handled in WORDS as syncope
      --  It seems to appear in texts as alternative stems  ii and ivi
      for i in reverse s'first..s'last-1  loop
         if (s(i..i+1) = "ii")  then
            pa_last := pa_last + 1;
            pa(pa_last) := ("Syncope  ii => ivi", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'first..i) & "v" & s(i+1..s'last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        --PA(PA_LAST).IR.QUAL.V.CON = (3, 4)/(6, 1) and then
        pa(pa_last).ir.key = 3  then          --  Perfect system
         yyy_meaning := head(
           "Syncopated perfect ivi can drop 'v' without contracting vowel "
           , max_meaning_size);

         put_stat("SYNCOPE  ivi at "
           & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
           & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      -- avis => as, evis => es, ivis => is, ovis => os   in perfect
      for i in reverse s'first..s'last-2  loop     --  Need isse
         if ((s(i..i+1) = "as")  or
           (s(i..i+1) = "es")  or
           (s(i..i+1) = "is")  or
           (s(i..i+1) = "os")) then
            --TEXT_IO.PUT_LINE("SYNCOPE vis   S = " & S & "    PA_SAVE = " & INTEGER'IMAGE(PA_SAVE));
            pa_last := pa_last + 1;
            pa(pa_last)         := ("Syncope   s => vis", syncope_inflection_record,
              yyy, null_mnpc);
            --TEXT_IO.PUT_LINE("SYNCOPE vis   S+ = " & S(S'FIRST..I) & "vi" & S(I+1..S'LAST) & "  " & INTEGER'IMAGE(PA_LAST));
            word(s(s'first..i) & "vi" & s(i+1..s'last), pa, pa_last);
            --TEXT_IO.PUT_LINE("SYNCOPE vis   DONE "  & "    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
            if pa_last > pa_save + 1  then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3  then          --  Perfect system
         yyy_meaning := head(
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , max_meaning_size);
         put_stat("SYNCOPE  vis at "
           & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
           & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
      end if;
      --  end loop;   --  over resulting solutions
      if pa_last > pa_save + 1  then

         return;

      else
         pa_last := pa_save;
      end if;

      -- aver => ar, ever => er, in perfect
      for i in reverse s'first+1..s'last-2  loop
         if ((s(i..i+1) = "ar")  or
           (s(i..i+1) = "er")  or
           (s(i..i+1) = "or")) then
            pa_last := pa_last + 1;
            pa(pa_last) := ("Syncope   r => v.r", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'first..i) & "ve" & s(i+1..s'last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;

      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3  then          --  Perfect system
         yyy_meaning := head(
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , max_meaning_size);

         put_stat("SYNCOPE  ver at "
           & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
           & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      -- iver => ier,  in perfect
      for i in reverse s'first..s'last-3  loop
         if (s(i..i+2) = "ier")  then
            pa_last := pa_last + 1;
            pa(pa_last) := ("Syncope  ier=>iver", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'first..i) & "v" & s(i+1..s'last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3  then          --  Perfect system
         yyy_meaning := head(
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , max_meaning_size);

         put_stat("SYNCOPE  ier at "
           & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
           & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      --         -- sis => s, xis => x, in perfect
      for i in reverse s'first..s'last-2  loop
         if ((s(i) = 's')  or
           (s(i) = 'x'))  then
            pa_last := pa_last + 1;
            pa(pa_last)         := ("Syncope s/x => +is", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'first..i) & "is" & s(i+1..s'last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3  then          --  Perfect system
         yyy_meaning := head(
           "Syncopated perfect sometimes drops the 'is' after 's' or 'x' "
           , max_meaning_size);
         put_stat("SYNCOPEx/sis at "
           & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
           & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      --  end loop;   --  over resulting solutions
      if pa_last > pa_save + 1  then

         return;

      else
         pa_last := pa_save;
      end if;

      pa(pa_last+1) := null_parse_record;     --  Just to clear the trys

   exception
      when others  =>
         pa_last := pa_save;
         pa(pa_last+1) := null_parse_record;     --  Just to clear the trys

   end syncope;

   procedure try_tricks(w : string;
                        pa : in out parse_array; pa_last : in out integer;
                                                 line_number : integer; word_number : integer) is
      --  Since the chances are 1/1000 that we have one,
      --  Ignore the possibility of two in the same word
      --  That is called lying with statistics
      s  : constant string(1..w'length) := w;
      pa_save : constant integer := pa_last;

      procedure tword(w : string;
                      pa : in out parse_array; pa_last : in out integer) is
      begin
         word_package.word(w, pa, pa_last);
         syncope(w, pa, pa_last);
      end tword;

      procedure flip(x1, x2 : string; explanation : string := "") is
         --  At the begining of input word, replaces X1 by X2
         pa_save : constant integer := pa_last;
      begin
         if s'length >= x1'length+2  and then
           s(s'first..s'first+x1'length-1) = x1   then
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x2 & s(s'first+x1'length..s'last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               if explanation = ""  then
                  xxx_meaning := head(
                    "An initial '" & x1 & "' may have replaced usual '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("TRICK   FLIP at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         pa_last := pa_save;
      end flip;

      procedure flip_flop(x1, x2 : string; explanation : string := "") is
         --  At the begining of input word, replaces X1 by X2 - then X2 by X1
         --  To be uesd only when X1 and X2 start with the same letter because it
         --  will be called from a point where the first letter is established
         pa_save : constant integer := pa_last;
      begin
         --TEXT_IO.PUT_LINE("FLIP_FLOP called    " & X1 & "  " & X2);
         if s'length >= x1'length+2  and then
           s(s'first..s'first+x1'length-1) = x1   then
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            --TEXT_IO.PUT_LINE("Trying " & X2 & S(S'FIRST+X1'LENGTH..S'LAST));
            tword(x2 & s(s'first+x1'length..s'last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               --TEXT_IO.PUT_LINE("FLIPF worked");
               if explanation = ""  then
                  xxx_meaning := head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("TRICK  FLIPF at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         --TEXT_IO.PUT_LINE("FLIPF failed");
         --TEXT_IO.PUT_LINE("Try FFLOP");

         if s'length >= x2'length+2  and then
           s(s'first..s'first+x2'length-1) = x2   then
            --TEXT_IO.PUT_LINE("Trying FFLOP");
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod " & x2 & "/" & x1, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            --TEXT_IO.PUT_LINE("Trying " & X1 & S(S'FIRST+X2'LENGTH..S'LAST));
            tword(x1 & s(s'first+x2'length..s'last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               --TEXT_IO.PUT_LINE("FFLOP worked");
               if explanation = ""  then
                  xxx_meaning := head(
                    "An initial '" & x2 & "' may be rendered by '" & x1 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("TRICK  FFLOP at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;

         end if;
         --TEXT_IO.PUT_LINE("FFLIP failed");
         pa_last := pa_save;
      end flip_flop;

      procedure internal(x1, x2 : string; explanation : string := "") is
         --  Replaces X1 with X2 anywhere in word and tries it for validity
         pa_save : constant integer := pa_last;
      begin
         for i in s'first..s'last-x1'length+1  loop
            if s(i..i+x1'length-1) = x1   then
               pa_last := pa_last + 1;
               pa(pa_last) := (head("Word mod " & x1 & "/" & x2, max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(s(s'first..i-1) & x2 & s(i+x1'length..s'last), pa, pa_last);
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)  then
                  if explanation = ""  then
                     xxx_meaning := head(
                       "An internal '" & x1 & "' might be rendered by '" & x2 & "'"
                       , max_meaning_size);
                  else
                     xxx_meaning := head(explanation, max_meaning_size);
                  end if;
                  put_stat("TRICK   INTR at "
                    & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                    & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;
            end if;
         end loop;
         pa_last := pa_save;
      end internal;

      procedure adj_terminal_iis(explanation : string := "") is
         pa_save : constant integer := pa_last;
         i : integer := 0;
      begin
         if s'length > 3  and then
           s(s'last-1..s'last) = "is"   then   --  Terminal 'is'
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod iis -> is", max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            word(s(s'first..s'last-2) & "iis", pa, pa_last);
            if (pa_last > pa_save + 1)    then
               i := pa_last;
               while i > pa_save + 1  loop
                  if pa(i).ir.qual.pofs = adj  and then
                    pa(i).ir.qual.adj.decl = (1, 1)  and then
                    ((pa(i).ir.qual.adj.cs = dat) or
                    (pa(i).ir.qual.adj.cs = abl))   and then
                    pa(i).ir.qual.adj.number = p   then
                     null;       --  Only for ADJ 1 1 DAT/ABL P
                  else
                     pa(i..pa_last-1) := pa(i+1..pa_last);
                     pa_last := pa_last - 1;
                  end if;
                  i := i - 1;
               end loop;
            end if;
            if (pa_last > pa_save + 1)    then
               if explanation = ""  then
                  xxx_meaning := head("A Terminal 'iis' on ADJ 1 1 DAT/ABL P might drop 'i'",
                    max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("TRICK  ADJIS at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         pa_last := pa_save;
      end adj_terminal_iis;

      procedure double_consonants(explanation : string := "") is
         pa_save : constant integer := pa_last;
      begin
         --  Medieval often replaced a classical doubled consonant with single
         --  The problem is to take possible medieval words
         --  and double (all) (isolated) consonants
         for i in s'first+1..s'last-1 loop  --  probably dont need to go to end
            if (not is_a_vowel(s(i))) and then
              (is_a_vowel(s(i-1)) and is_a_vowel(s(i+1))) then
               pa_last := pa_last + 1;
               pa(pa_last)           := (head("Word mod " & s(i) &
                 " -> " & s(i) & s(i), max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(s(s'first..i) & s(i) & s(i+1..s'last), pa, pa_last);
               --TEXT_IO.PUT_LINE(S(S'FIRST..I) & S(I) & S(I+1..S'LAST));
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)  then
                  if explanation = ""  then
                     xxx_meaning := head(
                       "A doubled consonant may be rendered by just the single"
                       & "  MEDIEVAL", max_meaning_size);
                  else
                     xxx_meaning := head(explanation, max_meaning_size);
                  end if;
                  put_stat("TRICK   2CON at "
                    & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                    & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;

            end if;
         end loop;
         pa_last := pa_save;
      end double_consonants;

      procedure two_words(explanation : string := "") is
         --  This procedure examines the word to determine if it is made up
         --  of two separate inflectted words
         --  They are usually an adjective and a noun or two nouns
         pa_save : constant integer := pa_last;
         pa_second : integer := pa_last;
         num_hit_one, num_hit_two : boolean := false;
         --MID : INTEGER := S'LENGTH/2;
         i, i_mid : integer := 0;
         remember_syncope : boolean := false;
         procedure words_no_syncope (w : string;
                                     pa : in out parse_array; pa_last : in out integer) is
         begin
            if words_mdev(do_syncope)  then
               remember_syncope := true;
               words_mdev(do_syncope) := false;
            end if;
            word_package.word(w, pa, pa_last);
            if remember_syncope  then
               words_mdev(do_syncope) := true;
            end if;
         end words_no_syncope;

         function common_prefix(s : string) return boolean is
            --  Common prefixes that have corresponding words (prepositions usually)
            --  which could confuse TWO_WORDS.  We wish to reject these.
         begin
            if s = "dis"  or
              s = "ex"   or
              s = "in"   or
              s = "per"  or
              s = "prae" or
              s = "pro"  or
              s = "re"   or
              s = "si"  or
              s = "sub"  or
              s = "super" or
              s = "trans"    then
               return true;
            else
               return false;
            end if;
         end common_prefix;

      begin
         --TEXT_IO.PUT_LINE("Entering TWO_WORDS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
         --if S(S'FIRST) /= 'q'  then    --  qu words more complicated

         if s'length  < 5  then    --  Dont try on too short words
            return;
         end if;

         i := 2;    --  Smallest is re-publica, but that killed by PREFIX, meipsum
     outer_loop:
         while i < s'length - 2  loop

            pa_last := pa_last + 1;
            pa(pa_last):= (head("Two words", max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            --TEXT_IO.PUT_LINE("Setting PA TWO_WORDS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

            while i < s'length - 2  loop
               --TEXT_IO.PUT_LINE("Trying  " & S(S'FIRST..S'FIRST+I-1));
               if not common_prefix(s(s'first..s'first+i-1))  then
                  words_no_syncope(s(s'first..s'first+i-1), pa, pa_last);
                  if (pa_last > pa_save + 1)     then
                     i_mid := i;
                     for j in pa_save+1..pa_last  loop
                        if pa(j).ir.qual.pofs = num  then
                           num_hit_one := true;
                           exit;
                        end if;
                     end loop;

                     --TEXT_IO.PUT_LINE("HIT first  " & S(S'FIRST..I_MID-1) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     --PARSE_RECORD_IO.PUT(PA(PA_LAST)); TEXT_IO.NEW_LINE;

                     exit;
                  end if;
               end if;
               i := i + 1;
            end loop;

            if (pa_last > pa_save + 1)     then
               null;
               --TEXT_IO.PUT_LINE("Confirm first  " & S(S'FIRST..I_MID) & "    PA_LAST =" & INTEGER'IMAGE(PA_LAST));
            else
               --TEXT_IO.PUT_LINE("No possible first  " & S(S'FIRST..I_MID));
               pa_last := pa_save;
               return;
            end if;

            --  Now for second word
            --TEXT_IO.PUT_LINE("Looking for second  >" & S(I_MID+1..S'LAST));
            pa_last := pa_last + 1;
            pa(pa_last) := null_parse_record;     --  Separator
            pa_second := pa_last;
            words_no_syncope(s(i_mid+1..s'last), pa, pa_last);
            if (pa_last > pa_second)   and then       --  No + 1 since XXX taken care of above
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               for j in pa_second..pa_last  loop
                  if pa(j).ir.qual.pofs = num  then
                     num_hit_two := true;
                     exit;
                  end if;
               end loop;

               --TEXT_IO.PUT_LINE("Found       second  " & S(I_MID+1..S'LAST) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

               if explanation = ""  then

                  if words_mode(trim_output)  and then
                    --  Should check that cases correspond
                    (num_hit_one and num_hit_two)  then
                     --  Clear out any non-NUM if we are in TRIM
                     for j in pa_save+1..pa_last  loop
                        if pa(j).d_k in general..unique  and then
                          pa(j).ir.qual.pofs /= num  then
                           pa(j..pa_last-1) := pa(j+1..pa_last);
                           pa_last := pa_last - 1;
                        end if;
                     end loop;

                     xxx_meaning := head(
                       "It is very likely a compound number    " &
                       s(s'first..s'first+i-1) & " + " &
                       s(s'first+i..s'last), max_meaning_size);
                     put_stat("TRICK   2NUM at "
                       & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                       & "   " & head(w, 20) & "   "  & s(1..i_mid) & '+' & s(i_mid+1..s'last));
                  else
                     xxx_meaning := head(
                       "May be 2 words combined (" &
                       s(s'first..s'first+i-1) & "+" &
                       s(s'first+i..s'last) &
                       ") If not obvious, probably incorrect", max_meaning_size);
                     put_stat("TRICK   2WDS at "
                       & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                       & "   " & head(w, 20) & "   "  & s(1..i_mid) & '+' & s(i_mid+1..s'last));
                  end if;
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;

               --TEXT_IO.PUT_LINE("Returing from 2WDS  PA_SAVE+1 = " & INTEGER'IMAGE(PA_SAVE+1) & "  " & PA(PA_SAVE+1).STEM);

               return;
            else
               pa_last := pa_save;
            end if;

            i := i + 1;
         end loop outer_loop;

         pa_last := pa_save;   --  No success, so reset to clear the TRICK PA

         --  I could try to check cases/gender/number for matches
         --  Discard all that do not have a match
         --  ADJ, N, NUM
         --  But that is probably being too pedantic for a case which may be sloppy
      end two_words;

      --------------------------------------------------------------------------
      --------------------------------------------------------------------------
      --------------------------------------------------------------------------
      --------------------------------------------------------------------------

   begin
      --  These things might be genericized, at least the PA(1) assignments
      --TEXT_IO.PUT_LINE("TRICKS called");

      xxx_meaning := null_meaning_type;

      --  If there is no satisfaction from above, we will try further

      case s(s'first) is

         when 'a'  =>

            flip_flop("adgn", "agn");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("adsc", "asc");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("adsp", "asp");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("arqui",  "arci");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("arqu",  "arcu");
            if pa_last > 0  then
               return;
            end if;
            flip("ae",  "e");
            if pa_last > 0  then
               return;
            end if;
            flip("al",  "hal");
            if pa_last > 0  then
               return;
            end if;
            flip("am",  "ham");
            if pa_last > 0  then
               return;
            end if;
            flip("ar",  "har");
            if pa_last > 0  then
               return;
            end if;
            flip("aur",  "or");
            if pa_last > 0  then
               return;
            end if;

         when 'd'  =>

            flip("dampn" , "damn");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("dij"  , "disj");       --  OLD p.543
            if pa_last > 0  then
               return;
            end if;
            flip_flop("dir"  , "disr");       --  OLD p.556
            if pa_last > 0  then
               return;
            end if;
            flip_flop("dir"  , "der");        --  OLD p.547
            if pa_last > 0  then
               return;
            end if;
            flip_flop("del"  , "dil");        --  OLD p.507/543
            if pa_last > 0  then
               return;
            end if;

         when 'e'  =>

            flip_flop("ecf" , "eff");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("ecs" , "exs");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("es"  , "ess");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("ex"  , "exs");
            if pa_last > 0  then
               return;
            end if;

            flip("eid",  "id");
            if pa_last > 0  then
               return;
            end if;
            flip("el",  "hel");
            if pa_last > 0  then
               return;
            end if;
            flip("e",  "ae");
            if pa_last > 0  then
               return;
            end if;

         when 'f'  =>

            flip_flop("faen" , "fen");
            if pa_last > 0  then
               return;
            end if;

            flip_flop("faen" , "foen");
            if pa_last > 0  then
               return;
            end if;

            flip_flop("fed" , "foed");
            if pa_last > 0  then
               return;
            end if;

            flip_flop("fet" , "foet");
            if pa_last > 0  then
               return;
            end if;

            flip("f",  "ph");
            if pa_last > 0  then
               return;
            end if;  -- Try lead then all

         when 'g'  =>

            flip("gna",  "na");
            if pa_last > 0  then
               return;
            end if;

         when 'h'  =>

            flip("har",  "ar");
            if pa_last > 0  then
               return;
            end if;
            flip("hal",  "al");
            if pa_last > 0  then
               return;
            end if;
            flip("ham",  "am");
            if pa_last > 0  then
               return;
            end if;
            flip("hel",  "el");
            if pa_last > 0  then
               return;
            end if;
            flip("hol",  "ol");
            if pa_last > 0  then
               return;
            end if;
            flip("hum",  "um");
            if pa_last > 0  then
               return;
            end if;

         when 'i'  =>

            -- for some forms of eo the stem "i" grates with an "is..." ending
            if s'length > 1 and then
              s(s'first..s'first+1) = "is"   then
               pa(1) := ("Word mod is => iis", null_inflection_record,
                 xxx, null_mnpc);
               pa_last := 1;
               tword("i" & s(s'first..s'last), pa, pa_last);
            end if;
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  and then
              pa(pa_last).ir.qual.pofs = v and then
              pa(pa_last).ir.qual.v.con = (6, 1) then  --    Check it is V 6 1 eo
               xxx_meaning := head(
                 "Some forms of eo stem 'i' grates with an 'is...' ending, so 'is' -> 'iis' "
                 , max_meaning_size);
               return;
            else
               pa_last := 0;
            end if;

         when 'k'  =>

            flip("k",  "c");
            if pa_last > 0  then
               return;
            end if;
            flip("c",  "k");
            if pa_last > 0  then
               return;
            end if;

         when 'l'  =>

            flip_flop("lub", "lib");
            if pa_last > 1 then
               return;
            end if;

         when 'm'  =>

            flip_flop("mani", "manu");
            if pa_last > 1 then
               return;
            end if;

         when 'n'  =>

            flip("na",  "gna");
            if pa_last > 0  then
               return;
            end if;

            flip_flop("nihil",  "nil");
            if pa_last > 0  then
               return;
            end if;


         when 'o'  =>

            flip_flop("obt", "opt");
            if pa_last > 1 then
               return;
            end if;
            flip_flop("obs", "ops");
            if pa_last > 1 then
               return;
            end if;
            flip("ol",  "hol");
            if pa_last > 0  then
               return;
            end if;
            flip("opp", "op");
            if pa_last > 1 then
               return;
            end if;
            flip("or",  "aur");
            if pa_last > 0  then
               return;
            end if;

         when 'p'  =>

            flip("ph",  "f");
            if pa_last > 0  then
               return;
            end if;  -- Try lead then all
            flip_flop("pre", "prae");
            if pa_last > 1 then
               return;
            end if;

            --  when 'q'  =>


         when 's'  =>

            --  From Oxford Latin Dictionary p.1835 "sub-"

            --SLUR("sub");

            flip_flop("subsc",  "susc");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("subsp",  "susp");
            if pa_last > 0  then
               return;
            end if;

            flip_flop("subc",  "susc");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("succ",  "susc");
            if pa_last > 0  then
               return;
            end if;

            flip_flop("subt",  "supt");
            if pa_last > 0  then
               return;
            end if;
            flip_flop("subt",  "sust");
            if pa_last > 0  then
               return;
            end if;

         when 't'  =>

            flip_flop("transv",  "trav");
            if pa_last > 0  then
               return;
            end if;
            --            FLIP("trig",  "tric");
            --            if PA_LAST > 0  then

         when 'u'  =>

            flip("ul",  "hul");
            if pa_last > 0  then
               return;
            end if;
            flip("uol",  "vul");
            if pa_last > 0  then
               return;
            end if;  --  u is not v for this purpose

         when 'y'  =>

            flip("y",  "i");
            if pa_last > 0  then
               return;
            end if;

         when 'z'  =>

            flip("z",  "di");
            if pa_last > 0  then
               return;
            end if;

         when others  =>  null;

      end case;   --  case on first letter

      internal("ae",  "e");
      if pa_last > 0  then
         return;
      end if;

      internal("bul",  "bol");
      if pa_last > 0  then
         return;
      end if;
      internal("bol",  "bul");
      if pa_last > 0  then
         return;
      end if;

      internal("cl",  "cul");
      if pa_last > 0  then
         return;
      end if;

      internal("cu",  "quu");
      if pa_last > 0  then
         return;
      end if;

      internal("f",  "ph");
      if pa_last > 0  then
         return;
      end if;
      internal("ph",  "f");
      if pa_last > 0  then
         return;
      end if;

      internal("h",  "");
      if pa_last > 0  then
         return;
      end if;

      internal("oe",  "e");
      if pa_last > 0  then
         return;
      end if;

      internal("vul",  "vol");
      if pa_last > 0  then
         return;
      end if;
      internal("vol",  "vul");
      if pa_last > 0  then
         return;
      end if;
      internal("uol",  "vul");
      if pa_last > 0  then
         return;
      end if;

      adj_terminal_iis;
      if pa_last > 0  then
         return;
      end if;

      ---------------------------------------------------------------

      if words_mdev(do_medieval_tricks)  then
         --      Medieval  ->  Classic

         --  Harrington/Elliott    1.1.1

         internal("col",  "caul");
         if pa_last > 0  then
            return;
         end if;

         --TEXT_IO.PUT_LINE("Trying com -> con");


         --  Harrington/Elliott    1.3

         internal("e",  "ae");
         if pa_last > 0  then
            return;
         end if;

         internal("o",  "u");
         if pa_last > 0  then
            return;
         end if;

         internal("i",  "y");
         if pa_last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.3.1

         internal("ism",  "sm");
         if pa_last > 0  then
            return;
         end if;

         internal("isp",  "sp");
         if pa_last > 0  then
            return;
         end if;

         internal("ist",  "st");
         if pa_last > 0  then
            return;
         end if;

         internal("iz",  "z");
         if pa_last > 0  then
            return;
         end if;

         internal("esm",  "sm");
         if pa_last > 0  then
            return;
         end if;

         internal("esp",  "sp");
         if pa_last > 0  then
            return;
         end if;

         internal("est",  "st");
         if pa_last > 0  then
            return;
         end if;

         internal("ez",  "z");
         if pa_last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.4

         internal("di",  "z");
         if pa_last > 0  then
            return;
         end if;

         internal("f",  "ph");
         if pa_last > 0  then
            return;
         end if;

         internal("is",  "ix");
         if pa_last > 0  then
            return;
         end if;

         internal("b",  "p");
         if pa_last > 0  then
            return;
         end if;

         internal("d",  "t");
         if pa_last > 0  then
            return;
         end if;

         internal("v",  "b");
         if pa_last > 0  then
            return;
         end if;

         internal("v",  "f");
         if pa_last > 0  then
            return;
         end if;

         internal("v",  "f");
         if pa_last > 0  then
            return;
         end if;

         internal("s",  "x");
         if pa_last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.4.1

         internal("ci",  "ti");
         if pa_last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.4.2

         internal("nt",  "nct");
         if pa_last > 0  then
            return;
         end if;

         internal("s",  "ns");
         if pa_last > 0  then
            return;
         end if;

         --  Others

         internal("ch",  "c");
         if pa_last > 0  then
            return;
         end if;

         internal("c",  "ch");
         if pa_last > 0  then
            return;
         end if;

         internal("th",  "t");
         if pa_last > 0  then
            return;
         end if;

         internal("t",  "th");
         if pa_last > 0  then
            return;
         end if;

         double_consonants;

      end if;   --  Medieval Tricks
                ---------------------------------------------------------------

      if not (words_mode(ignore_unknown_names)  and capitalized)  then   --  Don't try on Names
         if words_mdev(do_two_words)  then
            two_words;
         end if;
      end if;

      --  It could be an improperly formed Roman Numeral
      if only_roman_digits(w)  then

         pa_last := 1;
         pa(1) := ("Bad Roman Numeral?", null_inflection_record,
           xxx, null_mnpc);
         xxx_meaning := null_meaning_type;

         rrr_meaning := head(integer'image(bad_roman_number(w)) & "  as ill-formed ROMAN NUMERAL?;",
           max_meaning_size);
         pa_last := pa_last + 1;
         pa(pa_last) := ( stem => head(w, max_stem_size),
           ir => (
           qual => (
           pofs => num,
           num => (
           decl   => (2, 0),
           cs     => x,
           number => x,
           gender => x,
           sort   => card) ),

           key => 0,
           ending => null_ending_record,
           age => x,
           freq => d),
           d_k => rrr,
           mnpc => null_mnpc         );

         return;
      end if;

   exception
      when others  =>    --  I want to ignore anything that happens in TRICKS
         pa_last := pa_save;
         pa(pa_last+1) := null_parse_record;     --  Just to clear the trys

         text_io.put_line(    --  ERROR_FILE,
           "Exception in TRY_TRICKS processing " & w);
   end try_tricks;

   procedure try_slury(w : string;
                       pa : in out parse_array; pa_last : in out integer;
                                                line_number : integer; word_number : integer) is
      --  Since the chances are 1/1000 that we have one,
      --  Ignore the possibility of two in the same word
      --  That is called lying with statistics
      s  : constant string(1..w'length) := w;
      pa_save : constant integer := pa_last;

      procedure tword(w : string;
                      pa : in out parse_array; pa_last : in out integer) is
         save_use_prefixes : constant boolean := words_mdev(use_prefixes);
      begin
         words_mdev(use_prefixes) := false;
         word_package.word(w, pa, pa_last);
         syncope(w, pa, pa_last);
         words_mdev(use_prefixes) := save_use_prefixes;
      end tword;

      procedure flip(x1, x2 : string; explanation : string := "") is
         --  At the begining of input word, replaces X1 by X2
         pa_save : constant integer := pa_last;
      begin
         if s'length >= x1'length+2  and then
           s(s'first..s'first+x1'length-1) = x1   then
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x2 & s(s'first+x1'length..s'last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               if explanation = ""  then
                  xxx_meaning := head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("SLURY   FLIP at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         pa_last := pa_save;
      end flip;

      procedure flip_flop(x1, x2 : string; explanation : string := "") is
         --  At the begining of input word, replaces X1 by X2 - then X2 by X1
         --  To be uesd only when X1 and X2 start with the same letter because it
         --  will be called from a point where the first letter is established
         pa_save : constant integer := pa_last;
      begin
         if s'length >= x1'length+2  and then
           s(s'first..s'first+x1'length-1) = x1   then
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x2 & s(s'first+x1'length..s'last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               if explanation = ""  then
                  xxx_meaning := head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("SLURY   FLOP at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;

         elsif s'length >= x2'length+2  and then
           s(s'first..s'first+x2'length-1) = x2   then
            pa_last := pa_last + 1;
            pa(pa_last) := (head("Word mod " & x2 & "/" & x1, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x1 & s(s'first+x2'length..s'last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  then
               if explanation = ""  then
                  xxx_meaning := head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := head(explanation, max_meaning_size);
               end if;
               put_stat("SLURY   FLOP at "
                 & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                 & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;

         end if;
         pa_last := pa_save;
      end flip_flop;

      procedure slur(x1 : string; explanation : string := "") is
         pa_save : constant integer := pa_last;
         sl : constant integer := x1'length;
      begin
         if s'length >= x1'length+2  then
            if s(s'first..s'first+x1'length-1) = x1   and then   --  Initial  X1
              not is_a_vowel(s(s'first+sl))           then
               pa_last := pa_last + 1;
               pa(pa_last)           := (head("Slur " & x1 & "/" & x1(x1'first..sl-1) & "~", max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(x1(x1'first..sl-1) & s(s'first+sl) & s(s'first+sl..s'last), pa, pa_last);
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)  then
                  if explanation = ""  then
                     xxx_meaning := head(
                       "An initial '" & x1 & "' may be rendered by " & x1(x1'first..x1'last-1) & "~",
                       max_meaning_size);
                  else
                     xxx_meaning := head(explanation, max_meaning_size);
                  end if;
                  put_stat("SLURY   SLUR at "
                    & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                    & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;

            elsif (s(s'first..s'first+sl-1) = x1(x1'first..sl-1))  and then
              (s(s'first+sl-1) = s(s'first+sl))   and then   --  double letter
              not is_a_vowel(s(s'first+sl))           then
               pa_last := pa_last + 1;
               pa(pa_last) := (head("Slur " & x1(x1'first..sl-1) & "~" & "/" & x1, max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(x1 & s(s'first+sl..s'last), pa, pa_last);
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)  then
                  if explanation = ""  then
                     xxx_meaning := head(
                       "An initial '" & x1(x1'first..sl-1) & "~" & "' may be rendered by " & x1
                       , max_meaning_size);
                  else
                     xxx_meaning := head(explanation, max_meaning_size);
                  end if;
                  put_stat("SLURY   SLUR at "
                    & head(integer'image(line_number), 8) & head(integer'image(word_number), 4)
                    & "   " & head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;

            end if;
         end if;
         pa_last := pa_save;
      end slur;

   begin

      --XXX_MEANING := NULL_MEANING_TYPE;

      --  If there is no satisfaction from above, we will try further

      if s(s'first) = 'a'  then

         flip_flop("abs", "aps");
         if pa_last > 0  then
            return;
         end if;
         flip_flop("acq", "adq");
         if pa_last > 0  then
            return;
         end if;
         flip_flop("ante",  "anti");
         if pa_last > 0  then
            return;
         end if;
         flip_flop("auri",  "aure");
         if pa_last > 0  then
            return;
         end if;
         flip_flop("auri",  "auru");
         if pa_last > 0  then
            return;
         end if;
         slur("ad");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'first) = 'c'  then

         flip("circum" , "circun");
         if pa_last > 0  then
            return;
         end if;
         flip_flop("con", "com");
         if pa_last > 0  then
            return;
         end if;
         flip("co" , "com");
         if pa_last > 0  then
            return;
         end if;
         flip("co" , "con");
         if pa_last > 0  then
            return;
         end if;
         flip_flop("conl" , "coll");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'first) = 'i'  then

         slur("in");
         if pa_last > 1 then
            return;
         end if;

         flip_flop("inb", "imb");
         if pa_last > 1 then
            return;
         end if;
         flip_flop("inp", "imp");
         if pa_last > 1 then
            return;
         end if;

         --    -- for some forms of eo the stem "i" grates with an "is..." ending


      elsif s(s'first) = 'n'  then



         flip("nun",  "non");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'first) = 'o'  then

         slur("ob");
         if pa_last > 0  then
            return;
         end if;


      elsif s(s'first) = 'q'  then

         flip_flop("quadri",  "quadru");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'first) = 's'  then

         flip("se",  "ce");     --  Latham
         if pa_last > 0  then
            return;
         end if;
         
         --  From Oxford Latin Dictionary p.1835 "sub-"

         slur("sub");

      end if;   --  if on first letter

   exception
      when others  =>    --  I want to ignore anything that happens in SLURY
         pa_last := pa_save;
         pa(pa_last+1) := null_parse_record;     --  Just to clear the trys

         text_io.put_line(    --  ERROR_FILE,
           "Exception in TRY_SLURY processing " & w);
   end try_slury;

end tricks_package;
