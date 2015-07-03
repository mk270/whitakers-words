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
with word_parameters; use word_parameters;
with developer_parameters; use developer_parameters;
with inflections_package; use inflections_package;
with word_support_package; use word_support_package;
with word_package; use word_package;
with Put_stat;
package body tricks_package is

   function is_a_vowel(c : Character) return Boolean is
   begin
      if Lower_Case (c) = 'a'  or
        Lower_Case (c) = 'e'  or
        Lower_Case (c) = 'i'  or
        Lower_Case (c) = 'o'  or
        Lower_Case (c) = 'u'  or
        Lower_Case (c) = 'y'
      then
         return True;
      else
         return False;
      end if;
   end is_a_vowel;

   function a_roman_digit(char : Character) return Boolean is
   begin
      case char is
         when 'M' | 'm'  =>
            return True;
         when 'D' | 'd'  =>
            return True;
         when 'C' | 'c'  =>
            return True;
         when 'L' | 'l'  =>
            return True;
         when 'X' | 'x'  =>
            return True;
            --when 'U' | 'u'  => return TRUE;  --  possible but unlikely
         when 'V' | 'v'  =>
            return True;
         when 'I' | 'i'  =>
            return True;
         when others =>
            return False;
      end case;
   end a_roman_digit;

   function value(char : Character) return Natural is
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

   function only_roman_digits(s : String) return Boolean is
   begin

      for i in s'Range  loop
         if not a_roman_digit(s(i))  then
            return False;
         end if;
      end loop;
      return True;
   end only_roman_digits;

   function roman_number(st : String) return Natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid

      use Text_IO;
      total : Natural := 0;
      invalid : exception;
      j : Integer := 0;
      s : constant String := Upper_Case (st);

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
         j := s'Last;

         evaluate:
         while j >= s'First  loop
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
               exit evaluate when j < s'First;
               while s(j) = 'I'  loop
                  total := total + 1;
                  if total >= 5  then
                     raise invalid;
                  end if;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end loop;
            end if;

            if s(j) = 'V'  then
               total := total + 5;
               j := j - 1;
               exit evaluate when j < s'First;
               if s(j) = 'I'  and total = 5  then
                  total := total - 1;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;

               if s(j) = 'I' or s(j) = 'V'  then
                  raise invalid;
               end if;
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
               exit evaluate when j < s'First;
               while s(j) = 'X'  loop
                  total := total + 10;
                  if total >= 50 then
                     raise invalid;
                  end if;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end loop;

               if s(j) = 'I'  and total = 10  then
                  total := total - 1;
                     j := j - 1;
                     exit evaluate when j < s'First;
               end if;

               if s(j) = 'I' or s(j) = 'V'  then
                  raise invalid;
               end if;
            end if;

            if s(j) = 'L'  then
               total := total + 50;
               j := j - 1;
               exit evaluate when j < s'First;

               if s(j) = 'X'  and total <= 59  then
                  total := total - 10;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;

               if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L'  then
                  raise invalid;
               end if;

               if s(j) = 'C'  then
                  total := total + 100;
                  j := j - 1;
                  exit evaluate when j < s'First;
                  if s(j) = 'X'  and total = 100  then
                     total := total - 10;
                     j := j - 1;
                     exit evaluate when j < s'First;
                  end if;
               end if;

               if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L'  then
                  raise invalid;
               end if;
            end if;

            if s(j) = 'C'  then
               total := total + 100;
               j := j - 1;
               exit evaluate when j < s'First;
               while s(j) = 'C'  loop
                  total := total + 100;
                  if total >= 500  then
                     raise invalid;
                  end if;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end loop;
               if s(j) = 'X'  and total <= 109  then
                  total := total - 10;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;
               if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L'  then
                  raise invalid;
               end if;
            end if;

            if s(j) = 'D'  then
               total := total + 500;
               j := j - 1;
               exit evaluate when j < s'First;
               if s(j) = 'C'  and total <= 599  then
                  total := total - 100;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;
               if s(j) = 'M'  then
                  total := total + 1000;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;
               if s(j) = 'C'  and total <= 1099  then
                  total := total - 100;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;
               if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L' or s(j) = 'C' or s(j) = 'D'  then
                  raise invalid;
               end if;
            end if;

            if s(j) = 'M'  then
               total := total + 1000;
               j := j - 1;
               exit evaluate when j < s'First;
               while s(j) = 'M'  loop
                  total := total + 1000;
                  if total >= 5000  then
                     raise invalid;
                  end if;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end loop;
               if s(j) = 'C'  and total <= 1099  then
                  total := total - 100;
                  j := j - 1;
                  exit evaluate when j < s'First;
               end if;
               if s(j) = 'I' or s(j) = 'V'  or s(j) = 'X'  or s(j) = 'L' or s(j) = 'C' or s(j) = 'D'  then
                  raise invalid;
               end if;
            end if;
         end loop evaluate;
      end if;  --  On Only Roman digits

      return total;
   exception
      when invalid  =>
         return 0;
      when Constraint_Error  =>
         return 0;
   end roman_number;

   procedure roman_numerals(Input_word : String;
                            pa : in out parse_array; pa_last : in out Integer) is

      w : constant String := Trim (Input_word);
      roman_number_w : constant Integer := roman_number(w);

   begin
      if only_roman_digits(w) and then (roman_number_w /= 0)  then
         pa_last := pa_last + 1;
         pa(pa_last) := ( stem => Head(w, max_stem_size),
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
         rrr_meaning := Head(Integer'Image(roman_number_w) & "  as a ROMAN NUMERAL;",
           max_meaning_size);
      else
         null;    --  Is not ROMAN NUMERAL, so go on and try something else
      end if;
   end roman_numerals;

   function bad_roman_number(s : String) return Natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid
      --  This seems to allow all of Caesar's.   Actually there are no rules
      --  if you look at some of the 12-15 century stuff
      use Text_IO;
      total : Integer := 0;
      decremented_from : Integer := 0;

   begin

      --  Already known that all the Characters may be valid numerals
      --  Loop over the String to check validity, start with second place
      --PUT_LINE(" In function BAD_ROMAN_NUMBER ");
      --PUT_LINE(" BEFORE LOOP      S = " & S);
      total := value(s(s'Last));
      decremented_from := value(s(s'Last));
      for i in reverse s'First..s'Last-1  loop

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

   procedure syncope(w : String;
                     pa : in out parse_array; pa_last : in out Integer) is
      s  : constant String(1..w'Length) := Lower_Case (w);
      pa_save : constant Integer := pa_last;
      syncope_inflection_record : constant inflection_record := null_inflection_record;
      --     ((V, ((0, 0), (X, X, X), 0, X, X)), 0, NULL_ENDING_RECORD, X, A);
   begin

      --  Syncopated forms (see Gildersleeve and Lodge, 131)

      yyy_meaning := null_meaning_type;

      --  This one has to go first --  special for 3 4
      --  ivi  => ii ,  in perfect  (esp. for V 3 4)
      --  This is handled in WORDS as syncope
      --  It seems to appear in texts as alternative stems  ii and ivi
      for i in reverse s'First..s'Last-1  loop
         if s(i..i+1) = "ii" then
            pa_last := pa_last + 1;
            pa(pa_last) := ("Syncope  ii => ivi", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'First..i) & "v" & s(i+1..s'Last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        --PA(PA_LAST).IR.QUAL.V.CON = (3, 4)/(6, 1) and then
        pa(pa_last).ir.key = 3
      then          --  Perfect system
         yyy_meaning := Head(
           "Syncopated perfect ivi can drop 'v' without contracting vowel "
           , max_meaning_size);

         Put_stat("SYNCOPE  ivi at "
           & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
           & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      -- avis => as, evis => es, ivis => is, ovis => os   in perfect
      for i in reverse s'First..s'Last-2  loop     --  Need isse
         if (s(i..i+1) = "as")  or
            (s(i..i+1) = "es")  or
            (s(i..i+1) = "is")  or
            (s(i..i+1) = "os")
         then
            --TEXT_IO.PUT_LINE("SYNCOPE vis   S = " & S & "    PA_SAVE = " & INTEGER'IMAGE(PA_SAVE));
            pa_last := pa_last + 1;
            pa(pa_last)         := ("Syncope   s => vis", syncope_inflection_record,
              yyy, null_mnpc);
            --TEXT_IO.PUT_LINE("SYNCOPE vis   S+ = " & S(S'FIRST..I) & "vi" & S(I+1..S'LAST) & "  " & INTEGER'IMAGE(PA_LAST));
            word(s(s'First..i) & "vi" & s(i+1..s'Last), pa, pa_last);
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
        pa(pa_last).ir.key = 3
      then          --  Perfect system
         yyy_meaning := Head(
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , max_meaning_size);
         Put_stat("SYNCOPE  vis at "
           & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
           & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
      end if;
      --  end loop;   --  over resulting solutions
      if pa_last > pa_save + 1  then

         return;

      else
         pa_last := pa_save;
      end if;

      -- aver => ar, ever => er, in perfect
      for i in reverse s'First+1..s'Last-2  loop
         if (s(i..i+1) = "ar")  or
            (s(i..i+1) = "er")  or
            (s(i..i+1) = "or")
         then
            pa_last := pa_last + 1;
            pa(pa_last) := ("Syncope   r => v.r", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'First..i) & "ve" & s(i+1..s'Last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;

      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3
      then          --  Perfect system
         yyy_meaning := Head(
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , max_meaning_size);

         Put_stat("SYNCOPE  ver at "
           & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
           & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      -- iver => ier,  in perfect
      for i in reverse s'First..s'Last-3  loop
         if s(i..i+2) = "ier" then
            pa_last := pa_last + 1;
            pa(pa_last) := ("Syncope  ier=>iver", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'First..i) & "v" & s(i+1..s'Last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3
      then          --  Perfect system
         yyy_meaning := Head(
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , max_meaning_size);

         Put_stat("SYNCOPE  ier at "
           & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
           & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
         return;
      else
         pa_last := pa_save;
      end if;

      --         -- sis => s, xis => x, in perfect
      for i in reverse s'First..s'Last-2  loop
         if (s(i) = 's')  or
            (s(i) = 'x')
         then
            pa_last := pa_last + 1;
            pa(pa_last)         := ("Syncope s/x => +is", syncope_inflection_record,
              yyy, null_mnpc);
            word(s(s'First..i) & "is" & s(i+1..s'Last), pa, pa_last);
            if pa_last > pa_save + 1  then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         pa_last := pa_save;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if pa_last > pa_save + 1  and then
        pa(pa_last).ir.qual.pofs = v and then
        pa(pa_last).ir.key = 3
      then          --  Perfect system
         yyy_meaning := Head(
           "Syncopated perfect sometimes drops the 'is' after 's' or 'x' "
           , max_meaning_size);
         Put_stat("SYNCOPEx/sis at "
           & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
           & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
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

   procedure try_tricks(w : String;
                        pa : in out parse_array; pa_last : in out Integer;
                                                 line_number : Integer; word_number : Integer) is
      --  Since the chances are 1/1000 that we have one,
      --  Ignore the possibility of two in the same word
      --  That is called lying with statistics
      s  : constant String(1..w'Length) := w;
      pa_save : constant Integer := pa_last;

      procedure tword(w : String;
                      pa : in out parse_array; pa_last : in out Integer) is
      begin
         word_package.word(w, pa, pa_last);
         syncope(w, pa, pa_last);
      end tword;

      procedure flip(x1, x2 : String; explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2
         pa_save : constant Integer := pa_last;
      begin
         if s'Length >= x1'Length+2  and then
            s(s'First..s'First+x1'Length-1) = x1
         then
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x2 & s(s'First+x1'Length..s'Last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               if explanation = ""  then
                  xxx_meaning := Head(
                    "An initial '" & x1 & "' may have replaced usual '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("TRICK   FLIP at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         pa_last := pa_save;
      end flip;

      procedure flip_flop(x1, x2 : String; explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2 - then X2 by X1
         --  To be uesd only when X1 and X2 start with the same letter because it
         --  will be called from a point where the first letter is established
         pa_save : constant Integer := pa_last;
      begin
         --TEXT_IO.PUT_LINE("FLIP_FLOP called    " & X1 & "  " & X2);
         if s'Length >= x1'Length+2  and then
           s(s'First..s'First+x1'Length-1) = x1
         then
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            --TEXT_IO.PUT_LINE("Trying " & X2 & S(S'FIRST+X1'LENGTH..S'LAST));
            tword(x2 & s(s'First+x1'Length..s'Last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               --TEXT_IO.PUT_LINE("FLIPF worked");
               if explanation = ""  then
                  xxx_meaning := Head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("TRICK  FLIPF at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         --TEXT_IO.PUT_LINE("FLIPF failed");
         --TEXT_IO.PUT_LINE("Try FFLOP");

         if s'Length >= x2'Length+2  and then
           s(s'First..s'First+x2'Length-1) = x2
         then
            --TEXT_IO.PUT_LINE("Trying FFLOP");
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod " & x2 & "/" & x1, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            --TEXT_IO.PUT_LINE("Trying " & X1 & S(S'FIRST+X2'LENGTH..S'LAST));
            tword(x1 & s(s'First+x2'Length..s'Last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               --TEXT_IO.PUT_LINE("FFLOP worked");
               if explanation = ""  then
                  xxx_meaning := Head(
                    "An initial '" & x2 & "' may be rendered by '" & x1 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("TRICK  FFLOP at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;

         end if;
         --TEXT_IO.PUT_LINE("FFLIP failed");
         pa_last := pa_save;
      end flip_flop;

      procedure internal(x1, x2 : String; explanation : String := "") is
         --  Replaces X1 with X2 anywhere in word and tries it for validity
         pa_save : constant Integer := pa_last;
      begin
         for i in s'First..s'Last-x1'Length+1  loop
            if s(i..i+x1'Length-1) = x1   then
               pa_last := pa_last + 1;
               pa(pa_last) := (Head("Word mod " & x1 & "/" & x2, max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(s(s'First..i-1) & x2 & s(i+x1'Length..s'Last), pa, pa_last);
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)
               then
                  if explanation = ""  then
                     xxx_meaning := Head(
                       "An internal '" & x1 & "' might be rendered by '" & x2 & "'"
                       , max_meaning_size);
                  else
                     xxx_meaning := Head(explanation, max_meaning_size);
                  end if;
                  Put_stat("TRICK   INTR at "
                    & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                    & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;
            end if;
         end loop;
         pa_last := pa_save;
      end internal;

      procedure adj_terminal_iis(explanation : String := "") is
         pa_save : constant Integer := pa_last;
         i : Integer := 0;
      begin
         if s'Length > 3  and then
           s(s'Last-1..s'Last) = "is"
         then   --  Terminal 'is'
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod iis -> is", max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            word(s(s'First..s'Last-2) & "iis", pa, pa_last);
            if pa_last > pa_save + 1 then
               i := pa_last;
               while i > pa_save + 1  loop
                  if pa(i).ir.qual.pofs = adj  and then
                    pa(i).ir.qual.adj.decl = (1, 1)  and then
                    ((pa(i).ir.qual.adj.cs = dat) or
                    (pa(i).ir.qual.adj.cs = abl))   and then
                    pa(i).ir.qual.adj.number = p
                  then
                     null;       --  Only for ADJ 1 1 DAT/ABL P
                  else
                     pa(i..pa_last-1) := pa(i+1..pa_last);
                     pa_last := pa_last - 1;
                  end if;
                  i := i - 1;
               end loop;
            end if;
            if pa_last > pa_save + 1 then
               if explanation = ""  then
                  xxx_meaning := Head("A Terminal 'iis' on ADJ 1 1 DAT/ABL P might drop 'i'",
                    max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("TRICK  ADJIS at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         pa_last := pa_save;
      end adj_terminal_iis;

      procedure double_consonants(explanation : String := "") is
         pa_save : constant Integer := pa_last;
      begin
         --  Medieval often replaced a classical doubled consonant with single
         --  The problem is to take possible medieval words
         --  and double (all) (isolated) consonants
         for i in s'First+1..s'Last-1 loop  --  probably dont need to go to end
            if (not is_a_vowel(s(i))) and then
              (is_a_vowel(s(i-1)) and is_a_vowel(s(i+1)))
            then
               pa_last := pa_last + 1;
               pa(pa_last)           := (Head("Word mod " & s(i) &
                 " -> " & s(i) & s(i), max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(s(s'First..i) & s(i) & s(i+1..s'Last), pa, pa_last);
               --TEXT_IO.PUT_LINE(S(S'FIRST..I) & S(I) & S(I+1..S'LAST));
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)
               then
                  if explanation = ""  then
                     xxx_meaning := Head(
                       "A doubled consonant may be rendered by just the single"
                       & "  MEDIEVAL", max_meaning_size);
                  else
                     xxx_meaning := Head(explanation, max_meaning_size);
                  end if;
                  Put_stat("TRICK   2CON at "
                    & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                    & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;

            end if;
         end loop;
         pa_last := pa_save;
      end double_consonants;

      procedure two_words(explanation : String := "") is
         --  This procedure examines the word to determine if it is made up
         --  of two separate inflectted words
         --  They are usually an adjective and a noun or two nouns
         pa_save : constant Integer := pa_last;
         pa_second : Integer := pa_last;
         num_hit_one, num_hit_two : Boolean := False;
         --MID : INTEGER := S'LENGTH/2;
         i, i_mid : Integer := 0;
         remember_syncope : Boolean := False;
         procedure words_no_syncope (w : String;
                                     pa : in out parse_array; pa_last : in out Integer) is
         begin
            if words_mdev(do_syncope)  then
               remember_syncope := True;
               words_mdev(do_syncope) := False;
            end if;
            word_package.word(w, pa, pa_last);
            if remember_syncope  then
               words_mdev(do_syncope) := True;
            end if;
         end words_no_syncope;

         function common_prefix(s : String) return Boolean is
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
              s = "trans"
            then
               return True;
            else
               return False;
            end if;
         end common_prefix;

      begin
         --TEXT_IO.PUT_LINE("Entering TWO_WORDS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
         --if S(S'FIRST) /= 'q'  then    --  qu words more complicated

         if s'Length  < 5  then    --  Dont try on too short words
            return;
         end if;

         i := 2;    --  Smallest is re-publica, but that killed by PREFIX, meipsum

         outer_loop:
         while i < s'Length - 2  loop

            pa_last := pa_last + 1;
            pa(pa_last):= (Head("Two words", max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            --TEXT_IO.PUT_LINE("Setting PA TWO_WORDS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

            while i < s'Length - 2  loop
               --TEXT_IO.PUT_LINE("Trying  " & S(S'FIRST..S'FIRST+I-1));
               if not common_prefix(s(s'First..s'First+i-1))  then
                  words_no_syncope(s(s'First..s'First+i-1), pa, pa_last);
                  if pa_last > pa_save + 1 then
                     i_mid := i;
                     for j in pa_save+1..pa_last  loop
                        if pa(j).ir.qual.pofs = num  then
                           num_hit_one := True;
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

            if pa_last > pa_save + 1 then
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
            words_no_syncope(s(i_mid+1..s'Last), pa, pa_last);
            if (pa_last > pa_second)   and then       --  No + 1 since XXX taken care of above
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               for j in pa_second..pa_last  loop
                  if pa(j).ir.qual.pofs = num  then
                     num_hit_two := True;
                     exit;
                  end if;
               end loop;

               --TEXT_IO.PUT_LINE("Found       second  " & S(I_MID+1..S'LAST) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

               if explanation = ""  then
                  if words_mode(Trim_Output)  and then
                    --  Should check that cases correspond
                    (num_hit_one and num_hit_two)
                  then
                     --  Clear out any non-NUM if we are in TRIM
                     for j in pa_save+1..pa_last  loop
                        if pa(j).d_k in general..unique  and then
                          pa(j).ir.qual.pofs /= num
                        then
                           pa(j..pa_last-1) := pa(j+1..pa_last);
                           pa_last := pa_last - 1;
                        end if;
                     end loop;

                     xxx_meaning := Head(
                       "It is very likely a compound number    " &
                       s(s'First..s'First+i-1) & " + " &
                       s(s'First+i..s'Last), max_meaning_size);
                     Put_stat("TRICK   2NUM at "
                       & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                       & "   " & Head(w, 20) & "   "  & s(1..i_mid) & '+' & s(i_mid+1..s'Last));
                  else
                     xxx_meaning := Head(
                       "May be 2 words combined (" &
                       s(s'First..s'First+i-1) & "+" &
                       s(s'First+i..s'Last) &
                       ") If not obvious, probably incorrect", max_meaning_size);
                     Put_stat("TRICK   2WDS at "
                       & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                       & "   " & Head(w, 20) & "   "  & s(1..i_mid) & '+' & s(i_mid+1..s'Last));
                  end if;
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
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

      case s(s'First) is

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
            if s'Length > 1 and then
              s(s'First..s'First+1) = "is"
            then
               pa(1) := ("Word mod is => iis", null_inflection_record,
                 xxx, null_mnpc);
               pa_last := 1;
               tword("i" & s(s'First..s'Last), pa, pa_last);
            end if;
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)  and then
              pa(pa_last).ir.qual.pofs = v and then
              pa(pa_last).ir.qual.v.con = (6, 1)
            then  --    Check it is V 6 1 eo
               xxx_meaning := Head(
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

      end if;

      --  Medieval Tricks
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

         rrr_meaning := Head(Integer'Image(bad_roman_number(w)) & "  as ill-formed ROMAN NUMERAL?;",
           max_meaning_size);
         pa_last := pa_last + 1;
         pa(pa_last) := ( stem => Head(w, max_stem_size),
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

         Text_IO.Put_Line(    --  ERROR_FILE,
           "Exception in TRY_TRICKS processing " & w);
   end try_tricks;

   procedure try_slury(w : String;
                       pa : in out parse_array; pa_last : in out Integer;
                                                line_number : Integer; word_number : Integer) is
      --  Since the chances are 1/1000 that we have one,
      --  Ignore the possibility of two in the same word
      --  That is called lying with statistics
      s  : constant String(1..w'Length) := w;
      pa_save : constant Integer := pa_last;

      procedure tword(w : String;
                      pa : in out parse_array; pa_last : in out Integer) is
         save_use_prefixes : constant Boolean := words_mdev(use_prefixes);
      begin
         words_mdev(use_prefixes) := False;
         word_package.word(w, pa, pa_last);
         syncope(w, pa, pa_last);
         words_mdev(use_prefixes) := save_use_prefixes;
      end tword;

      procedure flip(x1, x2 : String; explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2
         pa_save : constant Integer := pa_last;
      begin
         if s'Length >= x1'Length+2  and then
           s(s'First..s'First+x1'Length-1) = x1
         then
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x2 & s(s'First+x1'Length..s'Last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               if explanation = ""  then
                  xxx_meaning := Head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("SLURY   FLIP at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;
         end if;
         pa_last := pa_save;
      end flip;

      procedure flip_flop(x1, x2 : String; explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2 - then X2 by X1
         --  To be uesd only when X1 and X2 start with the same letter because it
         --  will be called from a point where the first letter is established
         pa_save : constant Integer := pa_last;
      begin
         if s'Length >= x1'Length+2  and then
           s(s'First..s'First+x1'Length-1) = x1
         then
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod " & x1 & "/" & x2, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x2 & s(s'First+x1'Length..s'Last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               if explanation = ""  then
                  xxx_meaning := Head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("SLURY   FLOP at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;

         elsif s'Length >= x2'Length+2  and then
           s(s'First..s'First+x2'Length-1) = x2
         then
            pa_last := pa_last + 1;
            pa(pa_last) := (Head("Word mod " & x2 & "/" & x1, max_stem_size),
              null_inflection_record,
              xxx, null_mnpc);
            tword(x1 & s(s'First+x2'Length..s'Last), pa, pa_last);
            if (pa_last > pa_save + 1)   and then
              (pa(pa_last-1).ir.qual.pofs /= tackon)
            then
               if explanation = ""  then
                  xxx_meaning := Head(
                    "An initial '" & x1 & "' may be rendered by '" & x2 & "'"
                    , max_meaning_size);
               else
                  xxx_meaning := Head(explanation, max_meaning_size);
               end if;
               Put_stat("SLURY   FLOP at "
                 & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                 & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
               return;
            else
               pa_last := pa_save;
            end if;

         end if;
         pa_last := pa_save;
      end flip_flop;

      procedure slur(x1 : String; explanation : String := "") is
         pa_save : constant Integer := pa_last;
         sl : constant Integer := x1'Length;
      begin
         if s'Length >= x1'Length+2  then
            if s(s'First..s'First+x1'Length-1) = x1   and then   --  Initial  X1
              not is_a_vowel(s(s'First+sl))
            then
               pa_last := pa_last + 1;
               pa(pa_last)           := (Head("Slur " & x1 & "/" & x1(x1'First..sl-1) & "~", max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(x1(x1'First..sl-1) & s(s'First+sl) & s(s'First+sl..s'Last), pa, pa_last);
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)
               then
                  if explanation = ""  then
                     xxx_meaning := Head(
                       "An initial '" & x1 & "' may be rendered by " & x1(x1'First..x1'Last-1) & "~",
                       max_meaning_size);
                  else
                     xxx_meaning := Head(explanation, max_meaning_size);
                  end if;
                  Put_stat("SLURY   SLUR at "
                    & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                    & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
                  return;
               else
                  pa_last := pa_save;
               end if;

            elsif (s(s'First..s'First+sl-1) = x1(x1'First..sl-1))  and then
              (s(s'First+sl-1) = s(s'First+sl))   and then   --  double letter
              not is_a_vowel(s(s'First+sl))
            then
               pa_last := pa_last + 1;
               pa(pa_last) := (Head("Slur " & x1(x1'First..sl-1) & "~" & "/" & x1, max_stem_size),
                 null_inflection_record,
                 xxx, null_mnpc);
               tword(x1 & s(s'First+sl..s'Last), pa, pa_last);
               if (pa_last > pa_save + 1)   and then
                 (pa(pa_last-1).ir.qual.pofs /= tackon)
               then
                  if explanation = ""  then
                     xxx_meaning := Head(
                       "An initial '" & x1(x1'First..sl-1) & "~" & "' may be rendered by " & x1
                       , max_meaning_size);
                  else
                     xxx_meaning := Head(explanation, max_meaning_size);
                  end if;
                  Put_stat("SLURY   SLUR at "
                    & Head(Integer'Image(line_number), 8) & Head(Integer'Image(word_number), 4)
                    & "   " & Head(w, 20) & "   "  & pa(pa_save+1).stem);
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

      if s(s'First) = 'a'  then

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

      elsif s(s'First) = 'c'  then

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

      elsif s(s'First) = 'i'  then

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

      elsif s(s'First) = 'n'  then

         flip("nun",  "non");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'First) = 'o'  then

         slur("ob");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'First) = 'q'  then

         flip_flop("quadri",  "quadru");
         if pa_last > 0  then
            return;
         end if;

      elsif s(s'First) = 's'  then

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

         Text_IO.Put_Line(    --  ERROR_FILE,
           "Exception in TRY_SLURY processing " & w);
   end try_slury;

end tricks_package;
