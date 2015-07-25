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

with Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Word_Package; use Word_Package;
with Put_Stat;
package body Tricks_Package is

   function Is_A_Vowel (C : Character) return Boolean is
   begin
      if Lower_Case (C) = 'a'  or
        Lower_Case (C) = 'e'  or
        Lower_Case (C) = 'i'  or
        Lower_Case (C) = 'o'  or
        Lower_Case (C) = 'u'  or
        Lower_Case (C) = 'y'
      then
         return True;
      else
         return False;
      end if;
   end Is_A_Vowel;

   function A_Roman_Digit (Char : Character) return Boolean is
   begin
      case Char is
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
   end A_Roman_Digit;

   function Value (Char : Character) return Natural is
   begin
      case Char is
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
   end Value;

   function Only_Roman_Digits (S : String) return Boolean is
   begin

      for I in S'Range  loop
         if not A_Roman_Digit (S (I))  then
            return False;
         end if;
      end loop;
      return True;
   end Only_Roman_Digits;

   function Roman_Number (St : String) return Natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid

      use Ada.Text_IO;
      Total : Natural := 0;
      Invalid : exception;
      J : Integer := 0;
      S : constant String := Upper_Case (St);

   begin
      if Only_Roman_Digits (S)  then

         --
         --NUMERALS IN A STRING ARE ADDED: CC = 200 ; CCX = 210.
         --ONE NUMERAL TO THE LEFT of A LARGER NUMERAL IS SUBTRACTED FROM
         --THAT NUMBER: IX = 9
         --
         --SUBTRACT ONLY A SINGLE LETTER FROM A SINGLE NUMERAL.
         --VIII FOR 8, NOT IIX; 19 IS XIX, NOT IXX.
         --
         --SUBTRACT ONLY POWERS of TEN, SUCH AS I, X, or C.
         --NOT VL FOR 45, BUT XLV.
         --
         --DON'T SUBTRACT A LETTER FROM ANOTHER LETTER MORE THAN TEN TIMES
         --GREATER.
         --ONLY SUBTRACT I FROM V or X, and X FROM L or C.
         --NOT IL FOR 49, BUT XLIX. MIM is ILLEGAL.
         --
         --ONLY IF ANY NUMERAL PRECEEDING IS AT LEAST TEN TIMES LARGER.
         --NOT VIX FOR 14, BUT XIV.
         --NOT  IIX, BUT VIII.
         --ONLY IF ANY NUMERAL FOLLOWING IS SMALLER.
         --NOT XCL FOR 140, BUT CXL.
         --
         J := S'Last;

         Evaluate :
         while J >= S'First  loop
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
            if S (J) = 'I' then
               Total := Total + 1;
               J := J - 1;
               exit Evaluate when J < S'First;
               while S (J) = 'I'  loop
                  Total := Total + 1;
                  if Total >= 5  then
                     raise Invalid;
                  end if;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end loop;
            end if;

            if S (J) = 'V'  then
               Total := Total + 5;
               J := J - 1;
               exit Evaluate when J < S'First;
               if S (J) = 'I'  and Total = 5  then
                  Total := Total - 1;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;

               if S (J) = 'I' or S (J) = 'V'  then
                  raise Invalid;
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
            if S (J) = 'X'  then
               Total := Total + 10;
               J := J - 1;
               exit Evaluate when J < S'First;
               while S (J) = 'X'  loop
                  Total := Total + 10;
                  if Total >= 50 then
                     raise Invalid;
                  end if;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end loop;

               if S (J) = 'I'  and Total = 10  then
                  Total := Total - 1;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;

               if S (J) = 'I' or S (J) = 'V'  then
                  raise Invalid;
               end if;
            end if;

            if S (J) = 'L'  then
               Total := Total + 50;
               J := J - 1;
               exit Evaluate when J < S'First;

               if S (J) = 'X'  and Total <= 59  then
                  Total := Total - 10;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;

               if S (J) = 'I' or S (J) = 'V'
                 or S (J) = 'X'  or S (J) = 'L'
               then
                  raise Invalid;
               end if;

               if S (J) = 'C'  then
                  Total := Total + 100;
                  J := J - 1;
                  exit Evaluate when J < S'First;
                  if S (J) = 'X'  and Total = 100  then
                     Total := Total - 10;
                     J := J - 1;
                     exit Evaluate when J < S'First;
                  end if;
               end if;

               if S (J) = 'I' or S (J) = 'V'  or
                  S (J) = 'X'  or S (J) = 'L'
               then
                  raise Invalid;
               end if;
            end if;

            if S (J) = 'C'  then
               Total := Total + 100;
               J := J - 1;
               exit Evaluate when J < S'First;
               while S (J) = 'C'  loop
                  Total := Total + 100;
                  if Total >= 500  then
                     raise Invalid;
                  end if;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end loop;
               if S (J) = 'X'  and Total <= 109  then
                  Total := Total - 10;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;
               if S (J) = 'I' or S (J) = 'V'  or
                  S (J) = 'X' or S (J) = 'L'
               then
                  raise Invalid;
               end if;
            end if;

            if S (J) = 'D'  then
               Total := Total + 500;
               J := J - 1;
               exit Evaluate when J < S'First;
               if S (J) = 'C'  and Total <= 599  then
                  Total := Total - 100;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;
               if S (J) = 'M'  then
                  Total := Total + 1000;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;
               if S (J) = 'C'  and Total <= 1099  then
                  Total := Total - 100;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;
               if S (J) = 'I' or S (J) = 'V' or
                  S (J) = 'X' or S (J) = 'L' or S (J) = 'C' or S (J) = 'D'
               then
                  raise Invalid;
               end if;
            end if;

            if S (J) = 'M'  then
               Total := Total + 1000;
               J := J - 1;
               exit Evaluate when J < S'First;
               while S (J) = 'M'  loop
                  Total := Total + 1000;
                  if Total >= 5000  then
                     raise Invalid;
                  end if;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end loop;
               if S (J) = 'C'  and Total <= 1099  then
                  Total := Total - 100;
                  J := J - 1;
                  exit Evaluate when J < S'First;
               end if;
               if S (J) = 'I' or S (J) = 'V' or
                  S (J) = 'X' or S (J) = 'L' or S (J) = 'C' or S (J) = 'D'
               then
                  raise Invalid;
               end if;
            end if;
         end loop Evaluate;
      end if;  --  On Only Roman digits

      return Total;
   exception
      when Invalid  =>
         return 0;
      when Constraint_Error  =>
         return 0;
   end Roman_Number;

   procedure Roman_Numerals
     (Input_Word : String;
      Pa : in out Parse_Array;
      Pa_Last : in out Integer) is

      W : constant String := Trim (Input_Word);
      Roman_Number_W : constant Integer := Roman_Number (W);

   begin
      if Only_Roman_Digits (W) and then (Roman_Number_W /= 0)  then
         Pa_Last := Pa_Last + 1;
         Pa (Pa_Last) := (Stem => Head (W, Max_Stem_Size),
           IR => (
           Qual => (
           Pofs => Num,
           Num => (
           Decl    => (2, 0),
           Of_Case => X,
           Number  => X,
           Gender  => X,
           Sort    => Card)),

           Key => 0,
           Ending => Null_Ending_Record,
           Age => X,
           Freq => A),
           D_K => Rrr,
           MNPC => Null_MNPC);
         Rrr_Meaning := Head (Integer'Image (Roman_Number_W) &
           "  as a ROMAN NUMERAL;",
           Max_Meaning_Size);
      else
         null;    --  Is not ROMAN NUMERAL, so go on and try something else
      end if;
   end Roman_Numerals;

   function Bad_Roman_Number (S : String) return Natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid
      --  This seems to allow all of Caesar's.   Actually there are no rules
      --  if you look at some of the 12-15 century stuff
      use Ada.Text_IO;
      Total : Integer := 0;
      Decremented_From : Integer := 0;

   begin

      --  Already known that all the Characters may be valid numerals
      --  Loop over the String to check validity, start with second place
      --PUT_LINE (" In function BAD_ROMAN_NUMBER ");
      --PUT_LINE (" BEFORE LOOP      S = " & S);
      Total := Value (S (S'Last));
      Decremented_From := Value (S (S'Last));
      for I in reverse S'First .. S'Last - 1  loop

         if Value (S (I)) < Value (S (I + 1))  then
            --  Decrement
            Total := Total - Value (S (I));
            Decremented_From := Value (S (I + 1));
         elsif Value (S (I)) = Value (S (I + 1))  then
            if  Value (S (I)) < Decremented_From  then
               Total := Total - Value (S (I));   --  IIX = 8 !
            else
               Total := Total + Value (S (I));
            end if;
         elsif  Value (S (I)) > Value (S (I + 1))  then
            Total := Total + Value (S (I));
            Decremented_From := Value (S (I + 1));
         end if;
      end loop;
      if Total > 0  then
         return Total;
      else
         return 0;
      end if;

   exception
      when others  =>
         return 0;
   end Bad_Roman_Number;

   procedure Syncope (W : String;
                      Pa : in out Parse_Array; Pa_Last : in out Integer) is
      S  : constant String (1 .. W'Length) := Lower_Case (W);
      Pa_Save : constant Integer := Pa_Last;
      Syncope_Inflection_Record : constant Inflection_Record :=
        Null_Inflection_Record;
      --     ((V, ((0, 0), (X, X, X), 0, X, X)), 0, NULL_ENDING_RECORD, X, A);
   begin

      --  Syncopated forms (see Gildersleeve and Lodge, 131)

      Yyy_Meaning := Null_Meaning_Type;

      --  This one has to go first --  special for 3 4
      --  ivi  => ii ,  in perfect  (esp. for V 3 4)
      --  This is handled in WORDS as syncope
      --  It seems to appear in texts as alternative stems  ii and ivi
      for I in reverse S'First .. S'Last - 1  loop
         if S (I .. I + 1) = "ii" then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := ("Syncope  ii => ivi", Syncope_Inflection_Record,
              Yyy, Null_MNPC);
            Word (S (S'First .. I) & "v" & S (I + 1 .. S'Last), Pa, Pa_Last);
            if Pa_Last > Pa_Save + 1  then
               exit;
            end if;
         end if;
         Pa_Last := Pa_Save;     --  No luck, or it would have exited above
      end loop;
      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        --PA (PA_LAST).IR.QUAL.V.CON = (3, 4)/(6, 1) and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Yyy_Meaning := Head (
           "Syncopated perfect ivi can drop 'v' without contracting vowel "
           , Max_Meaning_Size);

         Put_Stat ("SYNCOPE  ivi at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      -- avis => as, evis => es, ivis => is, ovis => os   in perfect
      for I in reverse S'First .. S'Last - 2  loop     --  Need isse
         if (S (I .. I + 1) = "as")  or
           (S (I .. I + 1) = "es")  or
           (S (I .. I + 1) = "is")  or
           (S (I .. I + 1) = "os")
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last)         :=
              ("Syncope   s => vis", Syncope_Inflection_Record,
              Yyy, Null_MNPC);
            Word (S (S'First .. I) & "vi" & S (I + 1 .. S'Last), Pa, Pa_Last);
            if Pa_Last > Pa_Save + 1  then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         Pa_Last := Pa_Save;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Yyy_Meaning := Head (
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , Max_Meaning_Size);
         Put_Stat ("SYNCOPE  vis at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
      end if;
      --  end loop;   --  over resulting solutions
      if Pa_Last > Pa_Save + 1  then
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      -- aver => ar, ever => er, in perfect
      for I in reverse S'First + 1 .. S'Last - 2  loop
         if (S (I .. I + 1) = "ar")  or
           (S (I .. I + 1) = "er")  or
           (S (I .. I + 1) = "or")
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := ("Syncope   r => v.r", Syncope_Inflection_Record,
              Yyy, Null_MNPC);
            Word (S (S'First .. I) & "ve" & S (I + 1 .. S'Last), Pa, Pa_Last);
            if Pa_Last > Pa_Save + 1  then
               exit;
            end if;
         end if;
         Pa_Last := Pa_Save;     --  No luck, or it would have exited above
      end loop;

      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Yyy_Meaning := Head (
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , Max_Meaning_Size);

         Put_Stat ("SYNCOPE  ver at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      -- iver => ier,  in perfect
      for I in reverse S'First .. S'Last - 3  loop
         if S (I .. I + 2) = "ier" then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := ("Syncope  ier=>iver", Syncope_Inflection_Record,
              Yyy, Null_MNPC);
            Word (S (S'First .. I) & "v" & S (I + 1 .. S'Last), Pa, Pa_Last);
            if Pa_Last > Pa_Save + 1  then
               exit;
            end if;
         end if;
         Pa_Last := Pa_Save;     --  No luck, or it would have exited above
      end loop;
      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Yyy_Meaning := Head (
           "Syncopated perfect often drops the 'v' and contracts vowel "
           , Max_Meaning_Size);

         Put_Stat ("SYNCOPE  ier at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      --         -- sis => s, xis => x, in perfect
      for I in reverse S'First .. S'Last - 2  loop
         if (S (I) = 's')  or
           (S (I) = 'x')
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last)         :=
              ("Syncope s/x => +is", Syncope_Inflection_Record,
              Yyy, Null_MNPC);
            Word (S (S'First .. I) & "is" & S (I + 1 .. S'Last), Pa, Pa_Last);
            if Pa_Last > Pa_Save + 1  then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         Pa_Last := Pa_Save;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Yyy_Meaning := Head (
           "Syncopated perfect sometimes drops the 'is' after 's' or 'x' "
           , Max_Meaning_Size);
         Put_Stat ("SYNCOPEx/sis at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      --  end loop;   --  over resulting solutions
      if Pa_Last > Pa_Save + 1  then
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the trys

   exception
      when others  =>
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the trys

   end Syncope;

   procedure Try_Tricks
     (W           : String;
      Pa          : in out Parse_Array;
      Pa_Last     : in out Integer;
      Line_Number : Integer;
      Word_Number : Integer)
   is
      --  Since the chances are 1/1000 that we have one,
      --  Ignore the possibility of two in the same word
      --  That is called lying with statistics
      S  : constant String (1 .. W'Length) := W;
      Pa_Save : constant Integer := Pa_Last;

      procedure Tword (W : String;
                       Pa : in out Parse_Array; Pa_Last : in out Integer) is
      begin
         Word_Package.Word (W, Pa, Pa_Last);
         Syncope (W, Pa, Pa_Last);
      end Tword;

      procedure Flip (X1, X2 : String; Explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2
         Pa_Save : constant Integer := Pa_Last;
      begin
         if S'Length >= X1'Length + 2  and then
           S (S'First .. S'First + X1'Length - 1) = X1
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod " & X1 & "/" & X2, Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Tword (X2 & S (S'First + X1'Length .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               if Explanation = ""  then
                  Xxx_Meaning := Head (
                    "An initial '" & X1 &
                    "' may have replaced usual '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("TRICK   FLIP at "
                 & Head (Integer'Image (Line_Number), 8) &
                 Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;
         end if;
         Pa_Last := Pa_Save;
      end Flip;

      procedure Flip_Flop (X1, X2 : String; Explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2 - then X2 by X1
         --  To be used only when X1 and X2 start with the same letter because
         --  it will be called from a point where the first letter is
         --  established
         Pa_Save : constant Integer := Pa_Last;
      begin
         --TEXT_IO.PUT_LINE ("FLIP_FLOP called    " & X1 & "  " & X2);
         if S'Length >= X1'Length + 2  and then
           S (S'First .. S'First + X1'Length - 1) = X1
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod " & X1 & "/" & X2, Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Tword (X2 & S (S'First + X1'Length .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               --TEXT_IO.PUT_LINE ("FLIPF worked");
               if Explanation = ""  then
                  Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("TRICK  FLIPF at "
                 & Head (Integer'Image (Line_Number), 8) &
                 Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;
         end if;
         --TEXT_IO.PUT_LINE ("FLIPF failed");
         --TEXT_IO.PUT_LINE ("Try FFLOP");

         if S'Length >= X2'Length + 2  and then
           S (S'First .. S'First + X2'Length - 1) = X2
         then
            --TEXT_IO.PUT_LINE ("Trying FFLOP");
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod " & X2 & "/" & X1, Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Tword (X1 & S (S'First + X2'Length .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               --TEXT_IO.PUT_LINE ("FFLOP worked");
               if Explanation = ""  then
                  Xxx_Meaning := Head (
                    "An initial '" & X2 & "' may be rendered by '" & X1 & "'"
                    , Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("TRICK  FFLOP at "
                 & Head (Integer'Image (Line_Number), 8) &
                 Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;

         end if;
         --TEXT_IO.PUT_LINE ("FFLIP failed");
         Pa_Last := Pa_Save;
      end Flip_Flop;

      procedure Internal (X1, X2 : String; Explanation : String := "") is
         --  Replaces X1 with X2 anywhere in word and tries it for validity
         Pa_Save : constant Integer := Pa_Last;
      begin
         for I in S'First .. S'Last - X1'Length + 1  loop
            if S (I .. I + X1'Length - 1) = X1   then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last) :=
                 (Head ("Word mod " & X1 & "/" & X2, Max_Stem_Size),
                 Null_Inflection_Record,
                 Xxx, Null_MNPC);
               Tword (S (S'First .. I - 1) & X2 &
                 S (I + X1'Length .. S'Last), Pa, Pa_Last);
               if (Pa_Last > Pa_Save + 1)   and then
                 (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
               then
                  if Explanation = ""  then
                     Xxx_Meaning := Head (
                       "An internal '" & X1 &
                       "' might be rendered by '" & X2 & "'"
                       , Max_Meaning_Size);
                  else
                     Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
                  end if;
                  Put_Stat ("TRICK   INTR at "
                    & Head (Integer'Image (Line_Number), 8) &
                    Head (Integer'Image (Word_Number), 4)
                    & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
                  return;
               else
                  Pa_Last := Pa_Save;
               end if;
            end if;
         end loop;
         Pa_Last := Pa_Save;
      end Internal;

      procedure Adj_Terminal_Iis (Explanation : String := "") is
         Pa_Save : constant Integer := Pa_Last;
         I : Integer := 0;
      begin
         if S'Length > 3  and then
           S (S'Last - 1 .. S'Last) = "is"
         then   --  Terminal 'is'
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod iis -> is", Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Word (S (S'First .. S'Last - 2) & "iis", Pa, Pa_Last);
            if Pa_Last > Pa_Save + 1 then
               I := Pa_Last;
               while I > Pa_Save + 1  loop
                  if Pa (I).IR.Qual.Pofs = Adj  and then
                    Pa (I).IR.Qual.Adj.Decl = (1, 1)  and then
                    ((Pa (I).IR.Qual.Adj.Of_Case = Dat) or
                    (Pa (I).IR.Qual.Adj.Of_Case = Abl))   and then
                    Pa (I).IR.Qual.Adj.Number = P
                  then
                     null;       --  Only for ADJ 1 1 DAT/ABL P
                  else
                     Pa (I .. Pa_Last - 1) := Pa (I + 1 .. Pa_Last);
                     Pa_Last := Pa_Last - 1;
                  end if;
                  I := I - 1;
               end loop;
            end if;
            if Pa_Last > Pa_Save + 1 then
               if Explanation = ""  then
                  Xxx_Meaning := Head
                    ("A Terminal 'iis' on ADJ 1 1 DAT/ABL P might drop 'i'",
                    Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("TRICK  ADJIS at "
                 & Head (Integer'Image (Line_Number), 8)
                 & Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;
         end if;
         Pa_Last := Pa_Save;
      end Adj_Terminal_Iis;

      procedure Double_Consonants (Explanation : String := "") is
         Pa_Save : constant Integer := Pa_Last;
      begin
         --  Medieval often replaced a classical doubled consonant with single
         --  The problem is to take possible medieval words
         --  and double (all) (isolated) consonants
         for I in S'First + 1 .. S'Last - 1 loop
            --  probably dont need to go to end
            if (not Is_A_Vowel (S (I))) and then
              (Is_A_Vowel (S (I - 1)) and Is_A_Vowel (S (I + 1)))
            then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last)           := (Head ("Word mod " & S (I) &
                 " -> " & S (I) & S (I), Max_Stem_Size),
                 Null_Inflection_Record,
                 Xxx, Null_MNPC);
               Tword (S (S'First .. I) & S (I)
                 & S (I + 1 .. S'Last), Pa, Pa_Last);
               if (Pa_Last > Pa_Save + 1)   and then
                 (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
               then
                  if Explanation = ""  then
                     Xxx_Meaning := Head (
                       "A doubled consonant may be rendered by just the single"
                       & "  MEDIEVAL", Max_Meaning_Size);
                  else
                     Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
                  end if;
                  Put_Stat ("TRICK   2CON at "
                    & Head (Integer'Image (Line_Number), 8)
                    & Head (Integer'Image (Word_Number), 4)
                    & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
                  return;
               else
                  Pa_Last := Pa_Save;
               end if;

            end if;
         end loop;
         Pa_Last := Pa_Save;
      end Double_Consonants;

      procedure Two_Words (Explanation : String := "") is
         --  This procedure examines the word to determine if it is made up
         --  of two separate inflectted words
         --  They are usually an adjective and a noun or two nouns
         Pa_Save : constant Integer := Pa_Last;
         Pa_Second : Integer := Pa_Last;
         Num_Hit_One, Num_Hit_Two : Boolean := False;
         --MID : INTEGER := S'LENGTH/2;
         I, I_Mid : Integer := 0;
         Remember_Syncope : Boolean := False;
         procedure Words_No_Syncope
           (W       : String;
            Pa      : in out Parse_Array;
            Pa_Last : in out Integer)
         is
         begin
            if Words_Mdev (Do_Syncope)  then
               Remember_Syncope := True;
               Words_Mdev (Do_Syncope) := False;
            end if;
            Word_Package.Word (W, Pa, Pa_Last);
            if Remember_Syncope  then
               Words_Mdev (Do_Syncope) := True;
            end if;
         end Words_No_Syncope;

         function Common_Prefix (S : String) return Boolean is
            --  Common prefixes that have corresponding words (prepositions
            --  usually) which could confuse TWO_WORDS.  We wish to reject
            --  these.
         begin
            if S = "dis"  or
              S = "ex"   or
              S = "in"   or
              S = "per"  or
              S = "prae" or
              S = "pro"  or
              S = "re"   or
              S = "si"  or
              S = "sub"  or
              S = "super" or
              S = "trans"
            then
               return True;
            else
               return False;
            end if;
         end Common_Prefix;

      begin
         --if S (S'FIRST) /= 'q'  then    --  qu words more complicated

         if S'Length  < 5  then    --  Dont try on too short words
            return;
         end if;

         I := 2;
         --  Smallest is re-publica, but that killed by PREFIX, meipsum

         Outer_Loop :
         while I < S'Length - 2  loop

            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Two words", Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);

            while I < S'Length - 2  loop
               --TEXT_IO.PUT_LINE ("Trying  " & S (S'FIRST .. S'FIRST+I - 1));
               if not Common_Prefix (S (S'First .. S'First + I - 1))  then
                  Words_No_Syncope (S (S'First .. S'First + I - 1),
                    Pa, Pa_Last);
                  if Pa_Last > Pa_Save + 1 then
                     I_Mid := I;
                     for J in Pa_Save + 1 .. Pa_Last  loop
                        if Pa (J).IR.Qual.Pofs = Num  then
                           Num_Hit_One := True;
                           exit;
                        end if;
                     end loop;

                     exit;

                  end if;
               end if;
               I := I + 1;
            end loop;

            if Pa_Last > Pa_Save + 1 then
               null;
            else
               Pa_Last := Pa_Save;
               return;
            end if;

            --  Now for second word
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := Null_Parse_Record;     --  Separator
            Pa_Second := Pa_Last;
            Words_No_Syncope (S (I_Mid + 1 .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Second)   and then
              --  No + 1 since XXX taken care of above
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               for J in Pa_Second .. Pa_Last  loop
                  if Pa (J).IR.Qual.Pofs = Num  then
                     Num_Hit_Two := True;
                     exit;
                  end if;
               end loop;

               if Explanation = ""  then
                  if Words_Mode (Trim_Output)  and then
                    --  Should check that cases correspond
                    (Num_Hit_One and Num_Hit_Two)
                  then
                     --  Clear out any non-NUM if we are in TRIM
                     for J in Pa_Save + 1 .. Pa_Last  loop
                        if Pa (J).D_K in General .. Unique  and then
                          Pa (J).IR.Qual.Pofs /= Num
                        then
                           Pa (J .. Pa_Last - 1) := Pa (J + 1 .. Pa_Last);
                           Pa_Last := Pa_Last - 1;
                        end if;
                     end loop;

                     Xxx_Meaning := Head (
                       "It is very likely a compound number    " &
                       S (S'First .. S'First + I - 1) & " + " &
                       S (S'First + I .. S'Last), Max_Meaning_Size);
                     Put_Stat ("TRICK   2NUM at "
                       & Head (Integer'Image (Line_Number), 8)
                       & Head (Integer'Image (Word_Number), 4)
                       & "   " & Head (W, 20) & "   "  &
                       S (1 .. I_Mid) & '+' & S (I_Mid + 1 .. S'Last));
                  else
                     Xxx_Meaning := Head (
                       "May be 2 words combined (" &
                       S (S'First .. S'First + I - 1) & "+" &
                       S (S'First + I .. S'Last) &
                       ") If not obvious, probably incorrect",
                       Max_Meaning_Size);
                     Put_Stat ("TRICK   2WDS at "
                       & Head (Integer'Image (Line_Number), 8)
                       & Head (Integer'Image (Word_Number), 4)
                       & "   " & Head (W, 20) & "   "  &
                       S (1 .. I_Mid) & '+' & S (I_Mid + 1 .. S'Last));
                  end if;
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;

               return;
            else
               Pa_Last := Pa_Save;
            end if;

            I := I + 1;
         end loop Outer_Loop;

         Pa_Last := Pa_Save;   --  No success, so reset to clear the TRICK PA

         --  I could try to check cases/gender/number for matches
         --  Discard all that do not have a match
         --  ADJ, N, NUM
         --  But that is probably being too pedantic for a case which may be
         --  sloppy
      end Two_Words;

      --------------------------------------------------------------------------
      --------------------------------------------------------------------------
      --------------------------------------------------------------------------
      --------------------------------------------------------------------------

   begin
      --  These things might be genericized, at least the PA (1) assignments
      --TEXT_IO.PUT_LINE ("TRICKS called");

      Xxx_Meaning := Null_Meaning_Type;

      --  If there is no satisfaction from above, we will try further

      case S (S'First) is

         when 'a'  =>

            Flip_Flop ("adgn", "agn");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("adsc", "asc");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("adsp", "asp");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("arqui",  "arci");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("arqu",  "arcu");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("ae",  "e");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("al",  "hal");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("am",  "ham");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("ar",  "har");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("aur",  "or");
            if Pa_Last > 0  then
               return;
            end if;

         when 'd'  =>

            Flip ("dampn", "damn");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("dij", "disj");       --  OLD p.543
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("dir", "disr");       --  OLD p.556
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("dir", "der");        --  OLD p.547
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("del", "dil");        --  OLD p.507/543
            if Pa_Last > 0  then
               return;
            end if;

         when 'e'  =>

            Flip_Flop ("ecf", "eff");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("ecs", "exs");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("es", "ess");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("ex", "exs");
            if Pa_Last > 0  then
               return;
            end if;

            Flip ("eid",  "id");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("el",  "hel");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("e",  "ae");
            if Pa_Last > 0  then
               return;
            end if;

         when 'f'  =>

            Flip_Flop ("faen", "fen");
            if Pa_Last > 0  then
               return;
            end if;

            Flip_Flop ("faen", "foen");
            if Pa_Last > 0  then
               return;
            end if;

            Flip_Flop ("fed", "foed");
            if Pa_Last > 0  then
               return;
            end if;

            Flip_Flop ("fet", "foet");
            if Pa_Last > 0  then
               return;
            end if;

            Flip ("f",  "ph");
            if Pa_Last > 0  then
               return;
            end if;  -- Try lead then all

         when 'g'  =>

            Flip ("gna",  "na");
            if Pa_Last > 0  then
               return;
            end if;

         when 'h'  =>

            Flip ("har",  "ar");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("hal",  "al");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("ham",  "am");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("hel",  "el");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("hol",  "ol");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("hum",  "um");
            if Pa_Last > 0  then
               return;
            end if;

         when 'i'  =>

            -- for some forms of eo the stem "i" grates with an "is .. ." ending
            if S'Length > 1 and then
              S (S'First .. S'First + 1) = "is"
            then
               Pa (1) := ("Word mod is => iis", Null_Inflection_Record,
                 Xxx, Null_MNPC);
               Pa_Last := 1;
               Tword ("i" & S (S'First .. S'Last), Pa, Pa_Last);
            end if;
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)  and then
              Pa (Pa_Last).IR.Qual.Pofs = V and then
              Pa (Pa_Last).IR.Qual.Verb.Con = (6, 1)
            then  --    Check it is V 6 1 eo
               Xxx_Meaning := Head (
                 "Some forms of eo stem 'i' grates with " &
                 "an 'is .. .' ending, so 'is' -> 'iis' "
                 , Max_Meaning_Size);
               return;
            else
               Pa_Last := 0;
            end if;

         when 'k'  =>

            Flip ("k",  "c");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("c",  "k");
            if Pa_Last > 0  then
               return;
            end if;

         when 'l'  =>

            Flip_Flop ("lub", "lib");
            if Pa_Last > 1 then
               return;
            end if;

         when 'm'  =>

            Flip_Flop ("mani", "manu");
            if Pa_Last > 1 then
               return;
            end if;

         when 'n'  =>

            Flip ("na",  "gna");
            if Pa_Last > 0  then
               return;
            end if;

            Flip_Flop ("nihil",  "nil");
            if Pa_Last > 0  then
               return;
            end if;

         when 'o'  =>

            Flip_Flop ("obt", "opt");
            if Pa_Last > 1 then
               return;
            end if;
            Flip_Flop ("obs", "ops");
            if Pa_Last > 1 then
               return;
            end if;
            Flip ("ol",  "hol");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("opp", "op");
            if Pa_Last > 1 then
               return;
            end if;
            Flip ("or",  "aur");
            if Pa_Last > 0  then
               return;
            end if;

         when 'p'  =>

            Flip ("ph",  "f");
            if Pa_Last > 0  then
               return;
            end if;  -- Try lead then all
            Flip_Flop ("pre", "prae");
            if Pa_Last > 1 then
               return;
            end if;

            --  when 'q'  =>

         when 's'  =>

            --  From Oxford Latin Dictionary p.1835 "sub-"

            --SLUR ("sub");

            Flip_Flop ("subsc",  "susc");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("subsp",  "susp");
            if Pa_Last > 0  then
               return;
            end if;

            Flip_Flop ("subc",  "susc");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("succ",  "susc");
            if Pa_Last > 0  then
               return;
            end if;

            Flip_Flop ("subt",  "supt");
            if Pa_Last > 0  then
               return;
            end if;
            Flip_Flop ("subt",  "sust");
            if Pa_Last > 0  then
               return;
            end if;

         when 't'  =>

            Flip_Flop ("transv",  "trav");
            if Pa_Last > 0  then
               return;
            end if;
            --            FLIP ("trig",  "tric");
            --            if PA_LAST > 0  then

         when 'u'  =>

            Flip ("ul",  "hul");
            if Pa_Last > 0  then
               return;
            end if;
            Flip ("uol",  "vul");
            if Pa_Last > 0  then
               return;
            end if;  --  u is not v for this purpose

         when 'y'  =>

            Flip ("y",  "i");
            if Pa_Last > 0  then
               return;
            end if;

         when 'z'  =>

            Flip ("z",  "di");
            if Pa_Last > 0  then
               return;
            end if;

         when others  =>  null;

      end case;   --  case on first letter

      Internal ("ae",  "e");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("bul",  "bol");
      if Pa_Last > 0  then
         return;
      end if;
      Internal ("bol",  "bul");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("cl",  "cul");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("cu",  "quu");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("f",  "ph");
      if Pa_Last > 0  then
         return;
      end if;
      Internal ("ph",  "f");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("h",  "");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("oe",  "e");
      if Pa_Last > 0  then
         return;
      end if;

      Internal ("vul",  "vol");
      if Pa_Last > 0  then
         return;
      end if;
      Internal ("vol",  "vul");
      if Pa_Last > 0  then
         return;
      end if;
      Internal ("uol",  "vul");
      if Pa_Last > 0  then
         return;
      end if;

      Adj_Terminal_Iis;
      if Pa_Last > 0  then
         return;
      end if;

      ---------------------------------------------------------------

      if Words_Mdev (Do_Medieval_Tricks)  then
         --      Medieval  ->  Classic

         --  Harrington/Elliott    1.1.1

         Internal ("col",  "caul");
         if Pa_Last > 0  then
            return;
         end if;

         --TEXT_IO.PUT_LINE ("Trying com -> con");

         --  Harrington/Elliott    1.3

         Internal ("e",  "ae");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("o",  "u");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("i",  "y");
         if Pa_Last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.3.1

         Internal ("ism",  "sm");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("isp",  "sp");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("ist",  "st");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("iz",  "z");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("esm",  "sm");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("esp",  "sp");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("est",  "st");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("ez",  "z");
         if Pa_Last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.4

         Internal ("di",  "z");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("f",  "ph");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("is",  "ix");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("b",  "p");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("d",  "t");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("v",  "b");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("v",  "f");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("v",  "f");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("s",  "x");
         if Pa_Last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.4.1

         Internal ("ci",  "ti");
         if Pa_Last > 0  then
            return;
         end if;

         --  Harrington/Elliott    1.4.2

         Internal ("nt",  "nct");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("s",  "ns");
         if Pa_Last > 0  then
            return;
         end if;

         --  Others

         Internal ("ch",  "c");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("c",  "ch");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("th",  "t");
         if Pa_Last > 0  then
            return;
         end if;

         Internal ("t",  "th");
         if Pa_Last > 0  then
            return;
         end if;

         Double_Consonants;

      end if;

      --  Medieval Tricks
      ---------------------------------------------------------------

      if not (Words_Mode (Ignore_Unknown_Names) and Capitalized) then
      --  Don't try on Names
         if Words_Mdev (Do_Two_Words)  then
            Two_Words;
         end if;
      end if;

      --  It could be an improperly formed Roman Numeral
      if Only_Roman_Digits (W)  then

         Pa_Last := 1;
         Pa (1) := ("Bad Roman Numeral?", Null_Inflection_Record,
           Xxx, Null_MNPC);
         Xxx_Meaning := Null_Meaning_Type;

         Rrr_Meaning := Head (Integer'Image (Bad_Roman_Number (W))
           & "  as ill-formed ROMAN NUMERAL?;",
           Max_Meaning_Size);
         Pa_Last := Pa_Last + 1;
         Pa (Pa_Last) := (Stem => Head (W, Max_Stem_Size),
           IR => (
           Qual => (
           Pofs => Num,
           Num => (
           Decl    => (2, 0),
           Of_Case => X,
           Number  => X,
           Gender  => X,
           Sort    => Card)),

           Key => 0,
           Ending => Null_Ending_Record,
           Age => X,
           Freq => D),
           D_K => Rrr,
           MNPC => Null_MNPC);

         return;
      end if;

   exception
      when others  =>    --  I want to ignore anything that happens in TRICKS
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the trys

         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "Exception in TRY_TRICKS processing " & W);
   end Try_Tricks;

   procedure Try_Slury
     (W           : String;
      Pa          : in out Parse_Array;
      Pa_Last     : in out Integer;
      Line_Number : Integer;
      Word_Number : Integer)
   is
      --  Since the chances are 1/1000 that we have one,
      --  Ignore the possibility of two in the same word
      --  That is called lying with statistics
      S  : constant String (1 .. W'Length) := W;
      Pa_Save : constant Integer := Pa_Last;

      procedure Tword (W : String;
                       Pa : in out Parse_Array; Pa_Last : in out Integer) is
         Save_Use_Prefixes : constant Boolean := Words_Mdev (Use_Prefixes);
      begin
         Words_Mdev (Use_Prefixes) := False;
         Word_Package.Word (W, Pa, Pa_Last);
         Syncope (W, Pa, Pa_Last);
         Words_Mdev (Use_Prefixes) := Save_Use_Prefixes;
      end Tword;

      procedure Flip (X1, X2 : String; Explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2
         Pa_Save : constant Integer := Pa_Last;
      begin
         if S'Length >= X1'Length + 2  and then
           S (S'First .. S'First + X1'Length - 1) = X1
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod " & X1 & "/" & X2, Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Tword (X2 & S (S'First + X1'Length .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               if Explanation = ""  then
                  Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("SLURY   FLIP at "
                 & Head (Integer'Image (Line_Number), 8)
                 & Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;
         end if;
         Pa_Last := Pa_Save;
      end Flip;

      procedure Flip_Flop (X1, X2 : String; Explanation : String := "") is
         --  At the begining of Input word, replaces X1 by X2 - then X2 by X1
         --  To be uesd only when X1 and X2 start with the same letter because
         --  it will be called from a point where the first letter is
         --  established
         Pa_Save : constant Integer := Pa_Last;
      begin
         if S'Length >= X1'Length + 2  and then
           S (S'First .. S'First + X1'Length - 1) = X1
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod " & X1 & "/" & X2, Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Tword (X2 & S (S'First + X1'Length .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               if Explanation = ""  then
                  Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("SLURY   FLOP at "
                 & Head (Integer'Image (Line_Number), 8)
                 & Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;

         elsif S'Length >= X2'Length + 2  and then
           S (S'First .. S'First + X2'Length - 1) = X2
         then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Head ("Word mod " & X2 & "/" & X1, Max_Stem_Size),
              Null_Inflection_Record,
              Xxx, Null_MNPC);
            Tword (X1 & S (S'First + X2'Length .. S'Last), Pa, Pa_Last);
            if (Pa_Last > Pa_Save + 1)   and then
              (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
            then
               if Explanation = ""  then
                  Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat ("SLURY   FLOP at "
                 & Head (Integer'Image (Line_Number), 8)
                 & Head (Integer'Image (Word_Number), 4)
                 & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
               return;
            else
               Pa_Last := Pa_Save;
            end if;

         end if;
         Pa_Last := Pa_Save;
      end Flip_Flop;

      procedure Slur (X1 : String; Explanation : String := "") is
         Pa_Save : constant Integer := Pa_Last;
         Sl : constant Integer := X1'Length;
      begin
         if S'Length >= X1'Length + 2  then
            if S (S'First .. S'First + X1'Length - 1) = X1
              and then   --  Initial  X1
              not Is_A_Vowel (S (S'First + Sl))
            then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last)           :=
                 (Head ("Slur " & X1 & "/" & X1 (X1'First .. Sl - 1)
                 & "~", Max_Stem_Size),
                 Null_Inflection_Record,
                 Xxx, Null_MNPC);
               Tword (X1 (X1'First .. Sl - 1) & S (S'First + Sl)
                 & S (S'First + Sl .. S'Last), Pa, Pa_Last);
               if (Pa_Last > Pa_Save + 1)   and then
                 (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
               then
                  if Explanation = ""  then
                     Xxx_Meaning := Head (
                       "An initial '" & X1 & "' may be rendered by "
                       & X1 (X1'First .. X1'Last - 1) & "~",
                       Max_Meaning_Size);
                  else
                     Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
                  end if;
                  Put_Stat ("SLURY   SLUR at "
                    & Head (Integer'Image (Line_Number), 8)
                    & Head (Integer'Image (Word_Number), 4)
                    & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
                  return;
               else
                  Pa_Last := Pa_Save;
               end if;

            elsif (S (S'First .. S'First + Sl - 1) = X1 (X1'First .. Sl - 1))
              and then
              (S (S'First + Sl - 1) = S (S'First + Sl))
              and then   --  double letter
              not Is_A_Vowel (S (S'First + Sl))
            then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last) := (Head ("Slur " & X1 (X1'First .. Sl - 1)
                 & "~" & "/" & X1, Max_Stem_Size),
                 Null_Inflection_Record,
                 Xxx, Null_MNPC);
               Tword (X1 & S (S'First + Sl .. S'Last), Pa, Pa_Last);
               if (Pa_Last > Pa_Save + 1)   and then
                 (Pa (Pa_Last - 1).IR.Qual.Pofs /= Tackon)
               then
                  if Explanation = ""  then
                     Xxx_Meaning := Head (
                       "An initial '" & X1 (X1'First .. Sl - 1)
                       & "~" & "' may be rendered by " & X1
                       , Max_Meaning_Size);
                  else
                     Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
                  end if;
                  Put_Stat ("SLURY   SLUR at "
                    & Head (Integer'Image (Line_Number), 8)
                    & Head (Integer'Image (Word_Number), 4)
                    & "   " & Head (W, 20) & "   "  & Pa (Pa_Save + 1).Stem);
                  return;
               else
                  Pa_Last := Pa_Save;
               end if;

            end if;
         end if;
         Pa_Last := Pa_Save;
      end Slur;

   begin

      --XXX_MEANING := NULL_MEANING_TYPE;

      --  If there is no satisfaction from above, we will try further

      if S (S'First) = 'a'  then

         Flip_Flop ("abs", "aps");
         if Pa_Last > 0  then
            return;
         end if;
         Flip_Flop ("acq", "adq");
         if Pa_Last > 0  then
            return;
         end if;
         Flip_Flop ("ante",  "anti");
         if Pa_Last > 0  then
            return;
         end if;
         Flip_Flop ("auri",  "aure");
         if Pa_Last > 0  then
            return;
         end if;
         Flip_Flop ("auri",  "auru");
         if Pa_Last > 0  then
            return;
         end if;
         Slur ("ad");
         if Pa_Last > 0  then
            return;
         end if;

      elsif S (S'First) = 'c'  then

         Flip ("circum", "circun");
         if Pa_Last > 0  then
            return;
         end if;
         Flip_Flop ("con", "com");
         if Pa_Last > 0  then
            return;
         end if;
         Flip ("co", "com");
         if Pa_Last > 0  then
            return;
         end if;
         Flip ("co", "con");
         if Pa_Last > 0  then
            return;
         end if;
         Flip_Flop ("conl", "coll");
         if Pa_Last > 0  then
            return;
         end if;

      elsif S (S'First) = 'i'  then

         Slur ("in");
         if Pa_Last > 1 then
            return;
         end if;

         Flip_Flop ("inb", "imb");
         if Pa_Last > 1 then
            return;
         end if;
         Flip_Flop ("inp", "imp");
         if Pa_Last > 1 then
            return;
         end if;

         --  for some forms of eo the stem "i" grates with an "is .. ." ending

      elsif S (S'First) = 'n'  then

         Flip ("nun",  "non");
         if Pa_Last > 0  then
            return;
         end if;

      elsif S (S'First) = 'o'  then

         Slur ("ob");
         if Pa_Last > 0  then
            return;
         end if;

      elsif S (S'First) = 'q'  then

         Flip_Flop ("quadri",  "quadru");
         if Pa_Last > 0  then
            return;
         end if;

      elsif S (S'First) = 's'  then

         Flip ("se",  "ce");     --  Latham
         if Pa_Last > 0  then
            return;
         end if;

         --  From Oxford Latin Dictionary p.1835 "sub-"

         Slur ("sub");

      end if;   --  if on first letter

   exception
      when others  =>    --  I want to ignore anything that happens in SLURY
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the trys

         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "Exception in TRY_SLURY processing " & W);
   end Try_Slury;

end Tricks_Package;
