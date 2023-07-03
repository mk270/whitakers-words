-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936-2010)
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
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;

package body Words_Engine.Roman_Numerals_Package is

   function A_Roman_Digit (Char : Character) return Boolean is
   begin
      case Char is
         when 'M' | 'm' | 'D' | 'd' | 'C' | 'c' | 'L' |
              'l' | 'X' | 'x' | 'V' | 'v' | 'I' | 'i'  =>
            return True;
         --when 'U' | 'u'  => return TRUE;  --  possible but unlikely
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
      Total : Natural := 0;
      Invalid : exception;
      S : constant String := Upper_Case (St);
      PrevNum : Natural := 0;

      function GetNumValue (C : Character) return Natural is
      begin
         case C is
            when 'I' => return 1;
            when 'V' => return 5;
            when 'X' => return 10;
            when 'L' => return 50;
            when 'C' => return 100;
            when 'D' => return 500;
            when 'M' => return 1000;
            when others => raise Invalid;
         end case;
      end GetNumValue;

   begin
      if not Only_Roman_Digits (S) then
         return 0;
      end if;

      for I in reverse S'Range loop
         declare
            C : Character := S(I);
            CValue : Natural := GetNumValue(C);
         begin
            if CValue < PrevNum then
               Total := Total - CValue;
            else
               Total := Total + CValue;
               PrevNum := CValue;
            end if;

            -- Roman numeral above 5000 are too rare to be valid
            if Total >= 5000 then
               raise Invalid;
            end if;

            if (C = 'V' or C = 'L' or C = 'D') and (I > S'First) then
               case S(I - 1) is
                  when 'I' => raise Invalid;
                  when 'V' => raise Invalid;
                  when 'X' => raise Invalid;
                  when 'L' => raise Invalid;
                  when 'C' => raise Invalid;
                  when 'D' => raise Invalid;
                  when 'M' => raise Invalid;
               end case;
            end if;
         end;
      end loop;

      return Total;

   exception
      when Invalid =>
         return 0;
      when Constraint_Error =>
         return 0;
   end Roman_Number;

   procedure Roman_Numerals
     (Input_Word : String;
      Pa : in out Parse_Array;
      Pa_Last : in out Integer;
      Xp : in out Explanations) is

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
         Xp.Rrr_Meaning := Head (Integer'Image (Roman_Number_W) &
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
end Words_Engine.Roman_Numerals_Package;
