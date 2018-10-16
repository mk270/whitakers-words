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

--
-- This file needs a lot of work; details are in the comments at the bottom
-- of the file.
--

with Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Words_Engine.Word_Package; use Words_Engine.Word_Package;
with Words_Engine.Put_Stat;
with Words_Engine.Roman_Numerals_Package;
use Words_Engine.Roman_Numerals_Package;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Words_Engine.Trick_Tables; use Words_Engine.Trick_Tables;

package body Words_Engine.Tricks is

   function Is_A_Vowel (C : Character) return Boolean is
   begin
      case Lower_Case (C) is
         when 'a' | 'e' | 'i' | 'o' | 'u' | 'y' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_A_Vowel;

   procedure Roman_Numerals
     (Input_Word : String;
      Pa         : in out Parse_Array;
      Pa_Last    : in out Integer;
      Xp         : in out Explanations) is
   begin
      Roman_Numerals_Package.Roman_Numerals (Input_Word, Pa, Pa_Last, Xp);
   end Roman_Numerals;

   procedure Syncope (W       :        String;
                      Pa      : in out Parse_Array;
                      Pa_Last : in out Integer;
                      Xp      : in out Explanations)
   is
      S  : constant String (1 .. W'Length) := Lower_Case (W);
      Pa_Save : constant Integer := Pa_Last;
      Syncope_Inflection_Record : constant Inflection_Record :=
        Null_Inflection_Record;
      --     ((V, ((0, 0), (X, X, X), 0, X, X)), 0, NULL_ENDING_RECORD, X, A);

      procedure Explain_Syncope (Explanatory_Text, Stat_Text : String) is
      begin
         Xp.Yyy_Meaning := Head (Explanatory_Text, Max_Meaning_Size);
         Put_Stat (Stat_Text & Head (Integer'Image (Line_Number), 8) &
                               Head (Integer'Image (Word_Number), 4) &
           "   " & Head (W, 20) & "   " & Pa (Pa_Save + 1).Stem);
      end Explain_Syncope;
   begin

      --  Syncopated forms (see Gildersleeve and Lodge, 131)

      Xp.Yyy_Meaning := Null_Meaning_Type;

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
         Explain_Syncope
           ("Syncopated perfect ivi can drop 'v' without contracting vowel ",
           " SYNCOPE  ivi at ");
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      -- avis => as, evis => es, ivis => is, ovis => os   in perfect
      for I in reverse S'First .. S'Last - 2  loop     --  Need isse
         declare
            Fragment  : constant String  := S (I .. I + 1);
            Fragments : constant Strings := (+"as", +"es", +"is", +"os");
         begin
            if Member (+Fragment, Fragments)
            then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last)         :=
                 ("Syncope   s => vis", Syncope_Inflection_Record,
                 Yyy, Null_MNPC);
               Word (S (S'First .. I) & "vi" & S (I + 1 .. S'Last),
                 Pa, Pa_Last);
               if Pa_Last > Pa_Save + 1  then
                  exit;               --  Exit loop here if SYNCOPE found hit
               end if;
            end if;
            Pa_Last := Pa_Save;     --  No luck, or it would have exited above
         end;
      end loop;
      --  Loop over the resulting solutions
      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Explain_Syncope
           ("Syncopated perfect often drops the 'v' and contracts vowel ",
            "SYNCOPE  vis at ");
      end if;
      --  end loop;   --  over resulting solutions
      if Pa_Last > Pa_Save + 1  then
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      -- aver => ar, ever => er, in perfect
      for I in reverse S'First + 1 .. S'Last - 2  loop
         declare
            Fragment  : constant String  := S (I .. I + 1);
            Fragments : constant Strings := (+"ar", +"er", +"or");
         begin
            if Member (+Fragment, Fragments)
            then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last) := ("Syncope   r => v.r", Syncope_Inflection_Record,
                 Yyy, Null_MNPC);
               Word (S (S'First .. I) & "ve" & S (I + 1 .. S'Last),
                 Pa, Pa_Last);
               if Pa_Last > Pa_Save + 1  then
                  exit;
               end if;
            end if;
            Pa_Last := Pa_Save;     --  No luck, or it would have exited above
         end;
      end loop;

      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Explain_Syncope
           ("Syncopated perfect often drops the 'v' and contracts vowel ",
            "SYNCOPE  ver at ");
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
         Explain_Syncope
           ("Syncopated perfect often drops the 'v' and contracts vowel ",
            "SYNCOPE  ier at ");
         return;
      else
         Pa_Last := Pa_Save;
      end if;

      --         -- sis => s, xis => x, in perfect
      for I in reverse S'First .. S'Last - 2  loop
         declare
            Fragment : constant Character := S (I);
         begin
            if (Fragment = 's')  or
              (Fragment = 'x')
            then
               Pa_Last := Pa_Last + 1;
               Pa (Pa_Last)         :=
                 ("Syncope s/x => +is", Syncope_Inflection_Record,
                 Yyy, Null_MNPC);
               Word (S (S'First .. I) & "is" & S (I + 1 .. S'Last),
                 Pa, Pa_Last);
               if Pa_Last > Pa_Save + 1  then
                  exit;               --  Exit loop here if SYNCOPE found hit
               end if;
            end if;
            Pa_Last := Pa_Save;     --  No luck, or it would have exited above
         end;
      end loop;
      --  Loop over the resulting solutions
      if Pa_Last > Pa_Save + 1  and then
        Pa (Pa_Last).IR.Qual.Pofs = V and then
        Pa (Pa_Last).IR.Key = 3
      then          --  Perfect system
         Explain_Syncope
           ("Syncopated perfect sometimes drops the 'is' after 's' or 'x' ",
            "SYNCOPEx/sis at ");
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

      Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the tries

   exception
      when others  =>
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the tries

   end Syncope;

   procedure Try_Tricks
     (W           : String;
      Pa          : in out Parse_Array;
      Pa_Last     : in out Integer;
      Line_Number : Integer;
      Word_Number : Integer;
      Xp          : in out Explanations)
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
         Syncope (W, Pa, Pa_Last, Xp);
      end Tword;

      procedure Flip (X1, X2 : String; Explanation : String := "") is
         --  At the beginning of Input word, replaces X1 by X2
         Pa_Save : constant Integer := Pa_Last;
         Canned_Explanation : constant String := "' may have replaced usual '";
         Function_Id : constant String := "TRICK";
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
                  Xp.Xxx_Meaning := Head (
                    "An initial '" & X1 & Canned_Explanation & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat (Function_Id & "   FLIP at "
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
         --  At the beginning of Input word, replaces X1 by X2 - then X2 by X1
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
                  Xp.Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
                  Xp.Xxx_Meaning := Head (
                    "An initial '" & X2 & "' may be rendered by '" & X1 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
                     Xp.Xxx_Meaning := Head (
                       "An internal '" & X1 &
                       "' might be rendered by '" & X2 & "'"
                       , Max_Meaning_Size);
                  else
                     Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
                  Xp.Xxx_Meaning := Head
                    ("A Terminal 'iis' on ADJ 1 1 DAT/ABL P might drop 'i'",
                    Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
            --  probably don't need to go to end
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
                     Xp.Xxx_Meaning := Head (
                       "A doubled consonant may be rendered by just the single"
                       & "  MEDIEVAL", Max_Meaning_Size);
                  else
                     Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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

      begin
         --if S (S'FIRST) /= 'q'  then    --  qu words more complicated

         if S'Length  < 5  then    --  Don't try on too short words
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
               if not Common_Prefix (S (S'First .. S'First + I - 1)) then
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

                     Xp.Xxx_Meaning := Head (
                       "It is very likely a compound number    " &
                       S (S'First .. S'First + I - 1) & " + " &
                       S (S'First + I .. S'Last), Max_Meaning_Size);
                     Put_Stat ("TRICK   2NUM at "
                       & Head (Integer'Image (Line_Number), 8)
                       & Head (Integer'Image (Word_Number), 4)
                       & "   " & Head (W, 20) & "   "  &
                       S (1 .. I_Mid) & '+' & S (I_Mid + 1 .. S'Last));
                  else
                     Xp.Xxx_Meaning := Head (
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
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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

      -- FIXME: next two declarations (Finished and Iter_Tricks) duplicated
      -- entirely, pending reintegration
      Finished : Boolean := False;

      procedure Iter_Tricks (TT : TricksT)
      is
      begin
         for T in TT'Range loop
            case TT (T).Op is
               when TC_Flip_Flop =>
                  Flip_Flop (
                    To_String (TT (T).FF1),
                    To_String (TT (T).FF2));
               when TC_Flip =>
                  Flip (
                    To_String (TT (T).FF3),
                    To_String (TT (T).FF4));
               when TC_Internal =>
                  Internal (
                    To_String (TT (T).I1),
                    To_String (TT (T).I2));
               when TC_Slur =>
                  raise Tricks_Exception;
            end case;

            if Pa_Last > TT (T).Max then
               Finished := True;
               return;
            end if;
         end loop;

         Finished := False;
      end Iter_Tricks;

   begin
      --  These things might be genericized, at least the PA (1) assignments
      --TEXT_IO.PUT_LINE ("TRICKS called");

      Xp.Xxx_Meaning := Null_Meaning_Type;

      --  If there is no satisfaction from above, we will try further

      case S (S'First) is

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
               Xp.Xxx_Meaning := Head (
                 "Some forms of eo stem 'i' grates with " &
                 "an 'is .. .' ending, so 'is' -> 'iis' "
                 , Max_Meaning_Size);
               return;
            else
               Pa_Last := 0;
            end if;

         when 'a' | 'd' | 'e' | 'f' | 'g' | 'h' | 'k' | 'l' | 'm' | 'n' |
              'o' | 'p' | 's' | 't' | 'u' | 'y' | 'z' =>
            Iter_Tricks (Get_Tricks_Table (S (S'First)));
            if Finished then
               return;
            end if;
         when others =>
            null;

      end case;   --  case on first letter

      Iter_Tricks (Any_Tricks);
      if Finished then
         return;
      end if;

      Adj_Terminal_Iis;
      if Pa_Last > 0  then
         return;
      end if;

      ---------------------------------------------------------------

      if Words_Mdev (Do_Medieval_Tricks)  then
         --      Medieval  ->  Classic
         Iter_Tricks (Mediaeval_Tricks);
         if Finished then
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
         Xp.Xxx_Meaning := Null_Meaning_Type;

         Xp.Rrr_Meaning := Head (Integer'Image (Bad_Roman_Number (W))
           & "  as ill-formed ROMAN NUMERAL?;",
           Max_Meaning_Size);
         Pa_Last := Pa_Last + 1;
         Pa (Pa_Last) := (
           Stem => Head (W, Max_Stem_Size),
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
      when others  => --  I want to ignore anything that happens in TRICKS
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the tries

         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "Exception in TRY_TRICKS processing " & W);
   end Try_Tricks;

   procedure Try_Slury
     (W           : String;
      Pa          : in out Parse_Array;
      Pa_Last     : in out Integer;
      Line_Number : Integer;
      Word_Number : Integer;
      Xp          : in out Explanations)
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
         Syncope (W, Pa, Pa_Last, Xp);
         Words_Mdev (Use_Prefixes) := Save_Use_Prefixes;
      end Tword;

      procedure Flip (X1, X2 : String; Explanation : String := "") is
         --  At the beginning of Input word, replaces X1 by X2
         Pa_Save : constant Integer := Pa_Last;
         Canned_Explanation : constant String := "' may be rendered by '";
         Function_Id : constant String := "SLURY";
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
                  Xp.Xxx_Meaning := Head (
                    "An initial '" & X1 & Canned_Explanation & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
               end if;
               Put_Stat (Function_Id & "   FLIP at "
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
         --  At the beginning of Input word, replaces X1 by X2 - then X2 by X1
         --  To be used only when X1 and X2 start with the same letter because
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
                  Xp.Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
                  Xp.Xxx_Meaning := Head (
                    "An initial '" & X1 & "' may be rendered by '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
                     Xp.Xxx_Meaning := Head (
                       "An initial '" & X1 & "' may be rendered by "
                       & X1 (X1'First .. X1'Last - 1) & "~",
                       Max_Meaning_Size);
                  else
                     Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
                     Xp.Xxx_Meaning := Head (
                       "An initial '" & X1 (X1'First .. Sl - 1)
                       & "~" & "' may be rendered by " & X1
                       , Max_Meaning_Size);
                  else
                     Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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
      -- FIXME: next two declarations (Finished and Iter_Tricks) duplicated
      -- entirely, pending reintegration
      Finished : Boolean := False;

      procedure Iter_Tricks (TT : TricksT)
      is
      begin
         for T in TT'Range loop
            case TT (T).Op is
               when TC_Flip_Flop =>
                  Flip_Flop (
                    To_String (TT (T).FF1),
                    To_String (TT (T).FF2));
               when TC_Flip =>
                  Flip (
                    To_String (TT (T).FF3),
                    To_String (TT (T).FF4));
               when TC_Internal =>
                  raise Tricks_Exception;
               when TC_Slur =>
                  Slur (
                    To_String (TT (T).S1));
            end case;

            if Pa_Last > TT (T).Max then
               Finished := True;
               return;
            end if;
         end loop;

         Finished := False;
      end Iter_Tricks;

   begin

      --XXX_MEANING := NULL_MEANING_TYPE;

      --  If there is no satisfaction from above, we will try further

      case S (S'First) is

         when 'a' | 'c' | 'i' | 'n' | 'o' | 'q' | 's' =>
            Iter_Tricks (Get_Slur_Tricks_Table (S (S'First)));
            if Finished then
               return;
            end if;
         when others =>
            null;
      end case;   --  if on first letter

   exception
      when others  => --  I want to ignore anything that happens in SLURY
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the tries

         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "Exception in TRY_SLURY processing " & W);
   end Try_Slury;

end Words_Engine.Tricks;

-- Work remaining to be done:
--
--  * analyse all the things that can be factored back together
--  * factor out the 4 branches of Syncope ()
--  * brances of flip flop are almost identical
--  * there seem to be two copies of flip and flip flop
