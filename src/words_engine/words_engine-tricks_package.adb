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

package body Words_Engine.Tricks_Package is
   type Strings is array (Integer range <>) of Unbounded_String;

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   function Member (Needle   : Unbounded_String;
                    Haystack : Strings)
                   return Boolean
   is
   begin
      for S in Haystack'Range loop
         if Needle = Haystack (S) then
            return True;
         end if;
      end loop;
      return False;
   end Member;

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
                    "An initial '" & X1 &
                    "' may have replaced usual '" & X2 & "'"
                    , Max_Meaning_Size);
               else
                  Xp.Xxx_Meaning := Head (Explanation, Max_Meaning_Size);
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

         function Common_Prefix (S : String) return Boolean is
            --  Common prefixes that have corresponding words (prepositions
            --  usually) which could confuse TWO_WORDS.  We wish to reject
            --  these.
            Common_Prefixes : constant Strings := (
              +"dis",
              +"ex",
              +"in",
              +"per",
              +"prae",
              +"pro",
              +"re",
              +"si",
              +"sub",
              +"super",
              +"trans"
              );
         begin
            return Member (+S, Common_Prefixes);
         end Common_Prefix;

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

      type Trick_Class is (TC_Flip_Flop, TC_Flip);
      type Trick (Op : Trick_Class := TC_Flip_Flop) is
        record
           Max : Integer := 0;
           case Op is
              when TC_Flip_Flop =>
                 FF1 : Unbounded_String := Null_Unbounded_String;
                 FF2 : Unbounded_String := Null_Unbounded_String;
              when TC_Flip =>
                 FF3 : Unbounded_String := Null_Unbounded_String;
                 FF4 : Unbounded_String := Null_Unbounded_String;
           end case;
        end record;
      type Tricks is array (Integer range <>) of Trick;

      A_Tricks : constant Tricks := (
        (Max => 0, Op => TC_Flip_Flop, FF1 => +"adgn",  FF2 => +"agn"),
        (Max => 0, Op => TC_Flip_Flop, FF1 => +"adsc",  FF2 => +"asc"),
        (Max => 0, Op => TC_Flip_Flop, FF1 => +"adsp",  FF2 => +"asp"),
        (Max => 0, Op => TC_Flip_Flop, FF1 => +"arqui", FF2 => +"arci"),
        (Max => 0, Op => TC_Flip_Flop, FF1 => +"arqu",  FF2 => +"arcu"),
        (Max => 0, Op => TC_Flip, FF3 => +"ae",    FF4 => +"e"),
        (Max => 0, Op => TC_Flip, FFx => "al", FFy => "hal"),
        (Max => 0, Op => TC_Flip, FFx => "am", FFy => "ham"),
        (Max => 0, Op => TC_Flip, FFx => "ar", FFy => "har"),
        (Max => 0, Op => TC_Flip, FFx => "aur", FFy => "or"),
      );

      Finished : Boolean := False;

      procedure Iter_Tricks (TT : Tricks)
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

         when 'a'  =>
            Iter_Tricks (A_Tricks);
            if Finished then
               return;
            end if;

         when 'd'  =>

            (Max => 0, Op => TC_Flip, FFx => "dampn", FFy =>"damn"),
            (Max => 0, Op => TC_Flip_Flop, FFx => "dij", FFy =>"disj"); --  OLD p.54,
            (Max => 0, Op => TC_Flip_Flop, FFx => "dir", FFy =>"disr"); --  OLD p.55,
            (Max => 0, Op => TC_Flip_Flop, FFx => "dir", FFy =>"der"); --  OLD p.54,
            (Max => 0, Op => TC_Flip_Flop, FFx => "del", FFy =>"dil"); --  OLD p.507/54,

         when 'e'  =>

            (Max => 0, Op => TC_Flip_Flop, FFx => "ecf", FFy =>"eff"),
            (Max => 0, Op => TC_Flip_Flop, FFx => "ecs", FFy =>"exs"),
            (Max => 0, Op => TC_Flip_Flop, FFx => "es", FFy =>"ess"),
            (Max => 0, Op => TC_Flip_Flop, FFx => "ex", FFy =>"exs"),

            (Max => 0, Op => TC_Flip, FFx => "eid", FFy => "id"),
            (Max => 0, Op => TC_Flip, FFx => "el", FFy => "hel"),
            (Max => 0, Op => TC_Flip, FFx => "e", FFy => "ae"),

         when 'f'  =>

            (Max => 0, Op => TC_Flip_Flop, FFx => "faen", FFy =>"fen"),

            (Max => 0, Op => TC_Flip_Flop, FFx => "faen", FFy =>"foen"),

            (Max => 0, Op => TC_Flip_Flop, FFx => "fed", FFy =>"foed"),

            (Max => 0, Op => TC_Flip_Flop, FFx => "fet", FFy =>"foet"),

            (Max => 0, Op => TC_Flip, FFx => "f", FFy => "ph"),
            -- Try lead then all

         when 'g'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "gna", FFy => "na"),

         when 'h'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "har", FFy => "ar"),
            (Max => 0  , Op =>            TC_Flip, FFx => "hal", FFy => "al"),
            (Max => 0  , Op =>            TC_Flip, FFx => "ham", FFy => "am"),
            (Max => 0  , Op =>            TC_Flip, FFx => "hel", FFy => "el"),
            (Max => 0  , Op =>            TC_Flip, FFx => "hol", FFy => "ol"),
            (Max => 0  , Op =>            TC_Flip, FFx => "hum", FFy => "um"),

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

         when 'k'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "k", FFy => "c"),
            (Max => 0  , Op =>            TC_Flip, FFx => "c", FFy => "k"),

         when 'l'  =>

            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "lub", FFy =>"lib"),

         when 'm'  =>

            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "mani", FFy =>"manu"),

         when 'n'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "na", FFy => "gna"),

            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "nihil", FFy => "nil"),

         when 'o'  =>

            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "obt", FFy =>"opt"),
            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "obs", FFy =>"ops"),
            (Max => 0  , Op =>            TC_Flip, FFx => "ol", FFy => "hol"),
            (Max => 1 , Op =>            TC_Flip, FFx => "opp", FFy =>"op"),
            (Max => 0  , Op =>            TC_Flip, FFx => "or", FFy => "aur"),

         when 'p'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "ph", FFy => "f"),
            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "pre", FFy =>"prae"),

            --  when 'q'  =>

         when 's'  =>

            --  From Oxford Latin Dictionary p.1835 "sub-"

            --SLUR ("sub");

            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "subsc", FFy => "susc"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "subsp", FFy => "susp"),

            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "subc", FFy => "susc"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "succ", FFy => "susc"),

            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "subt", FFy => "supt"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "subt", FFy => "sust"),

         when 't'  =>

            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "transv", FFy => "trav"),
            --            FLIP ("trig",  "tric");
            --            if PA_LAST > 0  then

         when 'u'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "ul", FFy => "hul"),
            (Max => 0  , Op =>            TC_Flip, FFx => "uol", FFy => "vul"),
            --  u is not v for this purpose

         when 'y'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "y", FFy => "i"),

         when 'z'  =>

            (Max => 0  , Op =>            TC_Flip, FFx => "z", FFy => "di"),

         when others  =>  null;

      end case;   --  case on first letter

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "ae", FFy => "e"),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "bul", FFy => "bol"),
      (Max => 0  , Op =>      InteTC_Rnal, FFx => "bol", FFy => "bul"),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "cl", FFy => "cul"),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "cu", FFy => "quu"),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "f", FFy => "ph"),
      (Max => 0  , Op =>      InteTC_Rnal, FFx => "ph", FFy => "f"),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "h", FFy => ""),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "oe", FFy => "e"),

      (Max => 0  , Op =>      InteTC_Rnal, FFx => "vul", FFy => "vol"),
      (Max => 0  , Op =>      InteTC_Rnal, FFx => "vol", FFy => "vul"),
      (Max => 0  , Op =>      InteTC_Rnal, FFx => "uol", FFy => "vul"),

      Adj_Terminal_Iis;
      if Pa_Last > 0  then
         return;
      end if;

      ---------------------------------------------------------------

      if Words_Mdev (Do_Medieval_Tricks)  then
         --      Medieval  ->  Classic

         --  Harrington/Elliott    1.1.1

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "col", FFy => "caul"),

         --TEXT_IO.PUT_LINE ("Trying com -> con");

         --  Harrington/Elliott    1.3

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "e", FFy => "ae"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "o", FFy => "u"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "i", FFy => "y"),

         --  Harrington/Elliott    1.3.1

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "ism", FFy => "sm"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "isp", FFy => "sp"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "ist", FFy => "st"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "iz", FFy => "z"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "esm", FFy => "sm"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "esp", FFy => "sp"),

         Internal         (Max => 0  , OTC_P =>, FFx => "est", FFy => "st"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "ez", FFy => "z"),

         --  Harrington/Elliott    1.4

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "di", FFy => "z"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "f", FFy => "ph"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "is", FFy => "ix"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "b", FFy => "p"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "d", FFy => "t"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "v", FFy => "b"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "v", FFy => "f"),

         Internal ("v",  "f");
         if Pa_Last > 0  then
            return;
         end if;

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "s", FFy => "x"),

         --  Harrington/Elliott    1.4.1

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "ci", FFy => "ti"),

         --  Harrington/Elliott    1.4.2

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "nt", FFy => "nct"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "s", FFy => "ns"),

         --  Others

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "ch", FFy => "c"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "c", FFy => "ch"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "th", FFy => "t"),

         (Max => 0  , Op =>         InteTC_Rnal, FFx => "t", FFy => "th"),

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
      when others  =>    --  I want to ignore anything that happens in TRICKS
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

   begin

      --XXX_MEANING := NULL_MEANING_TYPE;

      --  If there is no satisfaction from above, we will try further

      case S (S'First) is
         when 'a' =>
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "abs", FFy =>"aps"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "acq", FFy =>"adq"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "ante", FFy => "anti"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "auri", FFy => "aure"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "auri", FFy => "auru")

            Slur ("ad");
            if Pa_Last > 0  then
               return;
            end if;
         when 'c' =>
            (Max => 0  , Op =>            TC_Flip, FFx => "circum", FFy =>"circun"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "con", FFy =>"com"),
            (Max => 0  , Op =>            TC_Flip, FFx => "co", FFy =>"com"),
            (Max => 0  , Op =>            TC_Flip, FFx => "co", FFy =>"con"),
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "conl", FFy =>"coll"),
         when 'i' =>
            Slur ("in");
            if Pa_Last > 1 then
               return;
            end if;

            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "inb", FFy =>"imb"),
            (Max => 1 , Op =>            Flip_TC_Flop, FFx => "inp", FFy =>"imp"),
            -- for some forms of eo the stem "i" grates with an "is .. ." ending
         when 'n' =>
            (Max => 0  , Op =>            TC_Flip, FFx => "nun", FFy => "non"),
         when 'o' =>
            Slur ("ob");
            if Pa_Last > 0  then
               return;
            end if;
         when 'q' =>
            (Max => 0  , Op =>            Flip_TC_Flop, FFx => "quadri", FFy => "quadru"),
         when 's' =>
            (Max => 0  , Op =>            TC_Flip, FFx => "se", FFy => "ce");     --  Latha,
            --  From Oxford Latin Dictionary p.1835 "sub-"
            Slur ("sub");
         when others =>
            null;
      end case;   --  if on first letter

   exception
      when others  =>    --  I want to ignore anything that happens in SLURY
         Pa_Last := Pa_Save;
         Pa (Pa_Last + 1) := Null_Parse_Record;     --  Just to clear the tries

         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "Exception in TRY_SLURY processing " & W);
   end Try_Slury;

end Words_Engine.Tricks_Package;

-- Work remaining to be done:
--
--  * analyse all the things that can be factored back together
--  * factor out the 4 branches of Syncope ()
--  * brances of flip flop are almost identical
--  * move tabular data into own package
