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

---------------------------------------------------------------------------

-- This file contains an alarming amount of repetitive code, to the extent of
-- unintelligbility

-- to factor out:

--   the if-branches and functions dealing with participles/supines

--   anything nested more than about six levels deep

--   the subprogramme "parse_latin_word" is 300-odd lines long, and depends on
--   variables defined outside it and a bunch of variables from other
--   source files

--   the use of labels entails negatively indenting source lines, and is
--   a major readability/intelligibility problem

--  there are some instances of variables shadowed, e.g., "j" was
--  shadowed twice in nested declare blocks

with Ada.Text_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Addons_Package; use Support_Utils.Addons_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Word_Package; use Word_Package;
with List_Package; use List_Package;
with Tricks_Package; use Tricks_Package;
with Put_Stat;
with Search_English;
with Support_Utils.Char_Utils; use Support_Utils.Char_Utils;
use Latin_Utils;

pragma Elaborate (Support_Utils.Word_Parameters);
package body Parse
is
   use Inflections_Package.Integer_IO;
   use Inflection_Record_IO;
   use Ada.Text_IO;

   -- the scope of most of these variables is over-broad
   Storage_Error_Count : Integer := 0;

   J2, K : Integer := 0;

   Pa : Parse_Array (1 .. 100) := (others => Null_Parse_Record);
   Syncope_Max : constant := 20;
   No_Syncope : Boolean := False;
   Tricks_Max : constant := 40;
   Sypa : Parse_Array (1 .. Syncope_Max) := (others => Null_Parse_Record);
   Trpa : Parse_Array (1 .. Tricks_Max) := (others => Null_Parse_Record);
   Pa_Last, Sypa_Last, Trpa_Last : Integer := 0;

   type Participle is
      record
         Ppl_On : Boolean;
         Ppl_Info : Vpar_Record;
         Compound_Tvm : Inflections_Package.Tense_Voice_Mood_Record;
         Ppp_Meaning : Meaning_Type;
      end record;

   type Verb_To_Be (Matches : Boolean) is
      record
         case Matches is
            when True =>
               Verb_Rec : Verb_Record;
            when False =>
               null;
         end case;
      end record;

   function Is_Esse (T : String) return Boolean is
   begin
      return Trim (T) = "esse";
   end Is_Esse;

   function Is_Fuisse (T : String) return Boolean is
   begin
      return Trim (T) = "fuisse";
   end Is_Fuisse;

   function Is_Iri (T : String) return Boolean is
   begin
      return Trim (T) = "iri";
   end Is_Iri;

   function Get_Participle_Info (Vpar : Vpar_Record)
                                return Vpar_Record
   is
   begin
      return (
        Vpar.Con,     --  In this case, there is 1
        Vpar.Of_Case, --  although several different
        Vpar.Number,  --  dictionary entries may fit
        Vpar.Gender,  --  all have same PPL_INFO
        Vpar.Tense_Voice_Mood
             );
   end Get_Participle_Info;

   type Participle_Gloss is
      record
         Key : Inflections_Package.Tense_Voice_Mood_Record;
         Gloss : String (1 .. 78);
      end record;

   -- should merge the next three arrays
   type Participle_Glosses_Arr is array (Integer range <>) of Participle_Gloss;

   Participle_Glosses : constant Participle_Glosses_Arr :=
     (
     (Key => (Perf, Passive, Ppl),
     Gloss => "PERF PASSIVE PPL + verb TO_BE => " &
     "PASSIVE perfect system                       "),
     (Key => (Fut, Active,  Ppl),
     Gloss => "FUT ACTIVE PPL + verb TO_BE => " &
     "ACTIVE Periphrastic - about to, going to       "),
     (Key => (Fut, Passive, Ppl),
     Gloss => "FUT PASSIVE PPL + verb TO_BE => " &
     "PASSIVE Periphrastic - should/ought/had to    ")
     );

   Participle_Glosses_With_Esse : constant Participle_Glosses_Arr :=
     (
     (Key => (Perf, Passive, Ppl),
     Gloss => "PERF PASSIVE PPL + esse => " &
     "PERF PASSIVE INF                                   "),
     (Key => (Fut,  Active,  Ppl),
     Gloss => "FUT ACTIVE PPL + esse => " &
     "PRES Periphastic/FUT ACTIVE INF - be about/going to  "),
     (Key => (Fut,  Passive, Ppl),
     Gloss => "FUT PASSIVE PPL + esse => " &
     "PRES PASSIVE INF                                    ")
     );

   Participle_Glosses_With_Fuisse : constant Participle_Glosses_Arr :=
     (
     (Key => (Perf, Passive, Ppl),
     Gloss => "PERF PASSIVE PPL + esse => " &
     "PERF PASSIVE INF                                   "),
     (Key => (Fut,  Active,  Ppl),
     Gloss => "FUT ACT PPL+fuisse => " &
     "PERF ACT INF Periphrastic - to have been about/going to "),
     (Key => (Fut,  Passive, Ppl),
     Gloss => "FUT PASSIVE PPL + fuisse => " &
     "PERF PASSIVE INF Periphrastic - about to, going to")
     );

   -- we pass in the "default" values of a bunch of variables

   -- this function is called in a loop, which used to overWrite
   -- the values of this handful of variables, at least one of
   -- which is a global defined elsewhere

   -- retaining the "defaults" allows us to (re)assign the values
   -- in the caller, which is effectively a no-op in the fall-through
   -- code path; we should pass thie information in as a record of type
   -- "participle" rather than as four separate values, to be clearer
   -- about what is going on, and save wear-and-tear on the stack frame

   -- compare this function with the very similar one directly below it;
   -- they should be factored back toGether
   function Get_Pas_Nom_Participle
     (Parsed_Verb          : Vpar_Record;
      Sum_Info             : Verb_Record;
      Default_Ppl_On       : Boolean;
      Default_Compound_Tvm : Tense_Voice_Mood_Record;
      Default_Ppp_Meaning  : Meaning_Type;
      Default_Ppl_Info     : Vpar_Record)
     return Participle
   is
      Compound_Tense : Tense_Type := Pres;

      function Get_Compound_Tense (Tense : Tense_Type) return Tense_Type
      is
      begin
         case Tense is
            when Pres | Perf => return Perf; --  Allows PERF for sum
            when Impf | Plup => return Plup;
            when Fut         => return Futp;
            when others      => return X;
         end case;
      end Get_Compound_Tense;

   begin

      for I in Participle_Glosses'Range loop
         if Participle_Glosses (I).Key = Parsed_Verb.Tense_Voice_Mood then

            if Parsed_Verb.Tense_Voice_Mood = (Perf, Passive, Ppl) then
               Compound_Tense := Get_Compound_Tense (
                 Sum_Info.Tense_Voice_Mood.Tense);
            else
               Compound_Tense := Sum_Info.Tense_Voice_Mood.Tense;
            end if;

            return (
              Ppl_On => True,
              Ppl_Info => Get_Participle_Info (Parsed_Verb),
              Ppp_Meaning => Head (Participle_Glosses (I).Gloss,
              Max_Meaning_Size),
              Compound_Tvm => (Compound_Tense, Passive,
              Sum_Info.Tense_Voice_Mood.Mood)
                   );
         end if;
      end loop;

      return (
        Ppl_On => Default_Ppl_On,
        Ppl_Info => Default_Ppl_Info,
        Ppp_Meaning => Default_Ppp_Meaning,
        Compound_Tvm => Default_Compound_Tvm
             );
   end Get_Pas_Nom_Participle;

   -- this function should be merged with the one above
   function Get_Pas_Participle (Parsed_Verb : Vpar_Record;
                                Trimmed_Next_Word : String;
                                Default_Ppl_On : Boolean;
                                Default_Compound_Tvm : Tense_Voice_Mood_Record;
                                Default_Ppp_Meaning : Meaning_Type;
                                Default_Ppl_Info : Vpar_Record)
                               return Participle
   is
      function Get_Compound_Tense (Tense : Tense_Type;
                                   Voice : Voice_Type;
                                   Uses_Esse : Boolean) return Tense_Type
      is
      begin
         case Tense is
            when Fut =>
               case Uses_Esse is
                  when False => return Perf;
                  when others =>
                     case Voice is
                        when Active => return Fut;
                        when Passive => return Pres;
                        when X => return Fut;
                           -- shouldn't happen!
                           -- FIXME: remove 'x' member of voice enumeration
                     end case;
               end case;
            when others => return Tense;
         end case;
      end Get_Compound_Tense;

      Voice : constant Voice_Type := Parsed_Verb.Tense_Voice_Mood.Voice;
      Uses_Esse : constant Boolean := Is_Esse (Trimmed_Next_Word);
      Compound_Tense : Tense_Type;

   begin
      -- voice and mood are always as specified in parsed_verb.tense_voice_mood
      -- if tense is future, then there's a complicated thing to do

      for I in Participle_Glosses_With_Esse'Range loop
         if Participle_Glosses_With_Esse (I).Key =
           Parsed_Verb.Tense_Voice_Mood
         then
            declare
               Ppp_Meaning_S : String (1 .. 78);
            begin
               Compound_Tense := Get_Compound_Tense (
                 Parsed_Verb.Tense_Voice_Mood.Tense,
                 Parsed_Verb.Tense_Voice_Mood.Voice,
                 Uses_Esse);

               if Uses_Esse then
                  Ppp_Meaning_S := Participle_Glosses_With_Esse (I).Gloss;
               else
                  Ppp_Meaning_S := Participle_Glosses_With_Fuisse (I).Gloss;
               end if;

               return (
                 Ppl_On => True,
                 Ppl_Info => Get_Participle_Info (Parsed_Verb),
                 Ppp_Meaning => Head (Ppp_Meaning_S, Max_Meaning_Size),
                 Compound_Tvm => (Compound_Tense, Voice, Inf)
                      );
            end;
         end if;
      end loop;

      return (
        Ppl_On => Default_Ppl_On,
        Ppl_Info => Default_Ppl_Info,
        Ppp_Meaning => Default_Ppp_Meaning,
        Compound_Tvm => Default_Compound_Tvm
             );

   end Get_Pas_Participle;

   function Is_Sum (T : String) return Verb_To_Be is
      Sa : constant array (Mood_Type range Ind .. Sub,
        Tense_Type range Pres .. Futp,
        Number_Type range S .. P,
        Person_Type range 1 .. 3)
        of String (1 .. 9) :=
        (
        (         --  IND
        (("sum      ", "es       ", "est      "),
        ("sumus    ", "estis    ", "sunt     ")),
        (("eram     ", "eras     ", "erat     "),
        ("eramus   ", "eratis   ", "erant    ")),
        (("ero      ", "eris     ", "erit     "),
        ("erimus   ", "eritis   ", "erunt    ")),
        (("fui      ", "fuisti   ", "fuit     "),
        ("fuimus   ", "fuistis  ", "fuerunt  ")),
        (("fueram   ", "fueras   ", "fuerat   "),
        ("fueramus ", "fueratis ", "fuerant  ")),
        (("fuero    ", "fueris   ", "fuerit   "),
        ("fuerimus ", "fueritis ", "fuerunt  "))
        ),
        (         --  SUB
        (("sim      ", "sis      ", "sit      "),
        ("simus    ", "sitis    ", "sint     ")),
        (("essem    ", "esses    ", "esset    "),
        ("essemus  ", "essetis  ", "essent   ")),
        (("zzz      ", "zzz      ", "zzz      "),
        ("zzz      ", "zzz      ", "zzz      ")),
        (("fuerim   ", "fueris   ", "fuerit   "),
        ("fuerimus ", "fueritis ", "fuerint  ")),
        (("fuissem  ", "fuisses  ", "fuisset  "),
        ("fuissemus", "fuissetis", "fuissent ")),
        (("zzz      ", "zzz      ", "zzz      "),
        ("zzz      ", "zzz      ", "zzz      "))
        )
        );

   begin
      if T = ""  then
         return Verb_To_Be'(Matches => False);
      elsif T (T'First) /= 's'  and
        T (T'First) /= 'e'  and
        T (T'First) /= 'f'
      then
         return Verb_To_Be'(Matches => False);
      end if;
      for L in Mood_Type range Ind .. Sub  loop
         for K in Tense_Type range Pres .. Futp  loop
            for J in Number_Type range S .. P loop
               for I in Person_Type range 1 .. 3  loop
                  if Trim (T) = Trim (Sa (L, K, J, I))  then
                     return Verb_To_Be'(
                       Matches => True,
                       Verb_Rec => ((5, 1), (K, Active, L), I, J)
                                       );
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return Verb_To_Be'(
        Matches => False
                        );
   end Is_Sum;

   -- parts of these three do_clear_* functions should be factored together
   procedure Do_Clear_Pas_Nom_Ppl (Sum_Info : in Verb_Record;
                                   Compound_Tvm : out Tense_Voice_Mood_Record;
                                   Ppl_On : in out Boolean;
                                   Ppl_Info : out Vpar_Record)
   is
      J4 : Integer := Pa_Last;
   begin
      while J4 >= 1  loop
         --  Sweep backwards to kill empty suffixes
         if Pa (J4).IR.Qual.Pofs in Tackon .. Suffix
           and then Ppl_On
         then
            null;

         elsif Pa (J4).IR.Qual.Pofs = Vpar and then
           Pa (J4).IR.Qual.Vpar.Of_Case = Nom  and then
           Pa (J4).IR.Qual.Vpar.Number = Sum_Info.Number
         then
            declare
               Part : constant Participle :=
                 Get_Pas_Nom_Participle (Pa (J4).IR.Qual.Vpar, Sum_Info,
                 Ppl_On, Compound_Tvm, Ppp_Meaning, Ppl_Info);
            begin
               Ppl_On := Part.Ppl_On;
               Ppl_Info := Part.Ppl_Info;
               Ppp_Meaning := Part.Ppp_Meaning;
               Compound_Tvm := Part.Compound_Tvm;
            end;
         else
            Pa (J4 .. Pa_Last - 1) := Pa (J4 + 1 .. Pa_Last);
            Pa_Last := Pa_Last - 1;
            Ppl_On := False;
         end if;
         J4 := J4 - 1;
      end loop;
   end Do_Clear_Pas_Nom_Ppl;

   -- parts of these three do_clear_* functions should be factored together
   procedure Do_Clear_Pas_Ppl (Next_Word : in String;
                               Compound_Tvm : out Tense_Voice_Mood_Record;
                               Ppl_On : in out Boolean;
                               Ppl_Info : out Vpar_Record)
   is
      J5 : Integer := Pa_Last;
   begin
      while J5 >= 1  loop
         --  Sweep backwards to kill empty suffixes
         if Pa (J5).IR.Qual.Pofs in Tackon .. Suffix
           and then Ppl_On
         then
            null;
         elsif Pa (J5).IR.Qual.Pofs = Vpar   then
            declare
               Trimmed_Next_Word : constant String := Next_Word;
               Part : constant Participle :=
                 Get_Pas_Participle (Pa (J5).IR.Qual.Vpar,
                 Trimmed_Next_Word, Ppl_On, Compound_Tvm, Ppp_Meaning,
                 Ppl_Info);
            begin
               Ppl_On := Part.Ppl_On;
               Ppl_Info := Part.Ppl_Info;
               Ppp_Meaning := Part.Ppp_Meaning;
               Compound_Tvm := Part.Compound_Tvm;
            end;
         else
            Pa (J5 .. Pa_Last - 1) := Pa (J5 + 1 .. Pa_Last);
            Pa_Last := Pa_Last - 1;
            Ppl_On := False;
         end if;
         J5 := J5 - 1;
      end loop;
   end Do_Clear_Pas_Ppl;

   -- parts of these three do_clear_* functions should be factored together
   procedure Do_Clear_Pas_Supine (Supine_Info : out Supine_Record;
                                  Nk : in Integer;
                                  Ppl_On : in out Boolean)
   is
      J6 : Integer := Pa_Last;
   begin
      while J6 >= 1  loop
         --  Sweep backwards to kill empty suffixes
         if Pa (J6).IR.Qual.Pofs in Tackon .. Suffix
           and then Ppl_On
         then
            null;

         elsif Pa (J6).IR.Qual.Pofs = Supine  and then
           Pa (J6).IR.Qual.Supine.Of_Case = Acc
         then

            Ppl_On := True;
            Supine_Info := (Pa (J6).IR.Qual.Supine.Con,
              Pa (J6).IR.Qual.Supine.Of_Case,
              Pa (J6).IR.Qual.Supine.Number,
              Pa (J6).IR.Qual.Supine.Gender);

            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) :=
              (Head ("SUPINE + iri", Max_Stem_Size),
              ((V,
              (Supine_Info.Con,
              (Fut, Passive, Inf),
              0,
              X)
               ), 0, Null_Ending_Record, X, A),
              Ppp, Null_MNPC);
            Ppp_Meaning := Head (
              "SUPINE + iri => " &
              "FUT PASSIVE INF - to be about/going/ready to be ~",
              Max_Meaning_Size);

            K := Nk;

         else
            Pa (J6 .. Pa_Last - 1) := Pa (J6 + 1 .. Pa_Last);
            Pa_Last := Pa_Last - 1;
            Ppl_On := False;
         end if;
         J6 := J6  - 1;
      end loop;
   end Do_Clear_Pas_Supine;

   procedure Perform_Syncope (Input_Word : in String)
   is
   begin
      Sypa_Last := 0;
      if Words_Mdev (Do_Syncope) and not No_Syncope then
         Syncope (Input_Word, Sypa, Sypa_Last);

         --  Make syncope another array to avoid PA-LAST = 0 problems
         Pa_Last := Pa_Last + Sypa_Last;

         --  Add SYPA to PA
         Pa (1 .. Pa_Last) :=
           Pa (1 .. Pa_Last - Sypa_Last) & Sypa (1 .. Sypa_Last);

         --  Clean up so it does not repeat
         Sypa (1 .. Syncope_Max) := (1 .. Syncope_Max => Null_Parse_Record);
         Sypa_Last := 0;
      end if;
      No_Syncope := False;
   end Perform_Syncope;

   procedure Enclitic (Input_Word : String;
                       Entering_Pa_Last : in out Integer;
                       Have_Done_Enclitic : in out Boolean) is
      Save_Do_Only_Fixes : constant Boolean := Words_Mdev (Do_Only_Fixes);
      Enclitic_Limit : Integer := 4;
      Try : constant String := Lower_Case (Input_Word);
   begin
      if Have_Done_Enclitic  then
         return;
      end if;

      Entering_Pa_Last := Pa_Last;
      if Pa_Last > 0 then
         Enclitic_Limit := 1;
      end if;

      -- loop_over_enclitic_tackons:
      for I in 1 .. Enclitic_Limit  loop
         --  If have parse, only do que of que, ne, ve, (est)

         -- remove_a_tackon:
         declare
            Less : constant String := Subtract_Tackon (Try, Tackons (I));
            Save_Pa_Last  : Integer := 0;
         begin
            if Less  /= Try  then
               --  LESS is less
               --WORDS_MODE (DO_FIXES) := FALSE;
               Word_Package.Word (Less, Pa, Pa_Last);

               if Pa_Last = 0  then
                  Save_Pa_Last := Pa_Last;
                  Try_Slury (Less, Pa, Pa_Last, Line_Number, Word_Number);
                  if Save_Pa_Last /= 0   then
                     if (Pa_Last - 1) - Save_Pa_Last = Save_Pa_Last  then
                        Pa_Last := Save_Pa_Last;
                     end if;
                  end if;
               end if;

               --  Do not SYNCOPE if there is a verb TO_BE or compound
               --  already there; I do this here and below, it might be
               --  combined but it workd now
               for I in 1 .. Pa_Last  loop
                  if Pa (I).IR.Qual.Pofs = V and then
                    Pa (I).IR.Qual.Verb.Con = (5, 1)
                  then
                     No_Syncope := True;
                  end if;
               end loop;

               Perform_Syncope (Input_Word);

               --  Restore FIXES
               --WORDS_MODE (DO_FIXES) := SAVE_DO_FIXES;

               Words_Mdev (Do_Only_Fixes) := True;
               Word (Input_Word, Pa, Pa_Last);
               Words_Mdev (Do_Only_Fixes) := Save_Do_Only_Fixes;

               if Pa_Last > Entering_Pa_Last  then
                  --  have a possible word
                  Pa_Last := Pa_Last + 1;
                  Pa (Entering_Pa_Last + 2 .. Pa_Last) :=
                    Pa (Entering_Pa_Last + 1 .. Pa_Last - 1);
                  Pa (Entering_Pa_Last + 1) := (Tackons (I).Tack,
                    ((Tackon, Null_Tackon_Record), 0, Null_Ending_Record, X, X),
                    Addons, Dict_IO.Count (Tackons (I).MNPC));

                  Have_Done_Enclitic := True;
               end if;
               return;
            end if;
         end;
      end loop;
   end Enclitic;

   procedure Tricks_Enclitic (Input_Word : String;
                              Entering_Trpa_Last : in out Integer;
                              Have_Done_Enclitic : Boolean) is
      Try : constant String := Lower_Case (Input_Word);
   begin
      if Have_Done_Enclitic then
         return;
      end if;

      Entering_Trpa_Last := Trpa_Last;

      for I in 1 .. 4  loop   --  que, ne, ve, (est)

         declare
            Less : constant String :=
              Subtract_Tackon (Try, Tackons (I));
         begin
            if Less  /= Try  then       --  LESS is less
               Try_Tricks (Less, Trpa, Trpa_Last, Line_Number, Word_Number);

               if Trpa_Last > Entering_Trpa_Last  then
                  --  have a possible word
                  Trpa_Last := Trpa_Last + 1;
                  Trpa (Entering_Trpa_Last + 2 .. Trpa_Last) :=
                    Trpa (Entering_Trpa_Last + 1 .. Trpa_Last - 1);
                  Trpa (Entering_Trpa_Last + 1) := (Tackons (I).Tack,
                    ((Tackon, Null_Tackon_Record), 0, Null_Ending_Record, X, X),
                    Addons, Dict_IO.Count (Tackons (I).MNPC));
               end if;
               return;
            end if;
         end;
      end loop;
   end Tricks_Enclitic;

   procedure Pass (Input_Word : String;
                   Entering_Pa_Last : in out Integer;
                   Have_Done_Enclitic : in out Boolean)
   is
      --  This is the core logic of the program, everything else is details
      Save_Do_Fixes : constant Boolean := Words_Mode (Do_Fixes);
      Save_Do_Only_Fixes : constant Boolean := Words_Mdev (Do_Only_Fixes);
   begin
      --  Do straight WORDS without FIXES/TRICKS, is the word in the dictionary
      Words_Mode (Do_Fixes) := False;
      Roman_Numerals (Input_Word, Pa, Pa_Last);
      Word (Input_Word, Pa, Pa_Last);

      if Pa_Last = 0  then
         Try_Slury (Input_Word, Pa, Pa_Last, Line_Number, Word_Number);
      end if;

      --  Do not SYNCOPE if there is a verb TO_BE or compound already there
      for I in 1 .. Pa_Last  loop
         if Pa (I).IR.Qual.Pofs = V and then
           Pa (I).IR.Qual.Verb.Con = (5, 1)
         then
            No_Syncope := True;
         end if;
      end loop;

      --  Pure SYNCOPE
      Perform_Syncope (Input_Word);

      --  There may be a vaild simple parse, if so it is most probable
      --  But I have to allow for the possibility that -que is answer,
      --  not colloque V
      Enclitic (Input_Word, Entering_Pa_Last, Have_Done_Enclitic);

      --  Restore FIXES
      Words_Mode (Do_Fixes) := Save_Do_Fixes;

      --  Now with only fixes
      if Pa_Last = 0  and then Words_Mode (Do_Fixes) then
         Words_Mdev (Do_Only_Fixes) := True;
         Word (Input_Word, Pa, Pa_Last);

         Perform_Syncope (Input_Word);

         Enclitic (Input_Word, Entering_Pa_Last, Have_Done_Enclitic);

         Words_Mdev (Do_Only_Fixes) := Save_Do_Only_Fixes;
      end if;
   end Pass;

   procedure Parse_English_Word (Input_Word : in String;
                                 Line : in String;
                                 K : in Integer;
                                 L : in out Integer)
   is
      Pofs : Part_Of_Speech_Type := X;
   begin
      --  Extract from the rest of the line
      --  Should do AUX here !!!!!!!!!!!!!!!!!!!!!!!!
      --extract_pofs:
      begin
         Part_Of_Speech_Type_IO.Get (Line (K + 1 .. L), Pofs, L);
      exception
         when others => Pofs := X;
      end;

      Search_English (Input_Word, Pofs);
   end Parse_English_Word;

   procedure Parse_Latin_Word
     (Configuration : in Configuration_Type;
      Input_Word    : in String;
      Line          : in String;
      Input_Line    : in String;
      L             : in Integer)

   is
      Entering_Pa_Last : Integer := 0;
      Entering_Trpa_Last    : Integer := 0;
      Have_Done_Enclitic : Boolean := False;
   begin   --  PARSE
      Xxx_Meaning := Null_Meaning_Type;

      Pa_Last := 0;
      Word_Number := Word_Number + 1;

      Pass (Input_Word, Entering_Pa_Last, Have_Done_Enclitic);

      --if (PA_LAST = 0) or DO_TRICKS_ANYWAY  then
      --  WORD failed, try to modify the word
      if (Pa_Last = 0)  and then
        not (Words_Mode (Ignore_Unknown_Names)  and Capitalized)
      then
         --  WORD failed, try to modify the word
         if Words_Mode (Do_Tricks)  then
            Words_Mode (Do_Tricks) := False;
            --  Turn it off so wont be circular
            Try_Tricks (Input_Word, Trpa, Trpa_Last, Line_Number, Word_Number);
            if Trpa_Last = 0  then
               Tricks_Enclitic (Input_Word, Entering_Trpa_Last,
                 Have_Done_Enclitic);
            end if;
            Words_Mode (Do_Tricks) := True;   --  Turn it back on
         end if;

         Pa_Last := Pa_Last + Trpa_Last;
         --  Make TRICKS another array to avoid PA-LAST = 0 problems
         Pa (1 .. Pa_Last) :=
           Pa (1 .. Pa_Last - Trpa_Last) & Trpa (1 .. Trpa_Last);
         --  Add SYPA to PA
         Trpa (1 .. Tricks_Max) :=
           (1 .. Tricks_Max => Null_Parse_Record);
         --  Clean up so it does not repeat
         Trpa_Last := 0;

      end if;
      --  At this point we have done what we can with individual words
      --  Now see if there is something we can do with word combinations
      --  For this we have to look ahead

      if Pa_Last > 0   then
         --  But PA may be killed by ALLOW in LIST_STEMS
         if Words_Mode (Do_Compounds)  and
           not (Configuration = Only_Meanings)
         then

            Compounds_With_Sum :
            declare
               Nw : String (1 .. 2500) := (others => ' ');
               Nk : Integer := 0;

               Compound_Tvm   : Inflections_Package.Tense_Voice_Mood_Record;
               Ppl_On : Boolean := False;

               Sum_Info : Verb_Record := ((5, 1), (X, Active, X), 0, X);
               Ppl_Info : Vpar_Record := ((0, 0), X, X, X, (X, X, X));
               Supine_Info : Supine_Record := ((0, 0), X, X, X);

               procedure Look_Ahead is
                  J3 : Integer := 0;
               begin
                  for I in K + 2 .. L  loop
                     --  Although I have removed punctuation above,
                     --  it may not always be so
                     exit when Is_Punctuation (Line (I));
                     J3 := J3 + 1;
                     Nw (J3) := Line (I);
                     Nk := I;
                  end loop;
               end Look_Ahead;

               function Next_Word return String is
               begin
                  return Trim (Nw);
               end Next_Word;

               Is_Verb_To_Be : Boolean := False;

            begin

               --  Look ahead for sum
               Look_Ahead;

               declare
                  Tmp : constant Verb_To_Be := Is_Sum (Next_Word);
               begin
                  case Tmp.Matches is
                     when True => Sum_Info := Tmp.Verb_Rec;
                     when False => null;
                  end case;
                  Is_Verb_To_Be := Tmp.Matches;
               end;

               if Is_Verb_To_Be then
                  --  On NEXT_WORD = sum, esse, iri

                  for I in 1 .. Pa_Last  loop    --  Check for PPL
                     if Pa (I).IR.Qual.Pofs = Vpar and then
                       Pa (I).IR.Qual.Vpar.Of_Case = Nom  and then
                       Pa (I).IR.Qual.Vpar.Number = Sum_Info.Number  and then
                       ((Pa (I).IR.Qual.Vpar.Tense_Voice_Mood =
                       (Perf, Passive, Ppl)) or
                       (Pa (I).IR.Qual.Vpar.Tense_Voice_Mood =
                       (Fut, Active,  Ppl)) or
                       (Pa (I).IR.Qual.Vpar.Tense_Voice_Mood =
                       (Fut, Passive, Ppl)))
                     then

                        --  There is at least one hit;
                        --  fix PA, and advance J over the sum
                        K := Nk;
                     end if;
                  end loop;

                  if K = Nk  then
                     --  There was a PPL hit
                     Do_Clear_Pas_Nom_Ppl (Sum_Info, Compound_Tvm, Ppl_On,
                       Ppl_Info);

                     Pa_Last := Pa_Last + 1;
                     Pa (Pa_Last) :=
                       (Head ("PPL+" & Next_Word, Max_Stem_Size),
                       ((V,
                       (Ppl_Info.Con,
                       Compound_Tvm,
                       Sum_Info.Person,
                       Sum_Info.Number)
                        ), 0, Null_Ending_Record, X, A),
                       Ppp, Null_MNPC);
                  end if;

               elsif Is_Esse (Next_Word) or Is_Fuisse (Next_Word)  then
                  --  On NEXT_WORD

                  for I in 1 .. Pa_Last  loop    --  Check for PPL
                     if Pa (I).IR.Qual.Pofs = Vpar and then
                       (((Pa (I).IR.Qual.Vpar.Tense_Voice_Mood =
                       (Perf, Passive, Ppl)) and
                       Is_Esse (Next_Word)) or
                       ((Pa (I).IR.Qual.Vpar.Tense_Voice_Mood =
                       (Fut,  Active,  Ppl)) or
                       (Pa (I).IR.Qual.Vpar.Tense_Voice_Mood =
                       (Fut,  Passive, Ppl))))
                     then

                        --  There is at least one hit;
                        --  fix PA, and advance J over the sum
                        K := Nk;
                     end if;
                  end loop;

                  if K = Nk  then
                     --  There was a PPL hit
                     Do_Clear_Pas_Ppl (Next_Word, Compound_Tvm,
                       Ppl_On, Ppl_Info);

                     Pa_Last := Pa_Last + 1;
                     Pa (Pa_Last) :=
                       (Head ("PPL+" & Next_Word, Max_Stem_Size),
                       ((V,
                       (Ppl_Info.Con,
                       Compound_Tvm,
                       0,
                       X)
                        ), 0, Null_Ending_Record, X, A),
                       Ppp, Null_MNPC);
                  end if;

               elsif Is_Iri (Next_Word)  then
                  --  On NEXT_WORD = sum, esse, iri
                  --  Look ahead for sum

                  for J in 1 .. Pa_Last  loop    --  Check for SUPINE
                     if Pa (J).IR.Qual.Pofs = Supine   and then
                       Pa (J).IR.Qual.Supine.Of_Case = Acc
                     then
                        --  There is at least one hit;
                        --  fix PA, and advance J over the iri
                        K := Nk;

                     end if;
                  end loop;

                  if K = Nk  then      --  There was a SUPINE hit
                     Do_Clear_Pas_Supine (Supine_Info, Nk, Ppl_On);
                  end if;
               end if;       --  On NEXT_WORD = sum, esse, iri
            end Compounds_With_Sum;
         end if;       --  On WORDS_MODE (DO_COMPOUNDS)
      end if;

      if  Words_Mode (Write_Output_To_File)      then
         List_Stems (Configuration, Output, Input_Word,
           Input_Line, Pa, Pa_Last);
      else
         List_Stems (Configuration, Current_Output, Input_Word,
           Input_Line, Pa, Pa_Last);
      end if;

      Pa_Last := 0;

   exception
      when others  =>
         Put_Stat ("Exception    at "
           & Head (Integer'Image (Line_Number), 8)
           & Head (Integer'Image (Word_Number), 4)
           & "   " & Head (Input_Word, 28) & "   "  & Input_Line);
         raise;

   end Parse_Latin_Word;

   procedure Parse_Line (Configuration : Configuration_Type;
                         Input_Line : String) is
      L : Integer := Trim (Input_Line)'Last;
      Line : String (1 .. 2500) := (others => ' ');
      W : String (1 .. L) := (others => ' ');
   begin
      Word_Number := 0;
      Line (1 .. L) := Trim (Input_Line);

      --  Someday I ought to be interested in punctuation and numbers,
      --  but not now
      --      eliminate_not_letters:
      for I in 1 .. L  loop
         if Is_Alpha_Etc (Line (I)) then
            null;
         else
            Line (I) := ' ';
         end if;
      end loop;

      J2 := 1;
      K := 0;

      -- loop over line
      while J2 <= L  loop

         --  Skip over leading and intervening blanks, looking for comments
         --  Punctuation, numbers, and special Characters were cleared above
         for I in K + 1 .. L  loop
            exit when Line (J2) in 'A' .. 'Z';
            exit when Line (J2) in 'a' .. 'z';
            if I < L and then Line (I .. I + 1) = "--" then
               return;      --  the rest of the line is comment
            end if;
            J2 := I + 1;
         end loop;

         exit when J2 > L;             --  Kludge

         Follows_Period := False;
         if Followed_By_Period  then
            Followed_By_Period := False;
            Follows_Period := True;
         end if;

         Capitalized := False;
         All_Caps := False;

         --  Extract the word
         for I in J2 .. L  loop

            --  Although I have removed punctuation above,
            --  it may not always be so
            if Line (I) = '.'  then
               Followed_By_Period := True;
               exit;
            end if;
            exit when Line (I) not in 'A' .. 'Z' and Line (I) not in 'a' .. 'z';
            W (I) := Line (I);
            K := I;

         end loop;

         if W (J2) in 'A' .. 'Z'  and then
           K - J2 >= 1  and then
           W (J2 + 1) in 'a' .. 'z'
         then
            Capitalized := True;
         end if;

         All_Caps := True;
         for I in J2 .. K  loop
            if W (I) = Lower_Case (W (I))  then
               All_Caps := False;
               exit;
            end if;
         end loop;

         for I in J2 .. K - 1  loop               --  Kludge for QVAE
            if W (I) = 'Q'  and then W (I + 1) = 'V'  then
               W (I + 1) := 'U';
            end if;
         end loop;

         if Language = English_To_Latin  then
            Parse_English_Word (W (J2 .. K), Line, K, L);
            exit;
         end if;

         -- split parse_line () at this point, into two functions

         Parse_Latin_Word (Configuration, W (J2 .. K), Line, Input_Line, L);
         ----------------------------------------------------------------------
         ----------------------------------------------------------------------

         J2 := K + 1;    --  In case it is end of line and we don't look for ' '

         exit when Words_Mdev (Do_Only_Initial_Word);

      end loop;        --  Loop on line

   exception
      --   Have STORAGE_ERROR check in WORD too  ?????????????
      when Storage_Error  =>    --  I want to again, at least twice
         if Words_Mdev (Do_Pearse_Codes) then
            Ada.Text_IO.Put ("00 ");
         end if;
         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "STORAGE_ERROR Exception in WORDS, try again");
         Storage_Error_Count := Storage_Error_Count + 1;
         if Storage_Error_Count >= 4 then
            raise;
         end if;
         Pa_Last := 0;
      when Give_Up =>
         Pa_Last := 0;
         raise;
      when others  =>    --  I want to try to Get on with the next line
         Ada.Text_IO.Put_Line (    --  ERROR_FILE,
           "Exception in PARSE_LINE processing " & Input_Line);
         if Words_Mode (Write_Unknowns_To_File)  then
            if Words_Mdev (Do_Pearse_Codes) then
               Ada.Text_IO.Put (Unknowns, "00 ");
            end if;
            Ada.Text_IO.Put (Unknowns, Input_Line (J2 .. K));
            Ada.Text_IO.Set_Col (Unknowns, 30);
            Inflections_Package.Integer_IO.Put (Unknowns, Line_Number, 5);
            Inflections_Package.Integer_IO.Put (Unknowns, Word_Number, 3);
            Ada.Text_IO.Put_Line (Unknowns, "    ========   ERROR      ");
         end if;
         Pa_Last := 0;
   end Parse_Line;
end Parse;
