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
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Addons_Package; use Support_Utils.Addons_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Words_Engine.Word_Package; use Words_Engine.Word_Package;
with Words_Engine.Tricks; use Words_Engine.Tricks;
with Words_Engine.Put_Stat;
with Words_Engine.Search_English;
--with Support_Utils.Char_Utils; use Support_Utils.Char_Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Words_Engine.Explanation_Package; use Words_Engine.Explanation_Package;
use Latin_Utils;

with Words_Engine.Pearse_Code; use Words_Engine.Pearse_Code;

pragma Elaborate (Support_Utils.Word_Parameters);
package body Words_Engine.Parse
is
   use Ada.Text_IO;

   package Word_Container is new Vectors (Natural, Unbounded_String);
   use Word_Container;

   use Result_Container;

   type Word_Analysis_Result is
     record
        WA : Word_Analysis;
        Used_Next_Word : Boolean;
     end record;

   Syncope_Max : constant := 20;
   Tricks_Max : constant := 40;

   -- the scope of these variables is over-broad
   Storage_Error_Count : Integer := 0;
   No_Syncope : Boolean := False;

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

   procedure Clear_Parse_Array (Any_Pa : out Parse_Array) is
   begin
      for I in Any_Pa'Range loop
         Any_Pa (I) := Null_Parse_Record;
      end loop;
   end Clear_Parse_Array;

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
   -- code path; we should pass this information in as a record of type
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
   function Get_Pas_Participle
     (Parsed_Verb          : Vpar_Record;
      Trimmed_Next_Word    : String;
      Default_Ppl_On       : Boolean;
      Default_Compound_Tvm : Tense_Voice_Mood_Record;
      Default_Ppp_Meaning  : Meaning_Type;
      Default_Ppl_Info     : Vpar_Record)
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
   procedure Do_Clear_Pas_Nom_Ppl
     (Sum_Info     : in     Verb_Record;
      Compound_Tvm :    out Tense_Voice_Mood_Record;
      Ppl_On       : in out Boolean;
      Ppl_Info     :    out Vpar_Record;
      Pa           : in out Parse_Array;
      Pa_Last      : in out Integer;
      Xp           : in out Explanations)
   is
   begin
      for J4 in reverse 1 .. Pa_Last loop
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
                 Ppl_On, Compound_Tvm, Xp.Ppp_Meaning, Ppl_Info);
            begin
               Ppl_On := Part.Ppl_On;
               Ppl_Info := Part.Ppl_Info;
               Xp.Ppp_Meaning := Part.Ppp_Meaning;
               Compound_Tvm := Part.Compound_Tvm;
            end;
         else
            Pa (J4 .. Pa_Last - 1) := Pa (J4 + 1 .. Pa_Last);
            Pa_Last := Pa_Last - 1;
            Ppl_On := False;
         end if;
      end loop;
   end Do_Clear_Pas_Nom_Ppl;

   -- parts of these three do_clear_* functions should be factored together
   procedure Do_Clear_Pas_Ppl (Next_Word    : in     String;
                               Compound_Tvm :    out Tense_Voice_Mood_Record;
                               Ppl_On       : in out Boolean;
                               Ppl_Info     :    out Vpar_Record;
                               Pa           : in out Parse_Array;
                               Pa_Last      : in out Integer;
                               Xp           : in out Explanations)
   is
   begin
      for J5 in reverse 1 .. Pa_Last loop
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
                 Trimmed_Next_Word, Ppl_On, Compound_Tvm, Xp.Ppp_Meaning,
                 Ppl_Info);
            begin
               Ppl_On := Part.Ppl_On;
               Ppl_Info := Part.Ppl_Info;
               Xp.Ppp_Meaning := Part.Ppp_Meaning;
               Compound_Tvm := Part.Compound_Tvm;
            end;
         else
            Pa (J5 .. Pa_Last - 1) := Pa (J5 + 1 .. Pa_Last);
            Pa_Last := Pa_Last - 1;
            Ppl_On := False;
         end if;
      end loop;
   end Do_Clear_Pas_Ppl;

   -- parts of these three do_clear_* functions should be factored together
   procedure Do_Clear_Pas_Supine (Supine_Info    :    out Supine_Record;
                                  Ppl_On         : in out Boolean;
                                  Pa             : in out Parse_Array;
                                  Pa_Last        : in out Integer;
                                  Used_Next_Word : in out Boolean;
                                  Xp             : in out Explanations)
   is
   begin
      for J6 in reverse 1 .. Pa_Last loop
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
            Xp.Ppp_Meaning := Head (
              "SUPINE + iri => " &
              "FUT PASSIVE INF - to be about/going/ready to be ~",
              Max_Meaning_Size);

            Used_Next_Word := True;
         else
            Pa (J6 .. Pa_Last - 1) := Pa (J6 + 1 .. Pa_Last);
            Pa_Last := Pa_Last - 1;
            Ppl_On := False;
         end if;
      end loop;
   end Do_Clear_Pas_Supine;

   procedure Perform_Syncope (Input_Word : in     String;
                              Pa         : in out Parse_Array;
                              Pa_Last    : in out Integer;
                              Xp         : in out Explanations)
   is
      Sypa : Parse_Array (1 .. Syncope_Max) := (others => Null_Parse_Record);
      Sypa_Last : Integer := 0;
   begin
      Clear_Parse_Array (Sypa); -- FIXME: presumably redundant
      if Words_Mdev (Do_Syncope) and not No_Syncope then
         Syncope (Input_Word, Sypa, Sypa_Last, Xp);

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

   procedure Enclitic (Input_Word         :        String;
                       Entering_Pa_Last   : in out Integer;
                       Have_Done_Enclitic : in out Boolean;
                       Pa                 : in out Parse_Array;
                       Pa_Last            : in out Integer;
                       Xp                 : in out Explanations) is
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
                  Try_Slury (Less, Pa, Pa_Last, Line_Number, Word_Number, Xp);
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

               Perform_Syncope (Input_Word, Pa, Pa_Last, Xp);

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

   procedure Tricks_Enclitic (Input_Word         :        String;
                              Entering_Trpa_Last : in out Integer;
                              Have_Done_Enclitic :        Boolean;
                              Trpa               : in out Parse_Array;
                              Trpa_Last          : in out Integer;
                              Xp                 : in out Explanations) is
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
               Try_Tricks (Less, Trpa, Trpa_Last, Line_Number, Word_Number, Xp);

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

   procedure Pass (Input_Word         :        String;
                   Entering_Pa_Last   : in out Integer;
                   Have_Done_Enclitic : in out Boolean;
                   Pa                 : in out Parse_Array;
                   Pa_Last            : in out Integer;
                   Xp                 : in out Explanations)
   is
      --  This is the core logic of the program, everything else is details
      Save_Do_Fixes : constant Boolean := Words_Mode (Do_Fixes);
      Save_Do_Only_Fixes : constant Boolean := Words_Mdev (Do_Only_Fixes);
   begin
      --  Do straight WORDS without FIXES/TRICKS, is the word in the dictionary
      Words_Mode (Do_Fixes) := False;
      Roman_Numerals (Input_Word, Pa, Pa_Last, Xp);
      Word (Input_Word, Pa, Pa_Last);

      if Pa_Last = 0  then
         Try_Slury (Input_Word, Pa, Pa_Last, Line_Number, Word_Number, Xp);
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
      Perform_Syncope (Input_Word, Pa, Pa_Last, Xp);

      --  There may be a valid simple parse, if so it is most probable
      --  But I have to allow for the possibility that -que is answer,
      --  not colloque V
      Enclitic (Input_Word, Entering_Pa_Last, Have_Done_Enclitic, Pa, Pa_Last,
                Xp);

      --  Restore FIXES
      Words_Mode (Do_Fixes) := Save_Do_Fixes;

      --  Now with only fixes
      if Pa_Last = 0  and then Words_Mode (Do_Fixes) then
         Words_Mdev (Do_Only_Fixes) := True;
         Word (Input_Word, Pa, Pa_Last);

         Perform_Syncope (Input_Word, Pa, Pa_Last, Xp);

         Enclitic (Input_Word, Entering_Pa_Last, Have_Done_Enclitic,
           Pa, Pa_Last, Xp);

         Words_Mdev (Do_Only_Fixes) := Save_Do_Only_Fixes;
      end if;
   end Pass;

   procedure Parse_English_Word (Input_Word : in String;
                                 Line       : in String;
                                 K          : in Integer;
                                 L2         : in Integer)
   is
      L    : Integer             := L2;
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

   procedure Compounds_With_Sum
     (Pa             : in out Parse_Array;
      Pa_Last        : in out Integer;
      Next_Word      :        String;
      Used_Next_Word : in out Boolean;
      Xp             : in out Explanations)
   is
      Compound_Tvm   : Inflections_Package.Tense_Voice_Mood_Record;
      Ppl_On : Boolean := False;

      Sum_Info : Verb_Record := ((5, 1), (X, Active, X), 0, X);
      Ppl_Info : Vpar_Record := ((0, 0), X, X, X, (X, X, X));
      Supine_Info : Supine_Record := ((0, 0), X, X, X);

      Is_Verb_To_Be : Boolean := False;
   begin
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
            declare
               Q : constant Quality_Record := Pa (I).IR.Qual;
            begin
               if Q.Pofs = Vpar and then
                 Q.Vpar.Of_Case = Nom  and then
                 Q.Vpar.Number = Sum_Info.Number  and then
                 ((Q.Vpar.Tense_Voice_Mood =
                 (Perf, Passive, Ppl)) or
                 (Q.Vpar.Tense_Voice_Mood =
                 (Fut, Active,  Ppl)) or
                 (Q.Vpar.Tense_Voice_Mood =
                 (Fut, Passive, Ppl)))
               then
                  --  There is at least one hit;
                  --  fix PA, and advance J over the sum
                  Used_Next_Word := True;
                  exit;
               end if;
            end;
         end loop;

         if Used_Next_Word  then
            --  There was a PPL hit
            Do_Clear_Pas_Nom_Ppl (Sum_Info, Compound_Tvm, Ppl_On,
              Ppl_Info, Pa, Pa_Last, Xp);

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
            declare
               Q : constant Quality_Record :=
                 Pa (I).IR.Qual;
            begin
               if Q.Pofs = Vpar and then
                 (((Q.Vpar.Tense_Voice_Mood =
                 (Perf, Passive, Ppl)) and
                 Is_Esse (Next_Word)) or
                 ((Q.Vpar.Tense_Voice_Mood =
                 (Fut,  Active,  Ppl)) or
                 (Q.Vpar.Tense_Voice_Mood =
                 (Fut,  Passive, Ppl))))
               then
                  --  There is at least one hit;
                  --  fix PA, and advance J over the sum
                  Used_Next_Word := True;
                  exit;
               end if;
            end;
         end loop;

         if Used_Next_Word  then
            --  There was a PPL hit
            Do_Clear_Pas_Ppl (Next_Word, Compound_Tvm,
              Ppl_On, Ppl_Info, Pa, Pa_Last, Xp);

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
            declare
               Q : constant Quality_Record := Pa (J).IR.Qual;
            begin
               if Q.Pofs = Supine   and then
                 Q.Supine.Of_Case = Acc
               then
                  --  There is at least one hit;
                  --  fix PA, and advance J over the iri
                  Used_Next_Word := True;
                  exit;
               end if;
            end;
         end loop;

         if Used_Next_Word  then      --  There was a SUPINE hit
            Do_Clear_Pas_Supine (Supine_Info, Ppl_On,
              Pa, Pa_Last, Used_Next_Word, Xp);
         end if;
      end if;       --  On NEXT_WORD = sum, esse, iri
   end Compounds_With_Sum;

   -- the K variable is passed in as Input_Word'Last, but it can be modified
   -- by this routine, in the case where a multi-word compound is detected,
   -- e.g., "movendi sunt"; the value is read in the caller, and employed to
   -- update an iterator over the whole input line. Used_Next_Word is also
   -- set in these circumstances, but is currently unused

   -- in future, we shall pass in the next word, if any, in the input line
   -- to this function, obviating the need to go grovelling around further
   -- down the string, and saving wear and tear on variables like K
   function Parse_Latin_Word
     (Configuration  : Configuration_Type;
      Input_Word     : String; -- a trimmed single word
      Input_Line     : String; -- what the user actually typed
      Next_Word      : String
     ) return Word_Analysis_Result
   is
      Pa : Parse_Array (1 .. 100) := (others => Null_Parse_Record);
      Pa_Last : Integer := 0;
      Trpa : Parse_Array (1 .. Tricks_Max) := (others => Null_Parse_Record);
      Trpa_Last : Integer := 0;

      Entering_Pa_Last : Integer := 0;
      Entering_Trpa_Last    : Integer := 0;
      Have_Done_Enclitic : Boolean := False;
      Used_Next_Word : Boolean := False;
      Xp : Explanations; -- to store the previously global state
   begin   --  PARSE
      Xp.Xxx_Meaning := Null_Meaning_Type;

      -- This step is actually redundant; it is mentioned here simply to
      -- make it explicit that the contents of Pa are discarded after each
      -- word is handled; Pa and friends do not need to be package-global,
      -- but could have been local to this procedure
      Clear_Parse_Array (Pa);
      Clear_Parse_Array (Trpa);

      Pa_Last := 0;
      Word_Number := Word_Number + 1;

      Pass (Input_Word, Entering_Pa_Last, Have_Done_Enclitic, Pa, Pa_Last, Xp);

      --if (PA_LAST = 0) or DO_TRICKS_ANYWAY  then
      --  WORD failed, try to modify the word
      if (Pa_Last = 0)  and then
        not (Words_Mode (Ignore_Unknown_Names)  and Capitalized)
      then
         --  WORD failed, try to modify the word
         if Words_Mode (Do_Tricks)  then
            Words_Mode (Do_Tricks) := False;
            --  Turn it off so won't be circular
            Try_Tricks (Input_Word, Trpa, Trpa_Last, Line_Number,
              Word_Number, Xp);
            if Trpa_Last = 0  then
               Tricks_Enclitic (Input_Word, Entering_Trpa_Last,
                 Have_Done_Enclitic, Trpa, Trpa_Last, Xp);
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
            Compounds_With_Sum (Pa, Pa_Last, Next_Word, Used_Next_Word, Xp);
         end if;       --  On WORDS_MODE (DO_COMPOUNDS)
      end if;

      declare
         WA : Word_Analysis;
      begin
         WA := Analyse_Word (Pa, Pa_Last, Input_Word, Xp);
         return (WA => WA, Used_Next_Word => Used_Next_Word);
      end;

   exception
      when others  =>
         Put_Stat ("Exception    at "
           & Head (Integer'Image (Line_Number), 8)
           & Head (Integer'Image (Word_Number), 4)
           & "   " & Head (Input_Word, 28) & "   "  & Input_Line);
         raise;

   end Parse_Latin_Word;

   function Is_Capitalized (Input_Word : String) return Boolean
   is
   begin
      if Input_Word'Length <= 1 then
         return False;
      end if;
      return
        Input_Word (Input_Word'First) in 'A' .. 'Z' and then
        Input_Word (Input_Word'First + 1) in 'a' .. 'z';
   end Is_Capitalized;

   function Is_All_Caps (Input_Word : String) return Boolean
   is
   begin
      for I in Input_Word'Range  loop
         if Input_Word (I) = Lower_Case (Input_Word (I))  then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Caps;

   procedure Do_Qvae_Kludge
     (W     : in out String;
      J2, K : in     Integer) is
   begin
      -- QVAE Kludge? No-one's seen QVAAKLUDES since the '70s!
      for I in J2 .. K - 1  loop
         if W (I) = 'Q'  and then W (I + 1) = 'V'  then
            W (I + 1) := 'U';
         end if;
      end loop;
   end Do_Qvae_Kludge;

   function String_Before_Dash (S : String) return String is
   begin
      if Ada.Strings.Fixed.Index (S, "--") > S'First then
         return S (S'First .. Ada.Strings.Fixed.Index (S, "--"));
      else
         return S;
      end if;
   end String_Before_Dash;

   function Strip_Non_Alpha_Etc (S : String) return String is
      Twice : constant Integer := 2 * S'Length;
      S2 : String (1 .. Twice);
      J : Integer := S'First - 1;

      function Is_Alpha_Etc (C : Character) return Boolean is
      begin
         case C is
            when 'a' .. 'z' | 'A' .. 'Z' | ' ' | '.' =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Alpha_Etc;
   begin
      for I in S'Range loop
         if Is_Alpha_Etc (S (I)) then
            J := J + 1;
            S2 (J) := S (I);
            if S (I) = '.' then
               J := J + 1;
               S2 (J) := ' ';
            end if;
         else
            J := J + 1;
            S2 (J) := ' ';
         end if;
      end loop;
      return S2 (1 .. J);
   end Strip_Non_Alpha_Etc;

   function Make_Words (Line : String) return Word_Container.Vector
   is
      Words : Word_Container.Vector;
      Indices : array (Line'Range) of Natural;
      Next_Index : Natural := Indices'First;
   begin
      if Line'Length = 0 then
         return Words;
      end if;

      Indices (Next_Index) := Line'First;
      while Indices (Next_Index) < Line'Last loop
         Next_Index := Next_Index + 1;
         Indices (Next_Index) := 1 +
           Ada.Strings.Fixed.Index (Line (Indices (Next_Index - 1) ..
                                                      Line'Last), " ");
         if Indices (Next_Index) = 1 then
            Indices (Next_Index) := Line'Last + 2;
         end if;

         declare
            S : constant String := Line (
              Indices (Next_Index - 1) .. Indices (Next_Index) - 2);
         begin
            if S /= "" and S /= "." then
               Words.Append (To_Unbounded_String (S));
            end if;
         end;
      end loop;
      return Words;
   end Make_Words;

   -- forward declarations for exception handlers
   procedure Report_Storage_Error;
   procedure Report_Unknown_Error (Input_Line : String);

   -- Analyse_Line (..., Input_Line : String)
   --
   -- This procedure massages the Input_Line, dealing with capitalisation,
   -- punctuation, trimming, and so on; it splits the line into separate
   -- words, and tries to look them up.
   --
   -- When looking up English words, it only deals with the first word of
   -- input; similarly, an option can be specified to make it only look up
   -- the first Latin word on a line.

   -- Before the main loop, we convert all non-alpha characters to spaces,
   -- where non-alpha means the complement of [a-zA-Z.-]

   -- What the main loop actually does:
   --
   -- ignore everything after a double dash
   --
   -- set (Followed_By_Period, Capitalized, All_Caps) appropriately
   --
   -- apply QVAE kludge
   --
   -- if doing English, do first word then quit
   -- otherwise, we are doing Latin, so do a word
   -- quit after first word if appropriate config value set

   function Analyse_Line (Configuration : Configuration_Type;
                          Input_Line    : String)
     return Result_Container.Vector
   is
      L     : constant Integer   := Trim (Input_Line)'Last;
      Line  : String (1 .. 2500) := (others => ' ');
      Used_Next_Word : Boolean := False;

      Undashed : constant String := String_Before_Dash (Input_Line);
      Stripped : constant String := Strip_Non_Alpha_Etc (Undashed);
      S : constant Word_Container.Vector := Make_Words (Stripped);
      Analyses : Result_Container.Vector;

      function Word_After (I : Count_Type) return String is
      begin
         if I + 1 >= S.Length then
            return "";
         else
            return To_String (Element (Container => S,
                                       Index => Integer'Val (I + 1)));
         end if;
      end Word_After;
   begin
      Word_Number := 0;
      Line (1 .. L) := Trim (Input_Line);

      for I in 0 .. (S.Length - 1) loop
         if Used_Next_Word then
            Used_Next_Word := False;
            goto Continue;
         end if;

         Followed_By_Period := False;
         Used_Next_Word := False;

         declare
            W : String := To_String (Element (Container => S,
                                              Index => Integer'Val (I)));
            Last : Integer := W'Last;
            Next_Word : constant String := Word_After (I);
         begin
            if W (W'Last) = '.' then
               Followed_By_Period := True;
               Last := W'Last - 1;
            end if;

            Do_Qvae_Kludge (W, W'First, Last);
            declare
               Input_Word : constant String := W (W'First .. Last);
               Result     : Word_Analysis_Result;

            begin
               Capitalized := Is_Capitalized (Input_Word);
               All_Caps    := Is_All_Caps (Input_Word);

               if Language = English_To_Latin  then
                  Parse_English_Word (Input_Word, Line, Last, L);
                  exit;
               end if;

               Result := Parse_Latin_Word (Configuration, Input_Word,
                 Input_Line, Next_Word);
               Used_Next_Word := Result.Used_Next_Word;

               Analyses.Append (Result.WA);
            end;
         end;
         <<Continue>>
         exit when Words_Mdev (Do_Only_Initial_Word);
      end loop;

      return Analyses;
   end Analyse_Line;

   procedure Parse_Line (Configuration : Configuration_Type;
                         Input_Line    : String)
   is
      Undashed : constant String := String_Before_Dash (Input_Line);

      -- hack around the weird reimplementation of output redirection
      procedure Put_Analysis (A_Cursor : Result_Container.Cursor) is
         Analysis : constant Word_Analysis := Element (Position => A_Cursor);
         type File_Type_Access is access constant Ada.Text_IO.File_Type;
         O : File_Type_Access;
      begin
         if Words_Mode (Write_Output_To_File) then
            O := Output'Access;
         else
            O := Current_Output.all'Access; -- Current_Output is a proc
         end if;

         List_Stems (Configuration, O.all, Analysis, Undashed);
      end Put_Analysis;

      procedure Print_Analyses
        (Analyses : Result_Container.Vector)
      is
      begin
         Analyses.Iterate (Process => Put_Analysis'Access);
      end Print_Analyses;
   begin
      declare
         Analyses : Result_Container.Vector := Analyse_Line (Configuration,
           Input_Line);
      begin
         Print_Analyses (Analyses);
         Clear (Analyses);
      end;
   exception
      when Storage_Error =>
         Report_Storage_Error;
         if Storage_Error_Count >= 4 then
            raise;
         end if;
      when Give_Up =>
         raise;
      when others =>
         Report_Unknown_Error (Input_Line);
         raise;
   end Parse_Line;

   procedure Report_Storage_Error
   is
   begin
      if Words_Mdev (Do_Pearse_Codes) then
         --Ada.Text_IO.Put ("00 ");
         Ada.Text_IO.Put (Pearse_Code.Format (Unknown));
      end if;
      Ada.Text_IO.Put_Line (    --  ERROR_FILE,
        "STORAGE_ERROR Exception in WORDS, try again");
      Storage_Error_Count := Storage_Error_Count + 1;
   end Report_Storage_Error;

   procedure Report_Unknown_Error (Input_Line : String)
   is
   begin
      Ada.Text_IO.Put_Line (    --  ERROR_FILE,
        "Exception in PARSE_LINE processing " & Input_Line);
      if Words_Mode (Write_Unknowns_To_File)  then
         if Words_Mdev (Do_Pearse_Codes) then
            Ada.Text_IO.Put (Unknowns, Pearse_Code.Format (Unknown));
         end if;
         Ada.Text_IO.Put (Unknowns, Input_Line);
         Ada.Text_IO.Set_Col (Unknowns, 30);
         Inflections_Package.Integer_IO.Put (Unknowns, Line_Number, 5);
         Inflections_Package.Integer_IO.Put (Unknowns, Word_Number, 3);
         Ada.Text_IO.Put_Line (Unknowns, "    ========   ERROR      ");
      end if;
   end Report_Unknown_Error;
end Words_Engine.Parse;
