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

with Ada.Exceptions; use Ada.Exceptions;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Support_Utils.Uniques_Package; use Support_Utils.Uniques_Package;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
use Latin_Utils;

package body Words_Engine.List_Sweep
is

   function Allowed_Stem (Pr : Parse_Record) return Boolean is
      Allowed : Boolean := True;
      --  modify as necessary and return it
      De : Dictionary_Entry;
   begin
      -- TEXT_IO.PUT ("ALLOWED? >");
      -- PARSE_RECORD_IO.PUT (PR);
      -- TEXT_IO.NEW_LINE;
      -- FIXME: duplicates (commented) code below
      if Pr.D_K not in General .. Local  then
         return True;
      end if;

      Dict_IO.Read (Dict_File (Pr.D_K), De, Pr.MNPC);

      --  NOUN CHECKS
      case  Pr.IR.Qual.Pofs is
         when N  =>
            if  Words_Mdev (For_Word_List_Check)  then
               if (Nom <= Pr.IR.Qual.Noun.Of_Case) and then
                 (S <= Pr.IR.Qual.Noun.Number)
               then
                  Allowed := True;
               elsif (Nom <= Pr.IR.Qual.Noun.Of_Case) and then
                 (Pr.IR.Qual.Noun.Number = P)
               then

                  Search_For_Pl :
                  declare
                     De : Dictionary_Entry;
                     Mean : Meaning_Type := Null_Meaning_Type;
                  begin
                     Allowed := False;
                     Dict_IO.Read (Dict_File (Pr.D_K), De, Pr.MNPC);
                     Mean := De.Mean;
                     for J in Meaning_Type'First .. Meaning_Type'Last - 2
                     loop
                        if Mean (J .. J + 2) = "pl."  then
                           Allowed := True;
                           exit;
                        end if;
                     end loop;
                  end Search_For_Pl;
               else
                  Allowed := False;
               end if;
            end if;

         when  Adj  =>
            if  Words_Mdev (For_Word_List_Check)  then
               Allowed := (Nom <= Pr.IR.Qual.Adj.Of_Case) and then
                          (S <= Pr.IR.Qual.Adj.Number) and then
                          (M <= Pr.IR.Qual.Adj.Gender);
            end if;
            --  VERB CHECKS
         when  V  =>
            --TEXT_IO.PUT ("VERB  ");
            --  Check for Verb 3 1  dic/duc/fac/fer shortened imperative
            --  See G&L 130.5
            declare
               Stem : constant String := Trim (Pr.Stem);
               Last_Three : String (1 .. 3);
            begin
               if (Pr.IR.Qual.Verb = ((3, 1), (Pres, Active, Imp), 2, S)) and
                 (Pr.IR.Ending.Size = 0)
               then    --  For this special case
                  if Stem'Length >= 3  then
                     Last_Three := Stem (Stem'Last - 2 .. Stem'Last);
                     if not ((Last_Three = "dic")  or
                             (Last_Three = "duc")  or
                             (Last_Three = "fac")  or
                             (Last_Three = "fer"))
                     then
                        Allowed := False;
                     end if;
                  else
                     Allowed := False;
                  end if;
               end if;
            end;

            --  Check for Verb Imperative being in permitted person
            if Pr.IR.Qual.Verb.Tense_Voice_Mood.Mood = Imp and then
               not (((Pr.IR.Qual.Verb.Tense_Voice_Mood.Tense = Pres) and
                     (Pr.IR.Qual.Verb.Person = 2)) or else

                    ((Pr.IR.Qual.Verb.Tense_Voice_Mood.Tense = Fut) and
                     (Pr.IR.Qual.Verb.Person = 2 or
                      Pr.IR.Qual.Verb.Person = 3)))
            then
               Allowed := False;
            end if;

            --  Check for V IMPERS and demand that only 3rd person
            if De.Part.V.Kind = Impers and then Pr.IR.Qual.Verb.Person /= 3
            then
               Allowed := False;
            end if;

            --  Check for V DEP    and demand PASSIVE
            if De.Part.V.Kind = Dep then
               --TEXT_IO.PUT ("DEP  ");
               if (Pr.IR.Qual.Verb.Tense_Voice_Mood.Voice = Active)  and
                 (Pr.IR.Qual.Verb.Tense_Voice_Mood.Mood = Inf)  and
                 (Pr.IR.Qual.Verb.Tense_Voice_Mood.Tense = Fut)
               then
                  --TEXT_IO.PUT ("PASSIVE  ");
                  Allowed := True;
               elsif (Pr.IR.Qual.Verb.Tense_Voice_Mood.Voice = Active)  and
                 (Pr.IR.Qual.Verb.Tense_Voice_Mood.Mood in Ind .. Inf)
               then
                  --TEXT_IO.PUT ("ACTIVE  ");
                  Allowed := False;
               else
                  --TEXT_IO.PUT ("??????  ");
                  null;
               end if;
            end if;

            --  Check for V SEMIDEP    and demand PASSIVE ex Perf
            if De.Part.V.Kind = Semidep and
               (Pr.IR.Qual.Verb.Tense_Voice_Mood.Mood in Ind .. Imp) and
               (((Pr.IR.Qual.Verb.Tense_Voice_Mood.Voice = Passive) and
                 (Pr.IR.Qual.Verb.Tense_Voice_Mood.Tense in Pres .. Fut)) or
                ((Pr.IR.Qual.Verb.Tense_Voice_Mood.Voice = Active) and
                 (Pr.IR.Qual.Verb.Tense_Voice_Mood.Tense in Perf .. Futp)))
            then
               Allowed := False;
            end if;

            if  Words_Mdev (For_Word_List_Check)  then
               if (Pr.IR.Qual.Verb.Person = 1) and then
                 (Pr.IR.Qual.Verb.Number = S)
               then
                  Allowed := (Pr.IR.Qual.Verb.Tense_Voice_Mood =
                     (Pres, Active, Ind)) and
                    ((De.Part.V.Kind in X .. Intrans) or else
                     (De.Part.V.Kind = Dep) or else
                     (De.Part.V.Kind = Semidep) or else
                     (De.Part.V.Kind = Perfdef));
               elsif De.Part.V.Kind = Impers then
                  Allowed := (Pr.IR.Qual.Verb.Person = 3)  and then
                    (Pr.IR.Qual.Verb.Number = S)  and then
                    (Pr.IR.Qual.Verb.Tense_Voice_Mood = (Pres, Active, Ind));
               else
                  Allowed := False;
               end if;
            end if;

         when  others  =>
            null;

      end case;

      if  Words_Mdev (For_Word_List_Check) then       --  Non parts
         if Pr.IR.Qual.Pofs in Vpar .. Supine then
            Allowed := False;
         end if;
      end if;                                           --  Non parts
      return Allowed;
   end Allowed_Stem;

   -- FIXME: Pa is effectively passed in twice; Sl is often a slice of Pa
   procedure Order_Parse_Array
     (Sl     : in out Parse_Array;
      Diff_J : out Integer;
      Pa     : in Parse_Array)
   is
      use Dict_IO;

      Hits                       : Integer := 0;
      Sl_Last                    : Integer := Sl'Last;
      Sl_Last_Initial            : constant Integer := Sl_Last;
      Sm                         : Parse_Record;
      Has_Noun_Abbreviation      : Boolean := False;

      Not_Only_Archaic           : Boolean := False;
      Not_Only_Medieval          : Boolean := False;
      Not_Only_Uncommon          : Boolean := False;

      function Depr (Pr : Parse_Record) return Dictionary_Entry is
         De : Dictionary_Entry;
      begin
         -- TEXT_IO.PUT ("DEPR  ");
         -- PARSE_RECORD_IO.PUT (PR);
         -- TEXT_IO.NEW_LINE;
         -- FIXME: duplicates (commented) code above
         if Pr.MNPC = Null_MNPC  then
            return Null_Dictionary_Entry;
         else
            if Pr.D_K in General .. Local  then
               --if PR.MNPC /= OMNPC  then
               Dict_IO.Set_Index (Dict_File (Pr.D_K), Pr.MNPC);
               Dict_IO.Read (Dict_File (Pr.D_K), De);
               --OMNPC := PR.MNPC;
               --ODE := DE;
               --else
               --DE := ODE;
               --end if;
            elsif Pr.D_K = Unique  then
               De :=  Uniques_De (Pr.MNPC);
            end if;
         end if;

         return De;
      end Depr;

   begin

      if Sl'Length = 0 then
         Diff_J := Sl_Last_Initial - Sl_Last;
         return;
      end if;

      -- FIXME: this code looks like it's duplicated in another file

      --  Bubble sort since this list should usually be very small (1-5)
      Hit_Loop :
      loop
         Hits := 0;

         --------------------------------------------------

         Switch :
         declare
            function "<" (Left, Right : Quality_Record) return Boolean is
            begin
               if Left.Pofs = Right.Pofs  and then
                 Left.Pofs = Pron        and then
                 Left.Pron.Decl.Which = 1
               then
                  return (Left.Pron.Decl.Var < Right.Pron.Decl.Var);
               else
                  return Inflections_Package."<"(Left, Right);
               end if;
            end "<";

            function Equ (Left, Right : Quality_Record) return Boolean is
            begin
               if Left.Pofs = Right.Pofs  and then
                 Left.Pofs = Pron        and then
                 Left.Pron.Decl.Which = 1
               then
                  return (Left.Pron.Decl.Var = Right.Pron.Decl.Var);
               else
                  return Inflections_Package."="(Left, Right);
               end if;
            end Equ;

            function Meaning (Pr : Parse_Record) return Meaning_Type is
            begin
               return Depr (Pr).Mean;
            end Meaning;

            function Compare (L : Parse_Record;
                              R : Parse_Record) return Boolean is
            begin
               --  Maybe <   =  on PR.STEM  -  will have to make up "<"
               --  Actually STEM and PART  --  and check that later in print
               return R.D_K  > L.D_K
                 or else  --  Let DICT.LOC list first

                 (R.D_K  = L.D_K    and then
                 R.MNPC  < L.MNPC)   or else

                 (R.D_K  = L.D_K    and then
                 R.MNPC  = L.MNPC    and then
                 R.IR.Qual < L.IR.Qual)  or else

                 (R.D_K  = L.D_K    and then
                 R.MNPC  = L.MNPC    and then
                 Equ (R.IR.Qual, L.IR.Qual)  and then
                 Meaning (R) < Meaning (L))
                 or else   --  | is > letter

                 (R.D_K  = L.D_K  and then
                 R.MNPC  = L.MNPC    and then
                 Equ (R.IR.Qual, L.IR.Qual)  and then
                 Meaning (R) = Meaning (L)   and then
                 R.IR.Ending.Size < L.IR.Ending.Size)  or else

                 (R.D_K  = L.D_K  and then
                 R.MNPC  = L.MNPC    and then
                 Equ (R.IR.Qual, L.IR.Qual)  and then
                 Meaning (R) = Meaning (L)   and then
                 R.IR.Ending.Size = L.IR.Ending.Size  and then
                 Inflections_Package."<"(R.IR.Qual, L.IR.Qual));
            end Compare;

         begin
            --  Need to remove duplicates in ARRAY_STEMS
            --  This sort is very sloppy
            --  One problem is that it can mix up some of the order of
            --  PREFIX, XXX, LOC

            --  I ought to do this for every set of results from
            --  different approaches not just in one fell swoop
            --  at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            Inner_Loop :
            for I in Sl'First .. Sl_Last - 1  loop
               if Compare (Sl (I), Sl (I + 1)) then
                  Sm := Sl (I);
                  Sl (I) := Sl (I + 1);
                  Sl (I + 1) := Sm;
                  Hits := Hits + 1;

               end if;

            end loop Inner_Loop;

         end Switch;
         --------------------------------------------------

         exit Hit_Loop when Hits = 0;
      end loop Hit_Loop;

      --  Fix up the Archaic/Medieval
      if Words_Mode (Trim_Output)  then
         --  Check to see if we can afford to TRIM,
         --  if there will be something left over
         for I in Sl'First .. Sl_Last
         loop
            declare
               De : Dictionary_Entry;
            begin
               if Sl (I).D_K in General .. Local  then

                  Dict_IO.Set_Index (Dict_File (Sl (I).D_K), Sl (I).MNPC);
                  --TEXT_IO.PUT (INTEGER'IMAGE (INTEGER (SL (I).MNPC)));
                  Dict_IO.Read (Dict_File (Sl (I).D_K), De);
                  --DICTIONARY_ENTRY_IO.PUT (DE); TEXT_IO.NEW_LINE;

                  if ((Sl (I).IR.Age = X) or else (Sl (I).IR.Age > A))  and
                    ((De.Tran.Age = X) or else (De.Tran.Age > A))
                  then
                     Not_Only_Archaic := True;
                  end if;
                  if ((Sl (I).IR.Age = X) or else (Sl (I).IR.Age < F))  and
                    --  Or E????
                    ((De.Tran.Age = X) or else (De.Tran.Age < F))
                  then
                     Not_Only_Medieval := True;
                  end if;
                  if ((Sl (I).IR.Freq = X) or else (Sl (I).IR.Freq < C)) and
                    --  A/X < C   --  C for inflections is uncommon  !!!!
                    ((De.Tran.Freq = X) or else (De.Tran.Freq < D))
                    --     --  E for DICTLINE is uncommon  !!!!
                  then
                     Not_Only_Uncommon := True;
                  end if;

                  if Sl (I).IR.Qual.Pofs = N  and then
                    Sl (I).IR.Qual.Noun.Decl = (9, 8)
                  then
                     Has_Noun_Abbreviation := True;
                  end if;
               end if;
            end;
         end loop;

         --  We order and Trim  within a subset SL, but have to correct the
         --  big set PA also
         --  Kill not ALLOWED first, then check the remaining from the top
         --  I am assuming there is no Trim ming of FIXES for AGE/ .. .
         for I in reverse Sl'First .. Sl_Last loop
            --  Remove not ALLOWED_STEM & null
            if not Allowed_Stem (Sl (I)) or (Pa (I) = Null_Parse_Record)
            then
               Sl (I .. Sl_Last - 1) := Sl (I + 1 .. Sl_Last);
               Sl_Last := Sl_Last - 1;
               Trimmed := True;
            elsif (Not_Only_Archaic and Words_Mdev (Omit_Archaic)) and then
              Sl (I).IR.Age = A
            then
               Sl (I .. Sl_Last - 1) := Sl (I + 1 .. Sl_Last);
               Sl_Last := Sl_Last - 1;
               Trimmed := True;
            elsif (Not_Only_Medieval and Words_Mdev (Omit_Medieval)) and then
              Sl (I).IR.Age >= F
            then
               Sl (I .. Sl_Last - 1) := Sl (I + 1 .. Sl_Last);
               Sl_Last := Sl_Last - 1;
               Trimmed := True;

            elsif (Not_Only_Uncommon and Words_Mdev (Omit_Uncommon)) and then
              Sl (I).IR.Freq >= C
            then      --  Remember A < C
               Sl (I .. Sl_Last - 1) := Sl (I + 1 .. Sl_Last);
               Sl_Last := Sl_Last - 1;
               Trimmed := True;

            ----Big problem.  This area has been generaing exceptions.
            ----At least one difficulty is that suffixes change POFS.
            ----So one has a N inflection (SL) but a V DE
            ----When the program checks for VOC, it wants a N
            ---- and then asks about KIND (P, N, T, .. .)
            ---- But the DE (v) does not have those
            ---- The solution would be to fix ADD SUFFIX
            ---- to do somethnig about
            --   passing the ADDON KIND
            ----  I do not want to face that now
            ----  It is likely that all this VOC/LOC is worthless anyway.
            ---    Maybe lower FREQ in INFLECTS
            ----
            ----  A further complication is the GANT and AO give
            --    different results (AO no exception)
            ----  That is probably because the program is in
            --    error and the result threrfore unspecified
            ----
            ----

            --  This is really working much too hard!
            --  just to kill Roman numeral for three single letters
            --  Also strange in that code depends on dictionary knowledge
            elsif Has_Noun_Abbreviation    and then
              (All_Caps and Followed_By_Period)
            then
               if (Sl (I).IR.Qual.Pofs /= N) or
                 ((Sl (I).IR.Qual /= (N, ((9, 8), X, X, M)))  and
                 (Trim (Sl (I).Stem)'Length = 1  and then
                 (Sl (I).Stem (1) = 'A'  or
                 Sl (I).Stem (1) = 'C'  or
                 Sl (I).Stem (1) = 'D'  or
                 --SL (I).STEM (1) = 'K'  or      --  No problem here
                 Sl (I).Stem (1) = 'L'  or
                 Sl (I).Stem (1) = 'M'            --  or
                 )))
               then
                  Sl (I .. Sl_Last - 1) := Sl (I + 1 .. Sl_Last);
                  Sl_Last := Sl_Last - 1;
                  Trimmed := True;
               end if;
            end if;
         end loop;

      end if;   --  On TRIM

      Diff_J := Sl_Last_Initial - Sl_Last;

   end Order_Parse_Array;

   procedure List_Sweep
     (Pa      : in out Parse_Array;
      Pa_Last : in out Integer)
   is
      --  This procedure is supposed to process the Output PARSE_ARRAY at
      --  PA level
      --  before it gets turned into SIRAA and DMNPCA in LIST_PACKAGE
      --  Since it does only PARSE_ARRAY it is just cheaking INFLECTIONS, not
      --  DICTIONARY

      -----------------------------------------------------------
   begin                               --  LIST_SWEEP

      if Pa'Length = 0 then
         return;
      end if;

      Reset_Pronoun_Kind :
      declare
         De : Dictionary_Entry;
      begin
         for I in 1 .. Pa_Last  loop
            if Pa (I).D_K = General  then
               Dict_IO.Set_Index (Dict_File (Pa (I).D_K), Pa (I).MNPC);
               Dict_IO.Read (Dict_File (Pa (I).D_K), De);
               if De.Part.Pofs = Pron  and then
                 De.Part.Pron.Decl.Which = 1
               then
                  Pa (I).IR.Qual.Pron.Decl.Var :=
                    Pronoun_Kind_Type'Pos (De.Part.Pron.Kind);
               end if;
            end if;
         end loop;
      end Reset_Pronoun_Kind;
      ---------------------------------------------------

      --  NEED TO REMOVE DISALLOWED BEFORE DOING ANYTHING - BUT
      --  WITHOUT REORDERING

      --  The problem I seem to have to face first, if not the first problem,
      --  is the situation in which there are several sets of identical IRs
      --  with different MNPC. These may be variants with some other stem
      --  (e.g., K=3) not affecting the (K=1) word. Or they might be
      --  identical forms with different meanings (| additional meanings)
      --  I need to group such common inflections - and pass this on somehow

      Sweeping :
      --  To remove disallowed stems/inflections and resulting dangling fixes
      declare
         Internal_Loop_Error : exception;
         Fix_On  : Boolean := False;
         Pw_On   : Boolean := False;
         P_First : Integer := 1;
         P_Last  : Integer := 0;
         Jj      : Integer := 0;
         Diff_J  : Integer := 0;
         subtype Xons is Part_Of_Speech_Type range Tackon .. Suffix;
      begin
         for J in reverse 1 .. Pa_Last loop --  Sweep backwards over PA

            if ((Pa (J).D_K in Addons .. Yyy) or (Pa (J).IR.Qual.Pofs in Xons))
              and then (Pw_On)
            then               --  first FIX/TRICK after regular
               Fix_On := True;
               Pw_On  := False;
               P_First := J + 1;

               Jj := J;
               while Pa (Jj + 1).IR.Qual.Pofs = Pa (Jj).IR.Qual.Pofs  loop
                  P_Last := Jj + 1;
                  Raise_Exception (Internal_Loop_Error'Identity,
                                   "Programming error; known bug, #70");
               end loop;

               ----Order internal to this set of inflections

               Order_Parse_Array (Pa (P_First .. P_Last), Diff_J, Pa);
               Pa (P_Last - Diff_J + 1 .. Pa_Last - Diff_J) :=
                 Pa (P_Last + 1 .. Pa_Last);
               Pa_Last := Pa_Last - Diff_J;
               P_First := 1;
               P_Last  := 0;

            elsif ((Pa (J).D_K in Addons .. Yyy) or
              (Pa (J).IR.Qual.Pofs in Xons))  and then
              (Fix_On)
            then               --  another FIX
               null;
            elsif ((Pa (J).D_K in Addons .. Yyy)  or
              (Pa (J).IR.Qual.Pofs = X))  and then  --  Kills TRICKS stuff
              (not Pw_On)
            then
               Pa (P_Last - Diff_J + 1 .. Pa_Last - Diff_J) :=
                 Pa (P_Last + 1 .. Pa_Last);
               Pa_Last := Pa_Last - Diff_J;
               P_Last := P_Last - 1;
            else
               Pw_On := True;
               Fix_On := False;
               if P_Last <= 0  then
                  P_Last := J;
               end if;
               if J = 1  then
                  Order_Parse_Array (Pa (1 .. P_Last), Diff_J, Pa);
                  Pa (P_Last - Diff_J + 1 .. Pa_Last - Diff_J) :=
                    Pa (P_Last + 1 .. Pa_Last);
                  Pa_Last := Pa_Last - Diff_J;
               end if;
            end if;                                      --  check PART

         end loop;                          --  loop sweep over PA
      end Sweeping;

      --  Last chance to weed out duplicates
      declare
         Pr  : Parse_Record := Null_Parse_Record;
         Opr : Parse_Record := Pa (1);
         J   : Integer      := 2;
      begin
         Compress_Loop :
         loop
            exit Compress_Loop when J > Pa_Last;
            Pr := Pa (J);
            if Pr /= Opr  then
               Supress_Key_Check :
               declare
                  function "<=" (A, B : Parse_Record) return Boolean is
                     use Dict_IO;
                  begin                  --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                     return A.IR.Qual = B.IR.Qual and A.MNPC = B.MNPC;
                  end "<=";
               begin
                  if (Pr.D_K /= Xxx) and (Pr.D_K /= Yyy) and (Pr.D_K /= Ppp)
                  then
                     if Pr <= Opr then
                        --  Get rid of duplicates, if ORDER is OK
                        Pa (J .. Pa_Last - 1) := Pa (J + 1 .. Pa_Last);
                        --  Shift PA down 1
                        Pa_Last := Pa_Last - 1;
                        --  because found key duplicate
                     -- Elsif suppresses duplicate lines; e.g., "ludica"
                     -- when TRIM_OUTPUT is off.  See #76.
                     elsif J + 1 <= Pa_Last then
                        if Pa (J) = Pa (J + 1) then
                           Pa (J .. Pa_Last - 1) := Pa (J + 1 .. Pa_Last);
                           Pa_Last := Pa_Last - 1;
                        end if;
                     end if;
                  else
                     J := J + 1;
                  end if;
               end Supress_Key_Check;
            else
               J := J + 1;
            end if;

            Opr := Pr;
         end loop Compress_Loop;
      end;

      for I in 1 .. Pa_Last  loop
         --  Destroy the artificial VAR for PRON 1 X
         if Pa (I).IR.Qual.Pofs = Pron  and then
           Pa (I).IR.Qual.Pron.Decl.Which = 1
         then
            Pa (I).IR.Qual.Pron.Decl.Var := 0;
         end if;

         if Pa (I).IR.Qual.Pofs = V   then
            if Pa (I).IR.Qual.Verb.Con = (3, 4)  then
               --  Fix V 3 4 to be 4th conjugation
               Pa (I).IR.Qual.Verb.Con := (4, 1);
               --    else
               --    --  Set to 0 other VAR for V
               --      PA (I).IR.QUAL.V.CON.VAR := 0;
            end if;
         end if;
      end loop;
   end List_Sweep;

end Words_Engine.List_Sweep;
