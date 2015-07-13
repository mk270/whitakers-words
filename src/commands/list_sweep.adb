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

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with word_parameters; use word_parameters;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with uniques_package; use uniques_package;
with developer_parameters; use developer_parameters;
with word_support_package; use word_support_package;
use Latin_Utils;

procedure list_sweep (pa : in out Parse_Array; pa_last : in out Integer) is
   --  This procedure is supposed to process the Output PARSE_ARRAY at PA level
   --  before it Get turned into SIRAA and DMNPCA in LIST_PACKAGE
   --  Since it does only PARSE_ARRAY it is just cheaking INFLECTIONS, not
   --  DICTIOARY

   use Inflection_Record_IO;
   use Dict_IO;

   pr, opr : Parse_Record := Null_Parse_Record;
   de : Dictionary_Entry := Null_Dictionary_Entry;
   i, j, jj : Integer := 0;
   diff_j : Integer := 0;

   not_only_archaic  : Boolean := False;
   not_only_medieval : Boolean := False;
   not_only_uncommon : Boolean := False;

   function allowed_stem (pr : Parse_Record) return Boolean is
      allowed : Boolean := True;
      --  modify as necessary and return it
      --DE : DICTIONARY_ENTRY;
   begin
      --TEXT_IO.PUT ("ALLOWED? >"); PARSE_RECORD_IO.PUT (PR); TEXT_IO.NEW_LINE;
      if pr.D_K not in general .. local  then
         return True;
      end if;

      Dict_IO.Read (Dict_File (pr.D_K), de, pr.MNPC);

      --  NOUN CHECKS
      case  pr.IR.qual.pofs is
         when N  =>
            if  words_mdev (for_word_list_check)  then
               if (Nom <= pr.IR.qual.N.Of_Case) and then
                 (S <= pr.IR.qual.N.number)
               then
                  allowed := True;
               elsif (Nom <= pr.IR.qual.N.Of_Case) and then
                 (pr.IR.qual.N.Number = P)
               then

                  search_for_pl :
                  declare
                     de : Dictionary_Entry;
                     mean : Meaning_Type := Null_Meaning_Type;
                  begin
                     allowed := False;
                     Dict_IO.Read (Dict_File (pr.D_K), de, pr.MNPC);
                     mean := de.Mean;
                     for j in Meaning_Type'First .. Meaning_Type'Last - 2  loop
                        if mean (j .. j + 2) = "pl."  then
                           allowed := True;
                           exit;
                        end if;
                     end loop;
                  end search_for_pl;
               else
                  allowed := False;
               end if;
            end if;

         when  Adj  =>
            if  words_mdev (for_word_list_check)  then
               if (Nom <= pr.IR.qual.Adj.cs) and then
                 (S <= pr.IR.qual.Adj.number) and then
                 (M <= pr.IR.qual.Adj.gender)
               then
                  allowed := True;
               else
                  allowed := False;
               end if;
            end if;
            --  VERB CHECKS
         when  V  =>
            --TEXT_IO.PUT ("VERB  ");
            --  Check for Verb 3 1  dic/duc/fac/fer shortened imperative
            --  See G&L 130.5
            declare
               stem : constant String := Trim (pr.Stem);
               last_three : String (1 .. 3);
            begin
               if (pr.IR.qual.V = ((3, 1), (Pres, Active, Imp), 2, S))  and
                 (pr.IR.ending.size = 0)
               then    --  For this special case
                  if stem'Length >= 3  then
                     last_three := stem (stem'Last - 2 .. stem'Last);
                     if (last_three = "dic")  or
                       (last_three = "duc")  or
                       (last_three = "fac")  or
                       (last_three = "fer")
                     then
                        null;
                     else
                        allowed := False;
                     end if;
                  else
                     allowed := False;
                  end if;
               end if;
            end;

            --  Check for Verb Imperative being in permitted person
            if pr.IR.qual.V.tense_voice_mood.Mood = Imp then
               if (pr.IR.qual.V.tense_voice_mood.Tense = Pres) and
                 (pr.IR.qual.V.person = 2)
               then
                  null;
               elsif (pr.IR.qual.V.tense_voice_mood.Tense = Fut) and
                 (pr.IR.qual.V.person = 2 or pr.IR.qual.V.person = 3)
               then
                  null;
               else
                  allowed := False;
               end if;
            end if;

            --  Check for V IMPERS and demand that only 3rd person
            if de.Part.V.Kind = Impers then
               if pr.IR.qual.V.person = 3 then
                  null;
               else
                  allowed := False;
               end if;
            end if;

            --  Check for V DEP    and demand PASSIVE
            if de.Part.V.Kind = Dep then
               --TEXT_IO.PUT ("DEP  ");
               if (pr.IR.qual.V.tense_voice_mood.Voice = Active)  and
                 (pr.IR.qual.V.tense_voice_mood.Mood = Inf)  and
                 (pr.IR.qual.V.tense_voice_mood.Tense = Fut)
               then
                  --TEXT_IO.PUT ("PASSIVE  ");
                  allowed := True;
               elsif (pr.IR.qual.V.tense_voice_mood.Voice = Active)  and
                 (pr.IR.qual.V.tense_voice_mood.Mood in Ind .. Inf)
               then
                  --TEXT_IO.PUT ("ACTIVE  ");
                  allowed := False;
               else
                  --TEXT_IO.PUT ("??????  ");
                  null;
               end if;
            end if;

            --  Check for V SEMIDEP    and demand PASSIVE ex Perf
            if de.Part.V.Kind = Semidep then
               if (pr.IR.qual.V.tense_voice_mood.Voice = Passive)  and
                 (pr.IR.qual.V.tense_voice_mood.Tense in Pres .. Fut)  and
                 (pr.IR.qual.V.tense_voice_mood.Mood in Ind .. Imp)
               then
                  allowed := False;
               elsif (pr.IR.qual.V.tense_voice_mood.Voice = Active)  and
                 (pr.IR.qual.V.tense_voice_mood.Tense in Perf .. Futp)  and
                 (pr.IR.qual.V.tense_voice_mood.Mood in Ind .. Imp)
               then
                  allowed := False;
               else
                  null;
               end if;
            end if;

            if  words_mdev (for_word_list_check)  then
               if (pr.IR.qual.V.person = 1) and then
                 (pr.IR.qual.V.number = S)
               then
                  if ((de.Part.V.Kind in X .. Intrans)  and
                    (pr.IR.qual.V.tense_voice_mood =
                    (Pres, Active, Ind))) or else
                    ((de.Part.V.Kind = Dep)  and
                    (pr.IR.qual.V.tense_voice_mood =
                    (Pres, Passive, Ind))) or else
                    ((de.Part.V.Kind = Semidep)  and
                    (pr.IR.qual.V.tense_voice_mood = (Pres, Active, Ind)))
                  then
                     allowed := True;
                  elsif (de.Part.V.Kind = Perfdef)  and
                     (pr.IR.qual.V.tense_voice_mood = (Perf, Active, Ind))
                  then
                     allowed := True;
                  else
                     allowed := False;
                  end if;
               elsif de.Part.V.Kind = Impers then
                  if (pr.IR.qual.V.person = 3)  and then
                    (pr.IR.qual.V.number = S)  and then
                    (pr.IR.qual.V.tense_voice_mood = (Pres, Active, Ind))
                  then
                     allowed := True;
                  else
                     allowed := False;
                  end if;
               else
                  allowed := False;
               end if;
            end if;

         when  others  =>
            null;

      end case;

      if  words_mdev (for_word_list_check) then       --  Non parts
         if pr.IR.qual.pofs in Vpar .. Supine then
            allowed := False;
         end if;
      end if;                                           --  Non parts
      return allowed;
   end allowed_stem;

   -----------------------------------------------------------

   procedure Order_Parse_Array
     (sl     : in out Parse_Array;
      diff_j : out Integer)
   is
      hits                       : Integer := 0;
      sl_last                    : Integer := sl'Last;
      sl_last_initial            : constant Integer := sl_last;
      sm                         : Parse_Record;
      has_noun_abbreviation      : Boolean := False;

      function depr (pr : Parse_Record) return Dictionary_Entry is
         de : Dictionary_Entry;
      begin
         --TEXT_IO.PUT ("DEPR  "); PARSE_RECORD_IO.PUT (PR); TEXT_IO.NEW_LINE;
         if pr.MNPC = Null_MNPC  then
            return Null_Dictionary_Entry;
         else
            if pr.D_K in general .. local  then
               --if PR.MNPC /= OMNPC  then
               Dict_IO.Set_Index (Dict_File (pr.D_K), pr.MNPC);
               Dict_IO.Read (Dict_File (pr.D_K), de);
               --OMNPC := PR.MNPC;
               --ODE := DE;
               --else
               --DE := ODE;
               --end if;
            elsif pr.D_K = unique  then
               de :=  uniques_de (pr.MNPC);
            end if;
         end if;

         return de;
      end depr;

   begin

      if sl'Length = 0 then
         diff_j := sl_last_initial - sl_last;
         return;
      end if;

      -- FIXME: this code looks like it's duplicated in another file

      --  Bubble sort since this list should usually be very small (1-5)
      hit_loop :
      loop
         hits := 0;

         --------------------------------------------------

         switch :
         declare
            function "<" (left, right : quality_record) return Boolean is
            begin
               if left.pofs = right.pofs  and then
                 left.pofs = Pron        and then
                 left.Pron.decl.Which = 1
               then
                  return (left.Pron.decl.Var < right.Pron.decl.Var);
               else
                  return Inflections_Package."<"(left, right);
               end if;
            end "<";

            function equ (left, right : quality_record) return Boolean is
            begin
               if left.pofs = right.pofs  and then
                 left.pofs = Pron        and then
                 left.Pron.decl.Which = 1
               then
                  return (left.Pron.decl.Var = right.Pron.decl.Var);
               else
                  return Inflections_Package."="(left, right);
               end if;
            end equ;

            function meaning (pr : Parse_Record) return Meaning_Type is
            begin
               return depr (pr).Mean;
            end meaning;

         begin
            --  Need to remove duplicates in ARRAY_STEMS
            --  This sort is very sloppy
            --  One problem is that it can mix up some of the order of
            --  PREFIX, XXX, LOC

            --  I ought to do this for every set of results from
            --  different approaches not just in one fell swoop
            --  at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            inner_loop :
            for i in sl'First .. sl_last - 1  loop
               --  Maybe <   =  on PR.STEM  -  will have to make up "<"
               --  Actually STEM and PART  --  and check that later in print
               if sl (i + 1).D_K  > sl (i).D_K
                 or else  --  Let DICT.LOC list first

                 (sl (i + 1).D_K  = sl (i).D_K    and then
                 sl (i + 1).MNPC  < sl (i).MNPC)   or else

                 (sl (i + 1).D_K  = sl (i).D_K    and then
                 sl (i + 1).MNPC  = sl (i).MNPC    and then
                 sl (i + 1).IR.qual < sl (i).IR.qual)  or else

                 (sl (i + 1).D_K  = sl (i).D_K    and then
                 sl (i + 1).MNPC  = sl (i).MNPC    and then
                 equ (sl (i + 1).IR.qual, sl (i).IR.qual)  and then
                 meaning (sl (i + 1)) < meaning (sl (i)))
                 or else   --  | is > letter

                 (sl (i + 1).D_K  = sl (i).D_K  and then
                 sl (i + 1).MNPC  = sl (i).MNPC    and then
                 equ (sl (i + 1).IR.qual, sl (i).IR.qual)  and then
                 meaning (sl (i + 1)) = meaning (sl (i))   and then
                 sl (i + 1).IR.ending.size < sl (i).IR.ending.size)    or else

                 (sl (i + 1).D_K  = sl (i).D_K  and then
                 sl (i + 1).MNPC  = sl (i).MNPC    and then
                 equ (sl (i + 1).IR.qual, sl (i).IR.qual)  and then
                 meaning (sl (i + 1)) = meaning (sl (i))   and then
                 sl (i + 1).IR.ending.size = sl (i).IR.ending.size  and then
                 Inflections_Package."<"(sl (i + 1).IR.qual, sl (i).IR.qual))
               then

                  sm := sl (i);
                  sl (i) := sl (i + 1);
                  sl (i + 1) := sm;
                  hits := hits + 1;

               end if;

            end loop inner_loop;

         end switch;
         --------------------------------------------------

         exit hit_loop when hits = 0;
      end loop hit_loop;

      --  Fix up the Archaic/Medieval
      if words_mode (Trim_Output)  then
         --  Check to see if we can afford to TRIM,
         --  if there will be something left over
         for i in sl'First .. sl_last  loop
            if sl (i).D_K in general .. local  then

               Dict_IO.Set_Index (Dict_File (sl (i).D_K), sl (i).MNPC);
               --TEXT_IO.PUT (INTEGER'IMAGE (INTEGER (SL (I).MNPC)));
               Dict_IO.Read (Dict_File (sl (i).D_K), de);
               --DICTIONARY_ENTRY_IO.PUT (DE); TEXT_IO.NEW_LINE;

               if ((sl (i).IR.age = x) or else (sl (i).IR.age > a))  and
                 ((de.Tran.Age = x) or else (de.Tran.Age > a))
               then
                  not_only_archaic := True;
               end if;
               if ((sl (i).IR.age = x) or else (sl (i).IR.age < f))  and
                 --  Or E????
                 ((de.Tran.Age = x) or else (de.Tran.Age < f))
               then
                  not_only_medieval := True;
               end if;
               if ((sl (i).IR.freq = x) or else (sl (i).IR.freq < c))   and
                 --  A/X < C   --  C for inflections is uncommon  !!!!
                 ((de.Tran.Freq = x) or else (de.Tran.Freq < d))
                  --     --  E for DICTLINE is uncommon  !!!!
               then
                  not_only_uncommon := True;
               end if;

               if sl (i).IR.qual.pofs = N  and then
                 sl (i).IR.qual.N.Decl = (9, 8)
               then
                  has_noun_abbreviation := True;
               end if;
            end if;
         end loop;

         --  We order and Trim  within a subset SL, but have to correct the
         --  big set PA also
         --  Kill not ALLOWED first, then check the remaining from the top
         --  I am assuming there is no Trim ming of FIXES for AGE/ .. .
         i := sl_last;
         while i >= sl'First  loop
            --  Remove not ALLOWED_STEM & null
            if not allowed_stem (sl (i)) or (pa (i) = Null_Parse_Record) then
               sl (i .. sl_last - 1) := sl (i + 1 .. sl_last);
               sl_last := sl_last - 1;
               Trimmed := True;
            end if;
            i := i - 1;
         end loop;

         i := sl_last;
         while i >= sl'First  loop
            --TEXT_IO.PUT_LINE ("TRIMMING FOR TRIM   I = " & INTEGER'IMAGE (I));
            if (not_only_archaic and words_mdev (omit_archaic)) and then
              sl (i).IR.age = a
            then
               sl (i .. sl_last - 1) := sl (i + 1 .. sl_last);
               sl_last := sl_last - 1;
               Trimmed := True;
            elsif (not_only_medieval and words_mdev (omit_medieval)) and then
              sl (i).IR.age >= f
            then
               sl (i .. sl_last - 1) := sl (i + 1 .. sl_last);
               sl_last := sl_last - 1;
               Trimmed := True;
            end if;
            i := i - 1;
         end loop;

         i := sl_last;
         while i >= sl'First  loop
            if (not_only_uncommon and words_mdev (omit_uncommon)) and then
              sl (i).IR.freq >= c
            then      --  Remember A < C
               sl (i .. sl_last - 1) := sl (i + 1 .. sl_last);
               sl_last := sl_last - 1;
               Trimmed := True;
            end if;
            i := i - 1;
         end loop;

         ----Big problem.  This area has been generaing exceptions.
         ----At least one difficulty is that suffixes change POFS.
         ----So one has a N inflection (SL) but a V DE
         ----When the program checks for VOC, it wants a N
         ---- and then asks about KIND (P, N, T, .. .)
         ---- But the DE (v) does not have those
         ---- The solution would be to fix ADD SUFFIX to do somethnig about
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
         i := sl_last;
         while i >= sl'First  loop
            if has_noun_abbreviation    and then
              (all_caps and followed_by_period)
            then
               if (sl (i).IR.qual.pofs /= N) or
                 ((sl (i).IR.qual /= (N, ((9, 8), X, X, M)))  and
                 (Trim (sl (i).Stem)'Length = 1  and then
                 (sl (i).Stem (1) = 'A'  or
                 sl (i).Stem (1) = 'C'  or
                 sl (i).Stem (1) = 'D'  or
                 --SL (I).STEM (1) = 'K'  or      --  No problem here
                 sl (i).Stem (1) = 'L'  or
                 sl (i).Stem (1) = 'M'            --  or
                 )))
               then
                  sl (i .. sl_last - 1) := sl (i + 1 .. sl_last);
                  sl_last := sl_last - 1;
                  Trimmed := True;
               end if;
            end if;
            i := i - 1;
         end loop;

      end if;   --  On TRIM

      diff_j := sl_last_initial - sl_last;

   end Order_Parse_Array;

begin                               --  LIST_SWEEP

   if pa'Length = 0 then
      return;
   end if;

   reset_pronoun_kind :
   declare
      de : Dictionary_Entry;
   begin
      for i in 1 .. pa_last  loop
         if pa (i).D_K = general  then
            Dict_IO.Set_Index (Dict_File (pa (i).D_K), pa (i).MNPC);
            Dict_IO.Read (Dict_File (pa (i).D_K), de);
            if de.Part.pofs = Pron  and then
              de.Part.Pron.Decl.Which = 1
            then
               pa (i).IR.qual.Pron.decl.Var :=
                 Pronoun_Kind_Type'Pos (de.Part.Pron.Kind);
            end if;
         end if;
      end loop;
   end reset_pronoun_kind;
   ---------------------------------------------------

   --  NEED TO REMOVE DISALLOWED BEFORE DOING ANYTHING - BUT WITHOUT REORDERING

   --  The problem I seem to have to face first, if not the first problem,
   --  is the situation in which there are several sets of identical IRs
   --  with different MNPC. These may be variants with some other stem
   --  (e.g., K=3) not affecting the (K=1) word. Or they might be
   --  identical forms with different meanings (| additional meanings)
   --  I need to group such common inflections - and pass this on somehow

   sweeping :
   --  To remove disallowed stems/inflections and resulting dangling fixes
   declare
      fix_on : Boolean := False;
      pw_on  : Boolean := False;
      p_first : Integer := 1;
      p_last  : Integer := 0;
      subtype xons is Part_Of_Speech_Type range Tackon .. Suffix;
   begin
      j := pa_last;

      while j >= 1  loop        --  Sweep backwards over PA

         if ((pa (j).D_K in addons .. yyy) or (pa (j).IR.qual.pofs in xons))
           and then (pw_on)
         then               --  first FIX/TRICK after regular
            fix_on := True;
            pw_on  := False;
            p_first := j + 1;

            jj := j;
            while pa (jj + 1).IR.qual.pofs = pa (jj).IR.qual.pofs  loop
               p_last := jj + 1;
            end loop;

            ----Order internal to this set of inflections

            Order_Parse_Array (pa (p_first .. p_last), diff_j);
            pa (p_last - diff_j + 1 .. pa_last - diff_j) :=
              pa (p_last + 1 .. pa_last);
            pa_last := pa_last - diff_j;
            p_first := 1;
            p_last  := 0;

         elsif ((pa (j).D_K in addons .. yyy) or
           (pa (j).IR.qual.pofs in xons))  and then
           (fix_on)
         then               --  another FIX
            null;
         elsif ((pa (j).D_K in addons .. yyy)  or
           (pa (j).IR.qual.pofs = X))  and then  --  Kills TRICKS stuff
           (not pw_on)
         then
            pa (p_last - diff_j + 1 .. pa_last - diff_j) :=
              pa (p_last + 1 .. pa_last);
            pa_last := pa_last - diff_j;
            p_last := p_last - 1;
         else
            pw_on := True;
            fix_on := False;
            if p_last <= 0  then
               p_last := j;
            end if;
            if j = 1  then
               Order_Parse_Array (pa (1 .. p_last), diff_j);
               pa (p_last - diff_j + 1 .. pa_last - diff_j) :=
                 pa (p_last + 1 .. pa_last);
               pa_last := pa_last - diff_j;
            end if;
         end if;                                      --  check PART

         j := j - 1;
      end loop;                          --  loop sweep over PA
   end sweeping;

   opr := pa (1);
   --  Last chance to weed out duplicates
   j := 2;

   compress_loop :
   loop
      exit compress_loop when j > pa_last;
      pr := pa (j);
      if pr /= opr  then
         supress_key_check :
         declare
            function "<=" (a, b : Parse_Record) return Boolean is
            begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
               if a.IR.qual = b.IR.qual  and
                 a.MNPC    = b.MNPC
               then
                  return True;
               else
                  return False;
               end if;
            end "<=";
         begin
            if (pr.D_K /= xxx) and (pr.D_K /= yyy) and  (pr.D_K /= ppp) then
               if pr <= opr then
                  --  Get rid of duplicates, if ORDER is OK
                  pa (j .. pa_last - 1) := pa (j + 1 .. pa_last);
                  --  Shift PA down 1
                  pa_last := pa_last - 1;
                  --  because found key duplicate
               end if;
            else
               j := j + 1;
            end if;
         end supress_key_check;
      else
         j := j + 1;
      end if;

      opr := pr;
   end loop compress_loop;

   for i in 1 .. pa_last  loop
      --  Destroy the artificial VAR for PRON 1 X
      if pa (i).IR.qual.pofs = Pron  and then
        pa (i).IR.qual.Pron.decl.Which = 1
      then
         pa (i).IR.qual.Pron.decl.Var := 0;
      end if;

      if pa (i).IR.qual.pofs = V   then
         if pa (i).IR.qual.V.con = (3, 4)  then
            --  Fix V 3 4 to be 4th conjugation
            pa (i).IR.qual.V.con := (4, 1);
            --    else
            --    --  Set to 0 other VAR for V
            --      PA (I).IR.QUAL.V.CON.VAR := 0;
         end if;
      end if;
   end loop;
end list_sweep;
