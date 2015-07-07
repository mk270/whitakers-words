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

with Strings_Package; use Strings_Package;
with word_parameters; use word_parameters;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
with uniques_package; use uniques_package;
with developer_parameters; use developer_parameters;
with word_support_package; use word_support_package;

procedure list_sweep(pa : in out Parse_Array; pa_last : in out Integer) is
   --  This procedure is supposed to process the Output PARSE_ARRAY at PA level
   --  before it Get turned into SIRAA and DMNPCA in LIST_PACKAGE
   --  Since it does only PARSE_ARRAY it is just cheaking INFLECTIONS, not DICTIOARY

   use Inflection_Record_IO;
   use Dict_IO;

   pr, opr : Parse_Record := Null_Parse_Record;
   de : dictionary_entry := null_dictionary_entry;
   i, j, jj : Integer := 0;
   diff_j : Integer := 0;

   not_only_archaic  : Boolean := False;
   not_only_medieval : Boolean := False;
   not_only_uncommon : Boolean := False;

   function allowed_stem(pr : Parse_Record) return Boolean is
      allowed : Boolean := True;   --  modify as necessary and return it
      --DE : DICTIONARY_ENTRY;
   begin
      --TEXT_IO.PUT("ALLOWED? >"); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
      if pr.D_K not in general..local  then
         return True;
      end if;

      Dict_IO.Read(dict_file(pr.D_K), de, pr.MNPC);

      --  NOUN CHECKS
      case  pr.IR.qual.pofs is
         when n  =>
            if  words_mdev(for_word_list_check)  then
               if (nom <= pr.IR.qual.n.cs) and then
                 (s <= pr.IR.qual.n.number)
               then
                  allowed := True;
               elsif (nom <= pr.IR.qual.n.cs) and then
                 (pr.IR.qual.n.number = p)
               then

                  search_for_pl:
                  declare
                     de : dictionary_entry;
                     mean : Meaning_Type := Null_Meaning_Type;
                  begin
                     allowed := False;
                     Dict_IO.Read(dict_file(pr.D_K), de, pr.MNPC);
                     mean := de.mean;
                     for j in Meaning_Type'First..Meaning_Type'Last-2  loop
                        if mean(j..j+2) = "pl."  then
                           allowed := True;
                           exit;
                        end if;
                     end loop;
                  end search_for_pl;
               else
                  allowed := False;
               end if;
            end if;

         when  adj  =>
            if  words_mdev(for_word_list_check)  then
               if (nom <= pr.IR.qual.adj.cs) and then
                 (s <= pr.IR.qual.adj.number) and then
                 (m <= pr.IR.qual.adj.gender)
               then
                  allowed := True;
               else
                  allowed := False;
               end if;
            end if;
            --  VERB CHECKS
         when  v  =>
            --TEXT_IO.PUT("VERB  ");
            --  Check for Verb 3 1  dic/duc/fac/fer shortened imperative
            --  See G&L 130.5
            declare
               stem : constant String := Trim (pr.Stem);
               last_three : String(1..3);
            begin
               if (pr.IR.qual.v = ((3, 1), (pres, active, imp), 2, s))  and
                 (pr.IR.ending.size = 0)
               then    --  For this special case
                  if stem'Length >= 3  then
                     last_three := stem(stem'Last-2..stem'Last);
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
            if pr.IR.qual.v.tense_voice_mood.mood = imp then
               if (pr.IR.qual.v.tense_voice_mood.tense = pres) and
                 (pr.IR.qual.v.person = 2)
               then
                  null;
               elsif (pr.IR.qual.v.tense_voice_mood.tense = fut) and
                 (pr.IR.qual.v.person = 2 or pr.IR.qual.v.person = 3)
               then
                  null;
               else
                  --PUT("IMP not in permitted person  "); PUT(PR.IR); NEW_LINE;
                  allowed := False;
               end if;
            end if;

            --  Check for V IMPERS and demand that only 3rd person    --  ???????
            if de.part.v.Kind = impers then
               if pr.IR.qual.v.person = 3 then
                  null;
               else
                  --PUT("IMPERS not in 3rd person     "); PUT(PR.IR); NEW_LINE;
                  allowed := False;
               end if;
            end if;

            --  Check for V DEP    and demand PASSIVE
            if de.part.v.Kind = dep then
               --TEXT_IO.PUT("DEP  ");
               if (pr.IR.qual.v.tense_voice_mood.voice = active)  and
                 (pr.IR.qual.v.tense_voice_mood.mood = inf)  and
                 (pr.IR.qual.v.tense_voice_mood.tense = fut)
               then
                  --TEXT_IO.PUT("PASSIVE  ");
                  --TEXT_IO.PUT("DEP    FUT INF not in ACTIVE "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                  allowed := True;
               elsif (pr.IR.qual.v.tense_voice_mood.voice = active)  and
                 (pr.IR.qual.v.tense_voice_mood.mood in ind..inf)
               then
                  --TEXT_IO.PUT("ACTIVE  ");
                  --TEXT_IO.PUT("DEP    not in PASSIVE     NOT ALLOWED   "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                  allowed := False;
               else
                  --TEXT_IO.PUT("??????  ");
                  null;
               end if;
            end if;

            --  Check for V SEMIDEP    and demand PASSIVE ex Perf
            if de.part.v.Kind = semidep then
               if (pr.IR.qual.v.tense_voice_mood.voice = passive)  and
                 (pr.IR.qual.v.tense_voice_mood.tense in pres..fut)  and
                 (pr.IR.qual.v.tense_voice_mood.mood in ind..imp)
               then
                  --PUT("SEMIDEP    Pres not in ACTIVE "); PUT(PR.IR); NEW_LINE;
                  allowed := False;
               elsif (pr.IR.qual.v.tense_voice_mood.voice = active)  and
                 (pr.IR.qual.v.tense_voice_mood.tense in perf..futp )  and
                 (pr.IR.qual.v.tense_voice_mood.mood in ind..imp)
               then
                  --PUT("SEMIDEP    Perf not in PASSIVE "); PUT(PR.IR); NEW_LINE;
                  allowed := False;
               else
                  null;
               end if;
            end if;

            if  words_mdev(for_word_list_check)  then
               if (pr.IR.qual.v.person = 1) and then
                  (pr.IR.qual.v.number = s)
               then
                  if ((de.part.v.Kind in x..intrans)  and
                    (pr.IR.qual.v.tense_voice_mood = (pres, active, ind))) or else
                    ((de.part.v.Kind = dep)  and
                    (pr.IR.qual.v.tense_voice_mood = (pres, passive, ind))) or else
                    ((de.part.v.Kind = semidep)  and
                    (pr.IR.qual.v.tense_voice_mood = (pres, active, ind)))
                  then
                     allowed := True;
                  elsif (de.part.v.Kind = perfdef)  and
                     (pr.IR.qual.v.tense_voice_mood = (perf, active, ind))
                  then
                     allowed := True;
                  else
                     allowed := False;
                  end if;
               elsif de.part.v.Kind = impers then
                  if (pr.IR.qual.v.person = 3)  and then
                    (pr.IR.qual.v.number = s)  and then
                    (pr.IR.qual.v.tense_voice_mood = (pres, active, ind))
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

      if  words_mdev(for_word_list_check) then       --  Non parts
         if pr.IR.qual.pofs in vpar..supine then
            allowed := False;
         end if;
      end if;                                           --  Non parts
      --TEXT_IO.PUT_LINE("Returning FOR ALLOWED    " & BOOLEAN'IMAGE(ALLOWED));
      return allowed;
   end allowed_stem;

   -----------------------------------------------------------

   procedure Order_Parse_Array(sl: in out Parse_Array; diff_j : out Integer) is
      hits : Integer := 0;
      sl_last : Integer := sl'Last;
      sl_last_initial : constant Integer := sl_last;
      sm : Parse_Record;
      has_noun_abbreviation      : Boolean := False;

      function depr (pr : Parse_Record) return dictionary_entry is
         de : dictionary_entry;
      begin
         --TEXT_IO.PUT("DEPR  "); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
         if pr.MNPC = Null_MNPC  then
            return null_dictionary_entry;
         else
            if pr.D_K in general..local  then
               --if PR.MNPC /= OMNPC  then
               Dict_IO.Set_Index(dict_file(pr.D_K), pr.MNPC);
               Dict_IO.Read(dict_file(pr.D_K), de);
               --OMNPC := PR.MNPC;
               --ODE := DE;
               --else
               --DE := ODE;
               --end if;
            elsif pr.D_K = unique  then
               de :=  uniques_de(pr.MNPC);
            end if;
         end if;

         return de;
      end depr;

   begin

      if sl'Length = 0 then
         -- ? diff_j := sl_last_initial - sl_last;
         return;
      end if;

      --  Bubble sort since this list should usually be very small (1-5)
      hit_loop:
      loop
         hits := 0;

         --------------------------------------------------

         switch:
         declare
            function "<" (left, right : quality_record) return Boolean is
            begin
               if left.pofs = right.pofs  and then
                 left.pofs = pron        and then
                 left.pron.decl.which = 1
               then
                  return (left.pron.decl.var < right.pron.decl.var);
               else
                  return Inflections_Package."<"(left, right);
               end if;
            end "<";

            function equ (left, right : quality_record) return Boolean is
            begin
               if left.pofs = right.pofs  and then
                 left.pofs = pron        and then
                 left.pron.decl.which = 1
               then
                  return (left.pron.decl.var = right.pron.decl.var);
               else
                  return Inflections_Package."="(left, right);
               end if;
            end equ;

            function meaning (pr : Parse_Record) return Meaning_Type is
            begin
               return depr(pr).mean;
            end meaning;

         begin
            --  Need to remove duplicates in ARRAY_STEMS
            --  This sort is very sloppy
            --  One problem is that it can mix up some of the order of PREFIX, XXX, LOC
            --  I ought to do this for every set of results from different approaches
            --  not just in one fell swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            inner_loop:
            for i in sl'First..sl_last-1  loop
               --  Maybe <   =  on PR.STEM  -  will have to make up "<"   --  Actually STEM and PART  --  and check that later in print
               if sl(i+1).D_K  > sl(i).D_K   or else  --  Let DICT.LOC list first

                 (sl(i+1).D_K  = sl(i).D_K    and then
                 sl(i+1).MNPC  < sl(i).MNPC)   or else

                 (sl(i+1).D_K  = sl(i).D_K    and then
                 sl(i+1).MNPC  = sl(i).MNPC    and then
                 sl(i+1).IR.qual < sl(i).IR.qual)  or else

                 (sl(i+1).D_K  = sl(i).D_K    and then
                 sl(i+1).MNPC  = sl(i).MNPC    and then
                 equ(sl(i+1).IR.qual, sl(i).IR.qual)  and then
                 meaning(sl(i+1)) < meaning(sl(i)))  or else   --  | is > letter

                 (sl(i+1).D_K  = sl(i).D_K  and then
                 sl(i+1).MNPC  = sl(i).MNPC    and then
                 equ(sl(i+1).IR.qual, sl(i).IR.qual)  and then
                 meaning(sl(i+1)) = meaning(sl(i))   and then
                 sl(i+1).IR.ending.size < sl(i).IR.ending.size)    or else

                 (sl(i+1).D_K  = sl(i).D_K  and then
                 sl(i+1).MNPC  = sl(i).MNPC    and then
                 equ(sl(i+1).IR.qual, sl(i).IR.qual)  and then
                 meaning(sl(i+1)) = meaning(sl(i))   and then
                 sl(i+1).IR.ending.size = sl(i).IR.ending.size  and then
                 Inflections_Package."<"(sl(i+1).IR.qual, sl(i).IR.qual))
               then

                  sm := sl(i);
                  sl(i) := sl(i+1);
                  sl(i+1) := sm;
                  hits := hits + 1;

               end if;

            end loop inner_loop;

         end switch;
         --------------------------------------------------

         exit hit_loop when hits = 0;
      end loop hit_loop;

      --  Fix up the Archaic/Medieval
      if words_mode(Trim_Output)  then
         --  Remove those inflections if MDEV and there is other valid
         --         TEXT_IO.PUT_LINE("SCANNING FOR TRIM   SL'FIRST = " & INTEGER'IMAGE(SL'FIRST) & "   SL'LAST = " & INTEGER'IMAGE(SL'LAST) );
         --         for I in SL'FIRST..SL_LAST  loop
         --         PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
         --         end loop;

         --  Check to see if we can afford to TRIM, if there will be something left over
         for i in sl'First..sl_last  loop
            --TEXT_IO.PUT_LINE("SCANNING FOR TRIM   I = " & INTEGER'IMAGE(I) & "  INFL AGE = " & AGE_TYPE'IMAGE(SL(I).IR.AGE));
            if sl(i).D_K in general..local  then

               Dict_IO.Set_Index(dict_file(sl(i).D_K), sl(i).MNPC);
               --TEXT_IO.PUT(INTEGER'IMAGE(INTEGER(SL(I).MNPC)));
               Dict_IO.Read(dict_file(sl(i).D_K), de);
               --DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;

               if ((sl(i).IR.age = x) or else (sl(i).IR.age > a))  and
                 ((de.tran.Age = x) or else (de.tran.Age > a))
               then
                  not_only_archaic := True;
               end if;
               if ((sl(i).IR.age = x) or else (sl(i).IR.age < f))  and     --  Or E????
                 ((de.tran.Age = x) or else (de.tran.Age < f))     --  Or E????
               then
                  not_only_medieval := True;
               end if;
               if ((sl(i).IR.freq = x) or else (sl(i).IR.freq < c))   and  --  A/X < C   --  C for inflections is uncommon  !!!!
                 ((de.tran.Freq = x) or else (de.tran.Freq < d))  --     --  E for DICTLINE is uncommon  !!!!
               then
                  not_only_uncommon := True;
               end if;

               if sl(i).IR.qual.pofs = n  and then
                 sl(i).IR.qual.n.decl = (9, 8)
               then
                  has_noun_abbreviation := True;
               end if;
            end if;
         end loop;

         --  We order and Trim  within a subset SL, but have to correct the big set PA also
         --  Kill not ALLOWED first, then check the remaining from the top
         --  I am assuming there is no Trim ming of FIXES for AGE/...
         i := sl_last;
         while i >= sl'First  loop
            --  Remove not ALLOWED_STEM & null
            if not allowed_stem(sl(i)) or (pa(i) = Null_Parse_Record) then
               --TEXT_IO.PUT_LINE("Not ALLOWED   SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               Trimmed := True;
               --TEXT_IO.PUT_LINE("Not ALLOWED end  SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
            end if;
            i := i - 1;
         end loop;

         i := sl_last;
         while i >= sl'First  loop
            --TEXT_IO.PUT_LINE("TRIMMING FOR TRIM   I = " & INTEGER'IMAGE(I));
            if (not_only_archaic and words_mdev(omit_archaic)) and then
              sl(i).IR.age = a
            then
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               --TEXT_IO.PUT_LINE("Archaic        SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               Trimmed := True;
            elsif (not_only_medieval and words_mdev(omit_medieval)) and then
               sl(i).IR.age >= f
            then
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               --TEXT_IO.PUT_LINE("Medieval       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               Trimmed := True;
            end if;
            i := i - 1;
         end loop;

         i := sl_last;
         while i >= sl'First  loop
            if (not_only_uncommon and words_mdev(omit_uncommon)) and then
              sl(i).IR.freq >= c
            then      --  Remember A < C
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               --TEXT_IO.PUT_LINE("Uncommon       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               Trimmed := True;
            end if;
            i := i - 1;
         end loop;

         ----------------------------------------------------------------------------
         ----------------------------------------------------------------------------
         ----------------------------------------------------------------------------
         ----------------------------------------------------------------------------
         ----------------------------------------------------------------------------
         ----Big problem.  This area has been generaing exceptions.
         ----At least one difficulty is that suffixes change POFS.
         ----So one has a N inflection (SL) but a V DE
         ----When the program checks for VOC, it wants a N
         ---- and then asks about KIND (P, N, T,...)
         ---- But the DE (v) does not have those
         ---- The solution would be to fix ADD SUFFIX to do somethnig about passing the ADDON KIND
         ----  I do not want to face that now
         ----  It is likely that all this VOC/LOC is worthless anyway.  Maybe lower FREQ in INFLECTS
         ----
         ----  A further complication is the GANT and AO give different results (AO no exception)
         ----  That is probably because the program is in error and the result threrfore unspecified
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
               if (sl(i).IR.qual.pofs /= n) or
                 (   (sl(i).IR.qual /= (n,  ((9, 8), x, x, m)))  and
                 ( Trim (sl(i).Stem)'Length = 1  and then
                 (sl(i).Stem(1) = 'A'  or
                 sl(i).Stem(1) = 'C'  or
                 sl(i).Stem(1) = 'D'  or
                 --SL(I).STEM(1) = 'K'  or      --  No problem here
                 sl(i).Stem(1) = 'L'  or
                 sl(i).Stem(1) = 'M'            --  or
                                                --SL(I).STEM(1) = 'N'  or
                                                --SL(I).STEM(1) = 'P'  or
                                                --SL(I).STEM(1) = 'Q'  or
                                                --SL(I).STEM(1) = 'T'
                 ) )    )
               then
                  sl(i..sl_last-1) := sl(i+1..sl_last);
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

   reset_pronoun_kind:
   declare
      de : dictionary_entry;
   begin
      for i in 1..pa_last  loop
         if pa(i).D_K = general  then
            Dict_IO.Set_Index(dict_file(pa(i).D_K), pa(i).MNPC);
            Dict_IO.Read(dict_file(pa(i).D_K), de);
            if de.part.pofs = pron  and then
              de.part.pron.Decl.which = 1
            then
               pa(i).IR.qual.pron.decl.var := Pronoun_Kind_Type'Pos (de.part.pron.Kind);
               --elsif DE.PART.POFS = PACK  and then
               -- DE.PART.PACK.DECL.WHICH =1  then
               -- PA(I).IR.QUAL.PACK.DECL.VAR := PRONOUN_KIND_TYPE'POS(DE.KIND.PRON_KIND);
            end if;
         end if;
      end loop;
   end reset_pronoun_kind;

   ---------------------------------------------------

   --  NEED TO REMOVE DISALLOWED BEFORE DOING ANYTHING - BUT WITHOUT REORDERING

   --  The problem I seem to have to face first, if not the first problem,
   --  is the situation in which there are several sets of identical IRs with different MNPC
   --  These may be variants with some other stem (e.g., K=3) not affecting the (K=1) word
   --  Or they might be identical forms with different meanings (| additional meanings)
   --  I need to group such common inflections - and pass this on somehow

   --   TEXT_IO.PUT_LINE("PA before SWEEPING in LIST_SWEEP     PA_LAST = " & INTEGER'IMAGE(PA_LAST));
   --   for I in 1..PA_LAST  loop
   --   PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
   --   end loop;

   sweeping:
   --  To remove disallowed stems/inflections and resulting dangling fixes
   declare
      fix_on : Boolean := False;
      pw_on  : Boolean := False;
      p_first : Integer := 1;
      p_last  : Integer := 0;
      subtype xons is Part_Of_Speech_Type range tackon..suffix;

   begin

      j := pa_last;

      while j >= 1  loop        --  Sweep backwards over PA

         if ((pa(j).D_K in addons..yyy) or (pa(j).IR.qual.pofs in xons))   and then
            (pw_on)
         then               --  first FIX/TRICK after regular
            fix_on := True;
            pw_on  := False;
            p_first := j + 1;

            jj := j;
            while pa(jj+1).IR.qual.pofs = pa(jj).IR.qual.pofs  loop
               p_last := jj + 1;
            end loop;

            ----Order internal to this set of inflections

            Order_Parse_Array(pa(p_first..p_last), diff_j);
            pa(p_last-diff_j+1..pa_last-diff_j) := pa(p_last+1..pa_last);
            pa_last := pa_last - diff_j;
            p_first := 1;
            p_last  := 0;

         elsif ((pa(j).D_K in addons..yyy) or (pa(j).IR.qual.pofs in xons))  and then
            (fix_on)
         then               --  another FIX
            --TEXT_IO.PUT_LINE("SWEEP  Another FIX/TRICK  J = " & INTEGER'IMAGE(J));
            null;
         elsif ((pa(j).D_K in addons..yyy)  or
            (pa(j).IR.qual.pofs = x))  and then  --  Kills TRICKS stuff
            (not pw_on)
         then
            pa(p_last-diff_j+1..pa_last-diff_j) := pa(p_last+1..pa_last);
            pa_last := pa_last - diff_j;
            p_last := p_last - 1;
         else
            pw_on := True;
            fix_on := False;
            if p_last <= 0  then
               p_last := j;
            end if;
            if j = 1  then
               Order_Parse_Array(pa(1..p_last), diff_j);
               pa(p_last-diff_j+1..pa_last-diff_j) := pa(p_last+1..pa_last);
               pa_last := pa_last - diff_j;
            end if;
         end if;                                      --  check PART

         j := j - 1;
      end loop;                          --  loop sweep over PA
   end sweeping;

   opr := pa(1);
   --  Last chance to weed out duplicates
   j := 2;

   compress_loop:
   loop
      exit compress_loop when j > pa_last;
      pr := pa(j);
      if pr /= opr  then
         supress_key_check:
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
               if pr <= opr  then       --  Get rid of duplicates, if ORDER is OK
                  pa(j.. pa_last-1) := pa(j+1..pa_last);  --  Shift PA down 1
                  pa_last := pa_last - 1;        --  because found key duplicate
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

   for i in 1..pa_last  loop
      --  Destroy the artificial VAR for PRON 1 X
      if pa(i).IR.qual.pofs = pron  and then
        pa(i).IR.qual.pron.decl.which =1
      then
         pa(i).IR.qual.pron.decl.var := 0;
      end if;

      if pa(i).IR.qual.pofs = v   then
         if pa(i).IR.qual.v.con = (3, 4)  then
            --  Fix V 3 4 to be 4th conjugation
            pa(i).IR.qual.v.con := (4, 1);
            --    else
            --    --  Set to 0 other VAR for V
            --      PA(I).IR.QUAL.V.CON.VAR := 0;
         end if;
      end if;
   end loop;
end list_sweep;
