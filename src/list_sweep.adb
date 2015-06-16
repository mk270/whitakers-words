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

with strings_package; use strings_package;
with word_parameters; use word_parameters;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with uniques_package; use uniques_package;
with developer_parameters; use developer_parameters;
with word_support_package; use word_support_package;

procedure list_sweep(pa : in out parse_array; pa_last : in out integer) is
   --  This procedure is supposed to process the output PARSE_ARRAY at PA level
   --  before it get turned into SIRAA and DMNPCA in LIST_PACKAGE
   --  Since it does only PARSE_ARRAY it is just cheaking INFLECTIONS, not DICTIOARY

   use inflection_record_io;
   use dict_io;

   pr, opr : parse_record := null_parse_record;
   de : dictionary_entry := null_dictionary_entry;
   i, j, jj : integer := 0;
   diff_j : integer := 0;

   not_only_archaic  : boolean := false;
   not_only_medieval : boolean := false;
   not_only_uncommon : boolean := false;

   function allowed_stem(pr : parse_record) return boolean is
      allowed : boolean := true;   --  modify as necessary and return it
                                   --DE : DICTIONARY_ENTRY;
   begin
      --TEXT_IO.PUT("ALLOWED? >"); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
      if pr.d_k not in general..local  then
         return true; 
      end if;

      dict_io.read(dict_file(pr.d_k), de, pr.mnpc);
      
      --  NOUN CHECKS
      case  pr.ir.qual.pofs is

         when n  =>

            if  words_mdev(for_word_list_check)  then
               if (nom <= pr.ir.qual.n.cs) and then
                 (s <= pr.ir.qual.n.number) then
                  allowed := true;
               elsif (nom <= pr.ir.qual.n.cs) and then
                 (pr.ir.qual.n.number = p) then
              search_for_pl:
                  declare
                     de : dictionary_entry;
                     mean : meaning_type := null_meaning_type;
                  begin
                     allowed := false;
                     dict_io.read(dict_file(pr.d_k), de, pr.mnpc);
                     mean := de.mean;
                     for j in meaning_type'first..meaning_type'last-2  loop
                        if mean(j..j+2) = "pl."  then
                           allowed := true;
                           exit;
                        end if;
                     end loop;
                  end search_for_pl;
                  --====================================
               else
                  allowed := false;
               end if;
            end if;

         when  adj  =>

            if  words_mdev(for_word_list_check)  then
               if (nom <= pr.ir.qual.adj.cs) and then
                 (s <= pr.ir.qual.adj.number) and then
                 (m <= pr.ir.qual.adj.gender)  then
                  allowed := true;
               else
                  allowed := false;
               end if;
            end if;

            --  VERB CHECKS

         when  v  =>
            --TEXT_IO.PUT("VERB  ");
            --  Check for Verb 3 1  dic/duc/fac/fer shortened imperative
            --  See G&L 130.5
            declare
               stem : constant string := trim(pr.stem);
               last_three : string(1..3);
            begin
               if (pr.ir.qual.v = ((3, 1), (pres, active, imp), 2, s))  and
                 (pr.ir.ending.size = 0)  then    --  For this special case
                  if stem'length >= 3  then
                     last_three := stem(stem'last-2..stem'last);
                     if (last_three = "dic")  or
                       (last_three = "duc")  or
                       (last_three = "fac")  or
                       (last_three = "fer")  then
                        null;
                     else
                        allowed := false;
                     end if;
                  else
                     allowed := false;
                  end if;
               end if;
            end;

            --  Check for Verb Imperative being in permitted person
            if (pr.ir.qual.v.tense_voice_mood.mood = imp) then
               if (pr.ir.qual.v.tense_voice_mood.tense = pres) and
                 (pr.ir.qual.v.person = 2)  then
                  null;
               elsif (pr.ir.qual.v.tense_voice_mood.tense = fut) and
                 (pr.ir.qual.v.person = 2 or pr.ir.qual.v.person = 3)  then
                  null;
               else
                  --PUT("IMP not in permitted person  "); PUT(PR.IR); NEW_LINE;
                  allowed := false;
               end if;
            end if;

            --  Check for V IMPERS and demand that only 3rd person    --  ???????
            if (de.part.v.kind = impers) then
               if (pr.ir.qual.v.person = 3)  then
                  null;
               else
                  --PUT("IMPERS not in 3rd person     "); PUT(PR.IR); NEW_LINE;
                  allowed := false;
               end if;
            end if;

            --  Check for V DEP    and demand PASSIVE
            if (de.part.v.kind = dep) then
               --TEXT_IO.PUT("DEP  ");
               if (pr.ir.qual.v.tense_voice_mood.voice = active)  and
                 (pr.ir.qual.v.tense_voice_mood.mood = inf)  and
                 (pr.ir.qual.v.tense_voice_mood.tense = fut)  then
                  --TEXT_IO.PUT("PASSIVE  ");
                  --TEXT_IO.PUT("DEP    FUT INF not in ACTIVE "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                  allowed := true;
               elsif (pr.ir.qual.v.tense_voice_mood.voice = active)  and
                 (pr.ir.qual.v.tense_voice_mood.mood in ind..inf)  then
                  --TEXT_IO.PUT("ACTIVE  ");
                  --TEXT_IO.PUT("DEP    not in PASSIVE     NOT ALLOWED   "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                  allowed := false;
               else
                  --TEXT_IO.PUT("??????  ");
                  null;
               end if;
            end if;

            --  Check for V SEMIDEP    and demand PASSIVE ex Perf
            if (de.part.v.kind = semidep) then
               if (pr.ir.qual.v.tense_voice_mood.voice = passive)  and
                 (pr.ir.qual.v.tense_voice_mood.tense in pres..fut)  and
                 (pr.ir.qual.v.tense_voice_mood.mood in ind..imp)  then
                  --PUT("SEMIDEP    Pres not in ACTIVE "); PUT(PR.IR); NEW_LINE;
                  allowed := false;
               elsif (pr.ir.qual.v.tense_voice_mood.voice = active)  and
                 (pr.ir.qual.v.tense_voice_mood.tense in perf..futp )  and
                 (pr.ir.qual.v.tense_voice_mood.mood in ind..imp)  then
                  --PUT("SEMIDEP    Perf not in PASSIVE "); PUT(PR.IR); NEW_LINE;
                  allowed := false;
               else
                  null;
               end if;
            end if;

            if  words_mdev(for_word_list_check)  then
               if (pr.ir.qual.v.person = 1) and then
                 (pr.ir.qual.v.number = s)  then
                  if ((de.part.v.kind in x..intrans)  and
                    (pr.ir.qual.v.tense_voice_mood = (pres, active, ind))) or else
                    ((de.part.v.kind = dep)  and
                    (pr.ir.qual.v.tense_voice_mood = (pres, passive, ind))) or else
                    ((de.part.v.kind = semidep)  and
                    (pr.ir.qual.v.tense_voice_mood = (pres, active, ind))) then
                     allowed := true;
                  elsif ((de.part.v.kind = perfdef)  and
                    (pr.ir.qual.v.tense_voice_mood = (perf, active, ind))) then
                     allowed := true;
                  else
                     allowed := false;
                  end if;
               elsif (de.part.v.kind = impers) then
                  if (pr.ir.qual.v.person = 3)  and then
                    (pr.ir.qual.v.number = s)  and then
                    (pr.ir.qual.v.tense_voice_mood = (pres, active, ind))   then
                     allowed := true;
                  else
                     allowed := false;
                  end if;
               else
                  allowed := false;
               end if;
            end if;

         when  others  =>
            null;

      end case;

      if  words_mdev(for_word_list_check)   then       --  Non parts
         if (pr.ir.qual.pofs in vpar..supine)    then
            allowed := false;
         end if;
      end if;                                           --  Non parts
                                                        --TEXT_IO.PUT_LINE("Returning FOR ALLOWED    " & BOOLEAN'IMAGE(ALLOWED));
      return allowed;

   end allowed_stem;

   -----------------------------------------------------------

   procedure order_parse_array(sl: in out parse_array; diff_j : out integer) is
      hits : integer := 0;
      sl_last : integer := sl'last;
      sl_last_initial : constant integer := sl_last;
      sm : parse_record;
      has_noun_abbreviation      : boolean := false;

      function depr (pr : parse_record) return dictionary_entry is
         de : dictionary_entry;
      begin
         --TEXT_IO.PUT("DEPR  "); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
         if pr.mnpc = null_mnpc  then
            return null_dictionary_entry;
         else
            if pr.d_k in general..local  then
               --if PR.MNPC /= OMNPC  then
               dict_io.set_index(dict_file(pr.d_k), pr.mnpc);
               dict_io.read(dict_file(pr.d_k), de);
               --OMNPC := PR.MNPC;
               --ODE := DE;
               --else
               --DE := ODE;
               --end if;
            elsif pr.d_k = unique  then
               de :=  uniques_de(pr.mnpc);
            end if;
         end if;

         return de;
      end depr;

   begin

      if sl'length = 0              then
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

            function "<" (left, right : quality_record) return boolean is
            begin
               if left.pofs = right.pofs  and then
                 left.pofs = pron        and then
                 left.pron.decl.which = 1    then
                  return (left.pron.decl.var < right.pron.decl.var);
               else
                  return inflections_package."<"(left, right);
               end if;
            end "<";

            function equ (left, right : quality_record) return boolean is
            begin

               if left.pofs = right.pofs  and then
                 left.pofs = pron        and then
                 left.pron.decl.which = 1    then

                  return (left.pron.decl.var = right.pron.decl.var);
               else

                  return inflections_package."="(left, right);
               end if;

            end equ;

            function meaning (pr : parse_record) return meaning_type is
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
            for i in sl'first..sl_last-1  loop
               --  Maybe <   =  on PR.STEM  -  will have to make up "<"   --  Actually STEM and PART  --  and check that later in print
               if sl(i+1).d_k  > sl(i).d_k   or else  --  Let DICT.LOC list first

                 (sl(i+1).d_k  = sl(i).d_k    and then
                 sl(i+1).mnpc  < sl(i).mnpc)   or else

                 (sl(i+1).d_k  = sl(i).d_k    and then
                 sl(i+1).mnpc  = sl(i).mnpc    and then
                 sl(i+1).ir.qual < sl(i).ir.qual)  or else

                 (sl(i+1).d_k  = sl(i).d_k    and then
                 sl(i+1).mnpc  = sl(i).mnpc    and then
                 equ(sl(i+1).ir.qual, sl(i).ir.qual)  and then
                 meaning(sl(i+1)) < meaning(sl(i)))  or else   --  | is > letter

                 (sl(i+1).d_k  = sl(i).d_k  and then
                 sl(i+1).mnpc  = sl(i).mnpc    and then
                 equ(sl(i+1).ir.qual, sl(i).ir.qual)  and then
                 meaning(sl(i+1)) = meaning(sl(i))   and then
                 sl(i+1).ir.ending.size < sl(i).ir.ending.size)    or else

                 (sl(i+1).d_k  = sl(i).d_k  and then
                 sl(i+1).mnpc  = sl(i).mnpc    and then
                 equ(sl(i+1).ir.qual, sl(i).ir.qual)  and then
                 meaning(sl(i+1)) = meaning(sl(i))   and then
                 sl(i+1).ir.ending.size = sl(i).ir.ending.size  and then
                 inflections_package."<"(sl(i+1).ir.qual, sl(i).ir.qual))
               then

                  sm := sl(i);
                  sl(i) := sl(i+1);
                  sl(i+1) := sm;
                  hits := hits + 1;

               end if;

            end loop inner_loop;

         end switch;
         --------------------------------------------------

         exit when hits = 0;
      end loop hit_loop;

      --  Fix up the Archaic/Medieval
      if words_mode(trim_output)  then
         --  Remove those inflections if MDEV and there is other valid
         --         TEXT_IO.PUT_LINE("SCANNING FOR TRIM   SL'FIRST = " & INTEGER'IMAGE(SL'FIRST) & "   SL'LAST = " & INTEGER'IMAGE(SL'LAST) );
         --         for I in SL'FIRST..SL_LAST  loop
         --         PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
         --         end loop;

         --  Check to see if we can afford to TRIM, if there will be something left over
         for i in sl'first..sl_last  loop
            --TEXT_IO.PUT_LINE("SCANNING FOR TRIM   I = " & INTEGER'IMAGE(I) & "  INFL AGE = " & AGE_TYPE'IMAGE(SL(I).IR.AGE));
            if sl(i).d_k in general..local  then

               dict_io.set_index(dict_file(sl(i).d_k), sl(i).mnpc);
               --TEXT_IO.PUT(INTEGER'IMAGE(INTEGER(SL(I).MNPC)));
               dict_io.read(dict_file(sl(i).d_k), de);
               --DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;

               if ((sl(i).ir.age = x) or else (sl(i).ir.age > a))  and
                 ((de.tran.age = x) or else (de.tran.age > a))  then
                  not_only_archaic := true;
               end if;
               if ((sl(i).ir.age = x) or else (sl(i).ir.age < f))  and     --  Or E????
                 ((de.tran.age = x) or else (de.tran.age < f))  then     --  Or E????
                  not_only_medieval := true;
               end if;
               if ((sl(i).ir.freq = x) or else (sl(i).ir.freq < c))   and  --  A/X < C   --  C for inflections is uncommon  !!!!
                 ((de.tran.freq = x) or else (de.tran.freq < d)) then  --     --  E for DICTLINE is uncommon  !!!!
                  not_only_uncommon := true;
               end if;

               if sl(i).ir.qual.pofs = n  and then
                 sl(i).ir.qual.n.decl = (9, 8) then
                  has_noun_abbreviation := true;
               end if;
            end if;
         end loop;

         --  We order and trim within a subset SL, but have to correct the big set PA also
         --  Kill not ALLOWED first, then check the remaining from the top
         --  I am assuming there is no trimming of FIXES for AGE/...
         i := sl_last;
         while i >= sl'first  loop
            if (not allowed_stem(sl(i))   or               --  Remove not ALLOWED_STEM & null
              (pa(i) = null_parse_record))  then
               --TEXT_IO.PUT_LINE("Not ALLOWED   SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               trimmed := true;
               --TEXT_IO.PUT_LINE("Not ALLOWED end  SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
            end if;
            i := i - 1;
         end loop;

         i := sl_last;
         while i >= sl'first  loop
            --TEXT_IO.PUT_LINE("TRIMMING FOR TRIM   I = " & INTEGER'IMAGE(I));
            if (not_only_archaic and words_mdev(omit_archaic)) and then
              sl(i).ir.age = a  then
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               --TEXT_IO.PUT_LINE("Archaic        SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               trimmed := true;
            elsif (not_only_medieval and words_mdev(omit_medieval)) and then
              sl(i).ir.age >= f  then
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               --TEXT_IO.PUT_LINE("Medieval       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               trimmed := true;
            end if;
            i := i - 1;
         end loop;

         i := sl_last;
         while i >= sl'first  loop
            if (not_only_uncommon and words_mdev(omit_uncommon)) and then
              sl(i).ir.freq >= c  then      --  Remember A < C
               sl(i..sl_last-1) := sl(i+1..sl_last);
               sl_last := sl_last - 1;
               --TEXT_IO.PUT_LINE("Uncommon       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               trimmed := true;
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
         while i >= sl'first  loop
            if has_noun_abbreviation    and then
              (all_caps and followed_by_period)  then
               if (sl(i).ir.qual.pofs /= n) or
                 (   (sl(i).ir.qual /= (n,  ((9, 8), x, x, m)))  and
                 ( trim(sl(i).stem)'length = 1  and then
                 (sl(i).stem(1) = 'A'  or
                 sl(i).stem(1) = 'C'  or
                 sl(i).stem(1) = 'D'  or
                 --SL(I).STEM(1) = 'K'  or      --  No problem here
                 sl(i).stem(1) = 'L'  or
                 sl(i).stem(1) = 'M'            --  or
                                                --SL(I).STEM(1) = 'N'  or
                                                --SL(I).STEM(1) = 'P'  or
                                                --SL(I).STEM(1) = 'Q'  or
                                                --SL(I).STEM(1) = 'T'
                 ) )    ) then
                  sl(i..sl_last-1) := sl(i+1..sl_last);
                  sl_last := sl_last - 1;
                  trimmed := true;
               end if;
            end if;
            i := i - 1;
         end loop;

      end if;   --  On TRIM

      diff_j := sl_last_initial - sl_last;

   end order_parse_array;

begin                               --  LIST_SWEEP

   if pa'length = 0              then
      return;
   end if;

reset_pronoun_kind:
    declare
       de : dictionary_entry;
    begin
       for i in 1..pa_last  loop
          if pa(i).d_k = general  then
             dict_io.set_index(dict_file(pa(i).d_k), pa(i).mnpc);
             dict_io.read(dict_file(pa(i).d_k), de);
             if de.part.pofs = pron  and then
               de.part.pron.decl.which =1  then
                pa(i).ir.qual.pron.decl.var := pronoun_kind_type'pos(de.part.pron.kind);
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
       fix_on : boolean := false;
       pw_on  : boolean := false;
       p_first : integer := 1;
       p_last  : integer := 0;
       subtype xons is part_of_speech_type range tackon..suffix;

    begin

       j := pa_last;

       while j >= 1  loop        --  Sweep backwards over PA

          if ((pa(j).d_k in addons..yyy) or (pa(j).ir.qual.pofs in xons))   and then
            (pw_on)     then               --  first FIX/TRICK after regular
             fix_on := true;
             pw_on  := false;
             p_first := j + 1;

             jj := j;
             while pa(jj+1).ir.qual.pofs = pa(jj).ir.qual.pofs  loop
                p_last := jj + 1;
             end loop;

             ----Order internal to this set of inflections

             order_parse_array(pa(p_first..p_last), diff_j);
             pa(p_last-diff_j+1..pa_last-diff_j) := pa(p_last+1..pa_last);
             pa_last := pa_last - diff_j;
             p_first := 1;
             p_last  := 0;

          elsif ((pa(j).d_k in addons..yyy) or (pa(j).ir.qual.pofs in xons))  and then
            (fix_on)     then               --  another FIX
                                            --TEXT_IO.PUT_LINE("SWEEP  Another FIX/TRICK  J = " & INTEGER'IMAGE(J));
             null;

          elsif ((pa(j).d_k in addons..yyy)  or
            (pa(j).ir.qual.pofs = x))  and then  --  Kills TRICKS stuff
            (not pw_on)     then
             pa(p_last-diff_j+1..pa_last-diff_j) := pa(p_last+1..pa_last);
             pa_last := pa_last - diff_j;
             p_last := p_last - 1;

          else
             pw_on := true;
             fix_on := false;
             if p_last <= 0  then
                p_last := j;
             end if;
             if j = 1  then
                order_parse_array(pa(1..p_last), diff_j);
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
       exit when j > pa_last;
       pr := pa(j);
       if pr /= opr  then
      supress_key_check:
          declare
             function "<=" (a, b : parse_record) return boolean is
             begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                if a.ir.qual = b.ir.qual  and
                  a.mnpc    = b.mnpc     then
                   return true;
                else
                   return false;
                end if;
             end "<=";
          begin
             if ((pr.d_k /= xxx) and (pr.d_k /= yyy) and  (pr.d_k /= ppp)) then
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
       if pa(i).ir.qual.pofs = pron  and then
         pa(i).ir.qual.pron.decl.which =1  then
          pa(i).ir.qual.pron.decl.var := 0;
       end if;
       if pa(i).ir.qual.pofs = v   then
          if pa(i).ir.qual.v.con = (3, 4)  then
             --  Fix V 3 4 to be 4th conjugation
             pa(i).ir.qual.v.con := (4, 1);
             --    else
             --    --  Set to 0 other VAR for V
             --      PA(I).IR.QUAL.V.CON.VAR := 0;
          end if;
       end if;
    end loop;

end list_sweep;
