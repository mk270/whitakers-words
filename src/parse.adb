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
--
--   the four occurrences of the stanza matching "Add SYPA to PA"

--   the if-branches, and functions, dealing with participles

--   anything nested more than about six levels deep

--   the subprogramme "parse_line" is 430-odd lines long, and depends on
--   variables defined outside it and a bunch of variables from other
--   source files

--   the use of labels entails negatively indenting source lines, and is
--   a major readability/intelligibility problem

with text_io;
with strings_package; use strings_package;
with word_parameters; use word_parameters;
with developer_parameters; use developer_parameters;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with addons_package; use addons_package;
with word_support_package; use word_support_package;
with word_package; use word_package;
with list_package; use list_package;
with tricks_package; use tricks_package;
with put_stat;
with search_english;
with char_utils; use char_utils;

pragma elaborate(word_parameters);
package body parse
is
   use inflections_package.integer_io;
   use inflection_record_io;
   use text_io;

   -- the scope of most of these variables is over-broad
   storage_error_count : integer := 0;

   j, k : integer := 0;

   pa : parse_array(1..100) := (others => null_parse_record);
   syncope_max : constant := 20;
   no_syncope : boolean := false;
   tricks_max : constant := 40;
   sypa : parse_array(1..syncope_max) := (others => null_parse_record);
   trpa : parse_array(1..tricks_max) := (others => null_parse_record);
   pa_last, sypa_last, trpa_last : integer := 0;

   type participle is
      record
         ppl_on : boolean;
         ppl_info : vpar_record;
         compound_tvm : inflections_package.tense_voice_mood_record;
         ppp_meaning : meaning_type;
      end record;

   type verb_to_be (matches : boolean) is
      record
         case matches is
            when true =>
               verb_rec : verb_record;
            when false =>
               null;
         end case;
      end record;

   function is_esse(t : string) return boolean is
   begin
      return trim(t) = "esse";
   end is_esse;

   function is_fuisse(t : string) return boolean is
   begin
      return trim(t) = "fuisse";
   end is_fuisse;

   function is_iri(t : string) return boolean is
   begin
      return trim(t) = "iri";
   end is_iri;

   function get_participle_info(vpar : vpar_record)
     return vpar_record
   is
   begin
      return (
        vpar.con,   --  In this case, there is 1
        vpar.cs,    --  although several different
        vpar.number,--  dictionary entries may fit
        vpar.gender,--  all have same PPL_INFO
        vpar.tense_voice_mood
             );
   end get_participle_info;

   type participle_gloss is
      record
         key : inflections_package.tense_voice_mood_record;
         gloss : string(1 .. 74);
      end record;

   type participle_glosses_arr is array (integer range <>) of participle_gloss;

   participle_glosses : constant participle_glosses_arr :=
     (
      (key => (perf, passive, ppl),
       gloss => "PERF PASSIVE PPL + verb TO_BE => PASSIVE perfect system                   "),
      (key => (fut, active,  ppl),
       gloss => "FUT ACTIVE PPL + verb TO_BE => ACTIVE Periphrastic - about to, going to   "),
      (key => (fut, passive, ppl),
       gloss => "FUT PASSIVE PPL + verb TO_BE => PASSIVE Periphrastic - should/ought/had to")
     );

   -- we pass in the "default" values of a bunch of variables

   -- this function is called in a loop, which used to overwrite
   -- the values of this handful of variables, at least one of
   -- which is a global defined elsewhere

   -- retaining the "defaults" allows us to (re)assign the values
   -- in the caller, which is effectively a no-op in the fall-through
   -- code path; we should pass thie information in as a record of type
   -- "participle" rather than as four separate values, to be clearer
   -- about what is going on, and save wear-and-tear on the stack frame

   -- compare this function with the very similar one directly below it;
   -- they should be factored back together
   function get_pas_nom_participle(parsed_verb : vpar_record;
                           sum_info : verb_record;
                           default_ppl_on : boolean;
                           default_compound_tvm : tense_voice_mood_record;
                           default_ppp_meaning : meaning_type;
                           default_ppl_info : vpar_record)
                          return participle
   is
      compound_tense : tense_type := pres;

      function get_compound_tense(tense : tense_type) return tense_type
      is
      begin
         case tense is
            when pres | perf => return perf; --  Allows PERF for sum
            when impf | plup => return plup;
            when fut         => return futp;
            when others      => return x;
         end case;
      end get_compound_tense;

   begin

      for i in participle_glosses'range loop
         if participle_glosses(i).key = parsed_verb.tense_voice_mood then

            if parsed_verb.tense_voice_mood = (perf, passive, ppl) then
               compound_tense := get_compound_tense(
                 sum_info.tense_voice_mood.tense);
            else
               compound_tense := sum_info.tense_voice_mood.tense;
            end if;

            declare
               ppl_info : constant vpar_record := get_participle_info(
                 parsed_verb);
            begin
               return (
                 ppl_on => true,
                 ppl_info => ppl_info,
                 ppp_meaning => head(
                    participle_glosses(i).gloss, max_meaning_size
                 ),
                 compound_tvm => (
                    compound_tense, passive, sum_info.tense_voice_mood.mood
                 )
               );
            end;
         end if;
      end loop;

      return (
        ppl_on => default_ppl_on,
        ppl_info => default_ppl_info,
        ppp_meaning => default_ppp_meaning,
        compound_tvm => default_compound_tvm
      );
   end;

   -- this function was almost an exact duplicate of the one above; it should
   -- be converted to the form above
   function get_pas_participle(parsed_verb : vpar_record;
                               sum_info : verb_record;
                               trimmed_next_word : string;
                               default_ppl_on : boolean;
                               default_compound_tvm : tense_voice_mood_record;
                               default_ppp_meaning : meaning_type;
                               default_ppl_info : vpar_record)
                              return participle
   is
      -- dummy initial value; never read, but assigned in every code path
      compound_tvm : inflections_package.tense_voice_mood_record :=
        (pres, active, ind);

      ppl_info : vpar_record;


   begin
      if parsed_verb.tense_voice_mood = (perf, passive, ppl)  then
         compound_tvm := (perf, passive, inf);

         ppl_info := get_participle_info(parsed_verb);
         ppp_meaning :=
           head("PERF PASSIVE PPL + esse => PERF PASSIVE INF",
           max_meaning_size);
         return (
           ppl_on => true,
           ppl_info => ppl_info,
           ppp_meaning => ppp_meaning,
           compound_tvm => compound_tvm
                );

      elsif parsed_verb.tense_voice_mood = (fut, active,  ppl)  then
         ppl_info := get_participle_info(parsed_verb);
         if is_esse(trimmed_next_word)  then
            compound_tvm := (fut, active, inf);
            ppp_meaning := head(
              "FUT ACTIVE PPL + esse => "
              & "PRES Periphastic/FUT ACTIVE INF - be about/going to",
              max_meaning_size);
            -- also peri COMPOUND_TVM := (PRES, ACTIVE, INF);
         else   --  fuisse
            compound_tvm := (perf, active, inf);
            ppp_meaning := head(
              "FUT ACT PPL+fuisse => "
              & "PERF ACT INF Periphrastic - to have been about/going to",
              max_meaning_size);
         end if;
         return (
           ppl_on => true,
           ppl_info => ppl_info,
           ppp_meaning => ppp_meaning,
           compound_tvm => compound_tvm
                );

      elsif parsed_verb.tense_voice_mood = (fut, passive, ppl)  then
         ppl_info := get_participle_info(parsed_verb);
         if is_esse(trimmed_next_word)  then
            compound_tvm := (pres, passive, inf);
            ppp_meaning := head(
              "FUT PASSIVE PPL + esse => PRES PASSIVE INF",
              max_meaning_size);
            -- also peri COMPOUND_TVM := (PRES, ACTIVE, INF);
         else   --  fuisse
            compound_tvm := (perf, passive, inf);
            ppp_meaning := head(
              "FUT PASSIVE PPL + fuisse => "
              & "PERF PASSIVE INF Periphrastic - about to, going to",
              max_meaning_size);
         end if;

         return (
           ppl_on => true,
           ppl_info => ppl_info,
           ppp_meaning => ppp_meaning,
           compound_tvm => compound_tvm
                );
      end if;

      return (
           ppl_on => default_ppl_on,
           ppl_info => default_ppl_info,
           ppp_meaning => default_ppp_meaning,
           compound_tvm => default_compound_tvm
             );

   end;

   function is_sum(t : string) return verb_to_be is
      sa : constant array (mood_type range ind..sub,
        tense_type range pres..futp,
        number_type range s..p,
        person_type range 1..3)
        of string(1..9) :=
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
      if t = ""  then
         return verb_to_be'(matches => false);
      elsif t(t'first) /= 's'  and
        t(t'first) /= 'e'  and
        t(t'first) /= 'f'      then
         return verb_to_be'(matches => false);
      end if;
      for l in mood_type range ind..sub  loop
         for k in tense_type range pres..futp  loop
            for j in number_type range s..p  loop
               for i in person_type range 1..3  loop
                  if trim(t) = trim(sa(l, k, j, i))  then
                     return verb_to_be'(
                          matches => true,
                          verb_rec => ((5, 1), (k, active, l), i, j)
                            );
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return verb_to_be'(
        matches => false
             );
   end is_sum;


   procedure enclitic(input_word : string;
                      entering_pa_last : in out integer;
                      have_done_enclitic : in out boolean) is
      save_do_only_fixes : constant boolean := words_mdev(do_only_fixes);
      enclitic_limit : integer := 4;
      try : constant string := lower_case(input_word);
   begin
      if have_done_enclitic  then    return;   end if;

      entering_pa_last := pa_last;
      if pa_last > 0 then enclitic_limit := 1; end if;

      -- loop_over_enclitic_tackons:
      for i in 1..enclitic_limit  loop
         --  If have parse, only do que of que, ne, ve, (est)

         -- remove_a_tackon:
         declare
            less : constant string := subtract_tackon(try, tackons(i));
            save_pa_last  : integer := 0;
         begin
            if less  /= try  then       --  LESS is less
                                        --WORDS_MODE(DO_FIXES) := FALSE;
               word_package.word(less, pa, pa_last);

               if pa_last = 0  then

                  save_pa_last := pa_last;
                  try_slury(less, pa, pa_last, line_number, word_number);
                  if save_pa_last /= 0   then
                     if (pa_last - 1) - save_pa_last = save_pa_last  then
                        pa_last := save_pa_last;
                     end if;
                  end if;

               end if;

               --  Do not SYNCOPE if there is a verb TO_BE or compound already there
               --  I do this here and below, it might be combined but it workd now
               for i in 1..pa_last  loop
                  if pa(i).ir.qual.pofs = v and then
                    pa(i).ir.qual.v.con = (5, 1)  then
                     no_syncope := true;
                  end if;
               end loop;

               sypa_last := 0;
               if words_mdev(do_syncope)  and not no_syncope  then
                  syncope(less, sypa, sypa_last);  --  Want SYNCOPE second to make cleaner LIST

                  pa_last := pa_last + sypa_last;   --  Make syncope another array to avoid PA_LAST = 0 problems
                  pa(1..pa_last) := pa(1..pa_last-sypa_last) & sypa(1..sypa_last);  --  Add SYPA to PA
                  sypa(1..syncope_max) := (1..syncope_max => null_parse_record);   --  Clean up so it does not repeat
                  sypa_last := 0;
               end if;
               no_syncope := false;
               --  Restore FIXES
               --WORDS_MODE(DO_FIXES) := SAVE_DO_FIXES;

               words_mdev(do_only_fixes) := true;
               word(input_word, pa, pa_last);
               words_mdev(do_only_fixes) := save_do_only_fixes;

               if pa_last > entering_pa_last  then
                  --  have a possible word
                  pa_last := pa_last + 1;
                  pa(entering_pa_last+2..pa_last) :=
                    pa(entering_pa_last+1..pa_last-1);
                  pa(entering_pa_last+1) := (tackons(i).tack,
                    ((tackon, null_tackon_record), 0, null_ending_record, x, x),
                    addons, dict_io.count(tackons(i).mnpc));

                  have_done_enclitic := true;
               end if;
               return;
            end if;
         end;
      end loop;
   end enclitic;


   procedure tricks_enclitic(input_word : string;
                             entering_trpa_last : in out integer;
                             have_done_enclitic : boolean) is
      try : constant string := lower_case(input_word);
   begin
      if have_done_enclitic  then    return;   end if;

      entering_trpa_last := trpa_last;

      for i in 1..4  loop   --  que, ne, ve, (est)

         declare
            less : constant string :=
              subtract_tackon(try, tackons(i));
         begin
            if less  /= try  then       --  LESS is less
               try_tricks(less, trpa, trpa_last, line_number, word_number);

               if trpa_last > entering_trpa_last  then      --  have a possible word
                  trpa_last := trpa_last + 1;
                  trpa(entering_trpa_last+2..trpa_last) :=
                    trpa(entering_trpa_last+1..trpa_last-1);
                  trpa(entering_trpa_last+1) := (tackons(i).tack,
                    ((tackon, null_tackon_record), 0, null_ending_record, x, x),
                    addons, dict_io.count(tackons(i).mnpc));
               end if;
               return;
            end if;
         end;
      end loop;
   end tricks_enclitic;

   procedure pass(input_word : string;
                  entering_pa_last : in out integer;
                  have_done_enclitic : in out boolean)
   is
      --  This is the core logic of the program, everything else is details
      save_do_fixes : constant boolean := words_mode(do_fixes);
      save_do_only_fixes : constant boolean := words_mdev(do_only_fixes);
   begin
      --  Do straight WORDS without FIXES/TRICKS, is the word in the dictionary
      words_mode(do_fixes) := false;
      roman_numerals(input_word, pa, pa_last);
      word(input_word, pa, pa_last);

      if pa_last = 0  then
         try_slury(input_word, pa, pa_last, line_number, word_number);
      end if;

      --  Do not SYNCOPE if there is a verb TO_BE or compound already there
      for i in 1..pa_last  loop
         if pa(i).ir.qual.pofs = v and then
           pa(i).ir.qual.v.con = (5, 1)  then
            no_syncope := true;
         end if;
      end loop;

      --  Pure SYNCOPE
      sypa_last := 0;
      if words_mdev(do_syncope)  and not no_syncope  then
         syncope(input_word, sypa, sypa_last);
         pa_last := pa_last + sypa_last;   --  Make syncope another array to avoid PA-LAST = 0 problems
         pa(1..pa_last) := pa(1..pa_last-sypa_last) & sypa(1..sypa_last);  --  Add SYPA to PA
         sypa(1..syncope_max) := (1..syncope_max => null_parse_record);   --  Clean up so it does not repeat
         sypa_last := 0;
      end if;
      no_syncope := false;

      --  There may be a vaild simple parse, if so it is most probable
      --  But I have to allow for the possibility that -que is answer, not colloque V
      enclitic(input_word, entering_pa_last, have_done_enclitic);

      --  Restore FIXES
      words_mode(do_fixes) := save_do_fixes;

      --  Now with only fixes
      if pa_last = 0  and then
        words_mode(do_fixes)  then
         words_mdev(do_only_fixes) := true;
         word(input_word, pa, pa_last);
         sypa_last := 0;
         if words_mdev(do_syncope)  and not no_syncope  then
            syncope(input_word, sypa, sypa_last);
            pa_last := pa_last + sypa_last;   --  Make syncope another array to avoid PA-LAST = 0 problems
            pa(1..pa_last) := pa(1..pa_last-sypa_last) & sypa(1..sypa_last);  --  Add SYPA to PA
            sypa(1..syncope_max) := (1..syncope_max => null_parse_record);   --  Clean up so it does not repeat
            sypa_last := 0;
         end if;
         no_syncope := false;

         enclitic(input_word, entering_pa_last, have_done_enclitic);

         words_mdev(do_only_fixes) := save_do_only_fixes;
      end if;

   end pass;

   procedure parse_line(configuration : configuration_type;
                        input_line : string) is
      l : integer := trim(input_line)'last;
      line : string(1..2500) := (others => ' ');
      w : string(1..l) := (others => ' ');
   begin
      word_number := 0;
      line(1..l) := trim(input_line);

      --  Someday I ought to be interested in punctuation and numbers, but not now
      --      eliminate_not_letters:
      for i in 1..l  loop
         if is_alpha_etc(line(i)) then
            null;
         else
            line(i) := ' ';
         end if;
      end loop;

      j := 1;
      k := 0;

      -- loop over line
      while j <= l  loop

         --  Skip over leading and intervening blanks, looking for comments
         --  Punctuation, numbers, and special characters were cleared above
         for i in k+1..l  loop
            exit when line(j) in 'A'..'Z';
            exit when line(j) in 'a'..'z';
            if i < l  and then
              line(i..i+1) = "--"   then
               return;      --  the rest of the line is comment
            end if;
            j := i + 1;
         end loop;

         exit when j > l;             --  Kludge

         follows_period := false;
         if followed_by_period  then
            followed_by_period := false;
            follows_period := true;
         end if;

         capitalized := false;
         all_caps := false;

         --  Extract the word
         for i in j..l  loop

            --  Although I have removed punctuation above, it may not always be so
            if line(i) = '.'  then
               followed_by_period := true;
               exit;
            end if;
            exit when ((line(i) not in 'A'..'Z') and (line(i) not in 'a'..'z'));
            w(i) := line(i);
            k := i;

         end loop;

         if w(j) in 'A'..'Z'  and then
           k - j >= 1  and then
           w(j+1) in 'a'..'z'  then
            capitalized := true;
         end if;

         all_caps := true;
         for i in j..k  loop
            if w(i) = lower_case(w(i))  then
               all_caps := false;
               exit;
            end if;
         end loop;

         for i in j..k-1  loop               --  Kludge for QVAE
            if w(i) = 'Q'  and then w(i+1) = 'V'  then
               w(i+1) := 'U';
            end if;
         end loop;

         if language = english_to_latin  then
            --  Since we do only one English word per line
            declare
               input_word : constant string := w(j..k);
               pofs : part_of_speech_type := x;
            begin

               --  Extract from the rest of the line
               --  Should do AUX here !!!!!!!!!!!!!!!!!!!!!!!!
               --extract_pofs:
               begin
                  part_of_speech_type_io.get(line(k+1..l), pofs, l);
               exception
                  when others =>
                     pofs := x;
               end;

               search_english(input_word, pofs);
               return;

            end;
         end if;

         declare
            input_word : constant string := w(j..k);
            entering_pa_last : integer := 0;
            entering_trpa_last    : integer := 0;
            have_done_enclitic : boolean := false;


         begin   --  PARSE
            xxx_meaning := null_meaning_type;

            pa_last := 0;
            word_number := word_number + 1;

            pass(input_word, entering_pa_last, have_done_enclitic);

            --if (PA_LAST = 0) or DO_TRICKS_ANYWAY  then
            --  WORD failed, try to modify the word
            if (pa_last = 0)  and then
              not (words_mode(ignore_unknown_names)  and capitalized)  then
               --  WORD failed, try to modify the word
               if words_mode(do_tricks)  then
                  words_mode(do_tricks) := false;
                  --  Turn it off so wont be circular
                  try_tricks(input_word, trpa, trpa_last, line_number, word_number);
                  if trpa_last = 0  then
                     tricks_enclitic(input_word, entering_trpa_last, have_done_enclitic);
                  end if;
                  words_mode(do_tricks) := true;   --  Turn it back on
               end if;

               pa_last := pa_last + trpa_last;   --  Make TRICKS another array to avoid PA-LAST = 0 problems
               pa(1..pa_last) := pa(1..pa_last-trpa_last) & trpa(1..trpa_last);  --  Add SYPA to PA
               trpa(1..tricks_max) := (1..tricks_max => null_parse_record);   --  Clean up so it does not repeat
               trpa_last := 0;

            end if;
            --  At this point we have done what we can with individual words
            --  Now see if there is something we can do with word combinations
            --  For this we have to look ahead

            if pa_last > 0   then
               --  But PA may be killed by ALLOW in LIST_STEMS
               if words_mode(do_compounds)  and
                 not (configuration = only_meanings)  then
              compounds_with_sum:
                  declare
                     nw : string(1..2500) := (others => ' ');
                     nk : integer := 0;

                     compound_tvm   : inflections_package.tense_voice_mood_record;
                     ppl_on : boolean := false;

                     sum_info : verb_record := ((5, 1), (x, active, x), 0, x);
                     ppl_info : vpar_record := ((0, 0), x, x, x, (x, x, x));
                     supine_info : supine_record := ((0, 0), x, x, x);

                     procedure look_ahead is
                        j : integer := 0;
                     begin
                        for i in k+2..l  loop
                           --  Although I have removed punctuation above, it may not always be so
                           exit when is_punctuation(line(i));
                           j := j + 1;
                           nw(j) := line(i);
                           nk := i;
                        end loop;
                     end look_ahead;

                     function next_word return string is
                     begin
                        return trim(nw);
                     end next_word;

                     is_verb_to_be : boolean := false;

                  begin

                     --  Look ahead for sum
                     look_ahead;

                     declare
                        tmp : constant verb_to_be := is_sum(next_word);
                     begin
                        case tmp.matches is
                           when true =>
                              sum_info := tmp.verb_rec;
                           when false =>
                              null;
                        end case;
                        is_verb_to_be := tmp.matches;
                     end;

                     if is_verb_to_be then
                        --  On NEXT_WORD = sum, esse, iri

                        for i in 1..pa_last  loop    --  Check for PPL
                           if pa(i).ir.qual.pofs = vpar and then
                             pa(i).ir.qual.vpar.cs = nom  and then
                             pa(i).ir.qual.vpar.number = sum_info.number  and then
                             ( (pa(i).ir.qual.vpar.tense_voice_mood = (perf, passive, ppl)) or
                             (pa(i).ir.qual.vpar.tense_voice_mood = (fut,  active,  ppl)) or
                             (pa(i).ir.qual.vpar.tense_voice_mood = (fut,  passive, ppl)) )  then

                              --  There is at least one hit, fix PA, and advance J over the sum
                              k := nk;

                           end if;
                        end loop;

                        if k = nk  then
                           --  There was a PPL hit
                       clear_pas_nom_ppl:
                           declare
                              j : integer := pa_last;
                           begin
                              while j >= 1  loop
                                 --  Sweep backwards to kill empty suffixes
                                 if pa(j).ir.qual.pofs in tackon .. suffix
                                   and then ppl_on then
                                    null;

                                 elsif pa(j).ir.qual.pofs = vpar and then
                                   pa(j).ir.qual.vpar.cs = nom  and then
                                   pa(j).ir.qual.vpar.number = sum_info.number  then
                                    declare
                                       part : constant participle := get_pas_nom_participle(pa(j).ir.qual.vpar, sum_info,
                                         ppl_on, compound_tvm, ppp_meaning, ppl_info);
                                    begin
                                       ppl_on := part.ppl_on;
                                       ppl_info := part.ppl_info;
                                       ppp_meaning := part.ppp_meaning;
                                       compound_tvm := part.compound_tvm;
                                    end;
                                 else
                                    pa(j..pa_last-1) := pa(j+1..pa_last);
                                    pa_last := pa_last - 1;
                                    ppl_on := false;
                                 end if;
                                 j := j - 1;
                              end loop;
                           end clear_pas_nom_ppl;

                           pa_last := pa_last + 1;
                           pa(pa_last) :=
                             (head("PPL+" & next_word, max_stem_size),
                             ((v,
                             (ppl_info.con,
                             compound_tvm,
                             sum_info.person,
                             sum_info.number)
                              ), 0, null_ending_record, x, a),
                             ppp, null_mnpc);

                        end if;

                     elsif is_esse(next_word) or is_fuisse(next_word)  then     --  On NEXT_WORD

                        for i in 1..pa_last  loop    --  Check for PPL
                           if pa(i).ir.qual.pofs = vpar and then
                             (((pa(i).ir.qual.vpar.tense_voice_mood = (perf, passive, ppl)) and
                             is_esse(next_word)) or
                             ((pa(i).ir.qual.vpar.tense_voice_mood = (fut,  active,  ppl)) or
                             (pa(i).ir.qual.vpar.tense_voice_mood = (fut,  passive, ppl))) )  then

                              --  There is at least one hit, fix PA, and advance J over the sum
                              k := nk;

                           end if;
                        end loop;

                        if k = nk  then
                           --  There was a PPL hit
                       clear_pas_ppl:
                           declare
                              j : integer := pa_last;
                           begin
                              while j >= 1  loop
                                 --  Sweep backwards to kill empty suffixes
                                 if pa(j).ir.qual.pofs in tackon .. suffix
                                   and then ppl_on then
                                    null;

                                 elsif pa(j).ir.qual.pofs = vpar   then
                                    declare
                                       trimmed_next_word : constant string := next_word;
                                       part : constant participle := get_pas_participle(pa(j).ir.qual.vpar, sum_info, trimmed_next_word,
                                         ppl_on, compound_tvm, ppp_meaning, ppl_info);
                                    begin
                                       ppl_on := part.ppl_on;
                                       ppl_info := part.ppl_info;
                                       ppp_meaning := part.ppp_meaning;
                                       compound_tvm := part.compound_tvm;
                                    end;
                                 else
                                    pa(j..pa_last-1) := pa(j+1..pa_last);
                                    pa_last := pa_last - 1;
                                    ppl_on := false;
                                 end if;
                                 j := j - 1;
                              end loop;
                           end clear_pas_ppl;

                           pa_last := pa_last + 1;
                           pa(pa_last) :=
                             (head("PPL+" & next_word, max_stem_size),
                             ((v,
                             (ppl_info.con,
                             compound_tvm,
                             0,
                             x)
                              ), 0, null_ending_record, x, a),
                             ppp, null_mnpc);

                        end if;

                     elsif is_iri(next_word)  then
                        --  On NEXT_WORD = sum, esse, iri
                        --  Look ahead for sum

                        for j in 1..pa_last  loop    --  Check for SUPINE
                           if pa(j).ir.qual.pofs = supine   and then
                             pa(j).ir.qual.supine.cs = acc    then
                              --  There is at least one hit, fix PA, and advance J over the iri
                              k := nk;

                           end if;
                        end loop;

                        if k = nk  then      --  There was a SUPINE hit
                       clear_pas_supine:
                           declare
                              j : integer := pa_last;
                           begin
                              while j >= 1  loop
                                 --  Sweep backwards to kill empty suffixes
                                 if pa(j).ir.qual.pofs in tackon .. suffix
                                   and then ppl_on then
                                    null;

                                 elsif pa(j).ir.qual.pofs = supine  and then
                                   pa(j).ir.qual.supine.cs = acc  then

                                    ppl_on := true;
                                    supine_info := (pa(j).ir.qual.supine.con,
                                      pa(j).ir.qual.supine.cs,
                                      pa(j).ir.qual.supine.number,
                                      pa(j).ir.qual.supine.gender);

                                    pa_last := pa_last + 1;
                                    pa(pa_last) :=
                                      (head("SUPINE + iri", max_stem_size),
                                      ((v,
                                      (supine_info.con,
                                      (fut, passive, inf),
                                      0,
                                      x)
                                       ), 0, null_ending_record, x, a),
                                      ppp, null_mnpc);
                                    ppp_meaning := head(
                                      "SUPINE + iri => FUT PASSIVE INF - to be about/going/ready to be ~",
                                      max_meaning_size);

                                    k := nk;

                                 else
                                    pa(j..pa_last-1) := pa(j+1..pa_last);
                                    pa_last := pa_last - 1;
                                    ppl_on := false;
                                 end if;
                                 j := j -1;
                              end loop;
                           end clear_pas_supine;
                        end if;

                     end if;       --  On NEXT_WORD = sum, esse, iri

                  end compounds_with_sum;
               end if;       --  On WORDS_MODE(DO_COMPOUNDS)

            end if;

            if  words_mode(write_output_to_file)      then
               list_stems(configuration, output, input_word,
                 input_line, pa, pa_last);
            else
               list_stems(configuration, current_output, input_word,
                 input_line, pa, pa_last);
            end if;

            pa_last := 0;

         exception
            when others  =>
               put_stat("Exception    at "
                 & head(integer'image(line_number), 8)
                 & head(integer'image(word_number), 4)
                 & "   " & head(input_word, 28) & "   "  & input_line);
               raise;

         end;


         ----------------------------------------------------------------------
         ----------------------------------------------------------------------

         j := k + 1;    --  In case it is end of line and we don't look for ' '

         exit when words_mdev(do_only_initial_word);

      end loop;        --  Loop on line

   exception
      --   Have STORAGE_ERROR check in WORD too  ?????????????
      when storage_error  =>    --  I want to again, at least twice
         if words_mdev(do_pearse_codes) then
            text_io.put("00 ");
         end if;
         text_io.put_line(    --  ERROR_FILE,
                              "STORAGE_ERROR Exception in WORDS, try again");
         storage_error_count := storage_error_count + 1;
         if storage_error_count >= 4  then  raise; end if;
         pa_last := 0;
      when give_up =>
         pa_last := 0;
         raise;
      when others  =>    --  I want to try to get on with the next line
         text_io.put_line(    --  ERROR_FILE,
           "Exception in PARSE_LINE processing " & input_line);
         if words_mode(write_unknowns_to_file)  then
            if words_mdev(do_pearse_codes) then
               text_io.put(unknowns, "00 ");
            end if;
            text_io.put(unknowns, input_line(j..k));
            text_io.set_col(unknowns, 30);
            inflections_package.integer_io.put(unknowns, line_number, 5);
            inflections_package.integer_io.put(unknowns, word_number, 3);
            text_io.put_line(unknowns, "    ========   ERROR      ");
         end if;
         pa_last := 0;
   end parse_line;
end parse;
