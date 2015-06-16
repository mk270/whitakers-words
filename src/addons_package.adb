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
with developer_parameters; use developer_parameters;
with preface;
package body addons_package is
   use text_io;
   use part_of_speech_type_io;
   use target_entry_io;
   use part_entry_io;
   --use KIND_ENTRY_IO;
   use stem_key_type_io;

   function equ(c, d : character) return boolean is
   begin
      if (d = 'u') or (d = 'v')  then
         if (c = 'u') or (c = 'v')  then
            return true;
         else
            return false;
         end if;
      else
         return c = d;
      end if;
   end equ;

   function equ(s, t : string) return boolean is
   begin
      if s'length /= t'length  then
         return false;
      end if;

      for i in 1..s'length  loop
         if not equ(s(s'first+i-1), t(t'first+i-1))  then
            return false;
         end if;
      end loop;

      return true;
   end equ;

   procedure load_addons (file_name : in string) is
      use tackon_entry_io;
      use prefix_entry_io;
      use suffix_entry_io;
      --use DICT_IO;

      s : string(1..100);
      l, last, tic, pre, suf, tac, pac : integer := 0;
      addons_file : text_io.file_type;
      pofs: part_of_speech_type;
      de : dictionary_entry := null_dictionary_entry;
      mean : meaning_type := null_meaning_type;
      m : integer := 1;
      --TG : TARGET_ENTRY;
      tn : tackon_entry;
      pm : prefix_item;
      ts : stem_type;

      procedure get_no_comment_line(f : in text_io.file_type;
                                    s : out string; last : out integer) is
         t : string(1..250) := (others => ' ');
         l : integer := 0;
      begin
         last := 0;
         while not end_of_file(f)  loop
            get_line(f, t, l);
            if l >= 2  and then
              (head(trim(t), 250)(1..2) = "--"  or
              head(trim(t), 250)(1..2) = "  ")  then
               null;
            else
               s(s'first .. l) := t(1..l);
               last := l;
               exit;
            end if;
         end loop;
      end get_no_comment_line;

      procedure extract_fix(s : in string;
                            xfix : out fix_type; xc : out character) is
         st : constant string := trim(s);
         l : constant integer := st'length;
         j : integer := 0;
      begin
         for i in 1..l  loop
            j := i;
            exit when ( (i < l) and then (st(i+1) = ' ') );
         end loop;
         xfix := head(st(1..j), max_fix_size);
         if j = l  then     --  there is no CONNECT CHARACTER
            xc := ' ';
            return;
         else
            for i in j+1..l  loop
               if st(i) /= ' '  then
                  xc := st(i);
                  exit;
               end if;
            end loop;
         end if;
         return;
      end extract_fix;

   begin
      open(addons_file, in_file, file_name);
      preface.put("ADDONS");
      preface.put(" loading ");

      --    if DICT_IO.IS_OPEN(DICT_FILE(D_K))  then
      --      DICT_IO.DELETE(DICT_FILE(D_K));
      --    end if;
      --    DICT_IO.CREATE(DICT_FILE(D_K), DICT_IO.INOUT_FILE,
      --          --ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, DICTIONARY_KIND'IMAGE(D_K)));
      --       "");
      --
      while not end_of_file(addons_file)  loop

         de := null_dictionary_entry;
         get_no_comment_line(addons_file, s, last);
         --TEXT_IO.PUT_LINE(S(1..LAST));
         get(s(1..last), pofs, l);
         case pofs is
            when tackon  =>
               ts := head(trim(s(l+1..last)), max_stem_size);
               de.stems(1) := ts;

               get_line(addons_file, s, last);
               get(s(1..last), tn, l);
               get_line(addons_file, s, last);
               mean := head(s(1..last), max_meaning_size);

               if  tn.base.pofs= pack   and then
                 (tn.base.pack.decl.which = 1 or
                 tn.base.pack.decl.which = 2)  and then
                 mean(1..9) = "PACKON w/"  then
                  pac := pac + 1;
                  packons (pac).pofs:= pofs;
                  packons(pac).tack := ts;
                  packons(pac).entr := tn;
                  --            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
                  --            DE.MEAN := MEAN;
                  --            DICT_IO.WRITE(DICT_FILE(D_K), DE);
                  packons (pac).mnpc := m;
                  means(m) := mean;
                  m := m + 1;

               else
                  tac := tac + 1;
                  tackons (tac).pofs:= pofs;
                  tackons(tac).tack := ts;
                  tackons(tac).entr := tn;
                  --            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
                  --            DE.MEAN := MEAN;
                  --            DICT_IO.WRITE(DICT_FILE(D_K), DE);
                  --            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
                  tackons (tac).mnpc := m;
                  means(m) := mean;
                  m := m + 1;
               end if;

               number_of_packons  := pac;
               number_of_tackons  := tac;

            when prefix  =>

               extract_fix(s(l+1..last), pm.fix, pm.connect);
               get_line(addons_file, s, last);
               get(s(1..last), pm.entr, l);
               get_line(addons_file, s, last);
               mean := head(s(1..last), max_meaning_size);

               if  pm.entr.root = pack     then
                  tic := tic + 1;
                  tickons (tic).pofs:= pofs;
                  tickons(tic).fix  := pm.fix;
                  tickons(tic).connect  := pm.connect;
                  tickons(tic).entr := pm.entr;
                  --            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
                  --            DE.MEAN := MEAN;
                  --            DICT_IO.WRITE(DICT_FILE(D_K), DE);
                  --            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
                  tickons (tic).mnpc := m;
                  means(m) := mean;
                  m := m + 1;

               else
                  pre := pre + 1;
                  prefixes(pre).pofs:= pofs;
                  prefixes(pre).fix  := pm.fix;
                  prefixes(pre).connect  := pm.connect;
                  prefixes(pre).entr := pm.entr;
                  --            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
                  de.mean := mean;
                  --            DICT_IO.WRITE(DICT_FILE(D_K), DE);
                  --            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
                  prefixes(pre).mnpc := m;
                  means(m) := mean;
                  m := m + 1;
               end if;

               number_of_tickons  := tic;
               number_of_prefixes := pre;

            when suffix  =>
               suf := suf + 1;
               suffixes(suf).pofs:= pofs;
               --TEXT_IO.PUT_LINE(S(1..LAST));
               extract_fix(s(l+1..last), suffixes(suf).fix, suffixes(suf).connect);
               --TEXT_IO.PUT("@1");
               get_line(addons_file, s, last);
               --TEXT_IO.PUT("@2");
               --TEXT_IO.PUT_LINE(S(1..LAST) & "<");
               --TEXT_IO.PUT("@2");
               get(s(1..last), suffixes(suf).entr, l);
               --TEXT_IO.PUT("@3");
               get_line(addons_file, s, last);
               --TEXT_IO.PUT("@4");
               mean := head(s(1..last), max_meaning_size);
               --TEXT_IO.PUT("@5");
               --
               --        DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
               --        DE.MEAN := MEAN;
               --        DICT_IO.WRITE(DICT_FILE(D_K), DE);
               --        --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
               suffixes(suf).mnpc := m;
               means(m) := mean;
               m := m + 1;

               number_of_suffixes := suf;

            when others  =>
               text_io.put_line("Bad ADDON    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
               text_io.put_line(s(1..last));
               raise text_io.data_error;
         end case;

      end loop;

      preface.put(tac, 1); preface.put("+");
      preface.put(pac, 2); preface.put(" TACKONS ");
      preface.put(tic, 1); preface.put("+");
      preface.put(pre, 3); preface.put(" PREFIXES ");
      preface.put(suf, 3); preface.put(" SUFFIXES ");

      preface.set_col(60); preface.put_line("--  Loaded correctly");
      close(addons_file);

      --for I in MEANS'RANGE  loop
      --  TEXT_IO.PUT(INTEGER'IMAGE(INTEGER(I))); TEXT_IO.PUT_LINE("--" & MEANS(I));
      --end loop;

   exception
      when text_io.name_error  =>
         preface.put_line("No ADDONS file ");
         null;
      when text_io.data_error  =>
         preface.put_line(s(1..last));
         preface.put_line("No further ADDONS read ");
         close(addons_file);
      when others      =>
         preface.put_line("Exception in LOAD_ADDONS");
         preface.put_line(s(1..last));
   end load_addons;

   function subtract_tackon(w : string; x : tackon_item) return string is
      wd : constant string := trim(w);
      l  : constant integer := wd'length;
      xf : constant string := trim(x.tack);
      z  : constant integer := xf'length;
   begin
      --PUT_LINE("In SUB TACKON " & INTEGER'IMAGE(L) & INTEGER'IMAGE(Z));
      if words_mdev(use_tackons) and then
        l > z  and then
        --WD(L-Z+1..L) = XF(1..Z)  then
        equ(wd(l-z+1..l),  xf(1..z)) then
         --PUT("In SUBTRACT_TACKON we got a hit   "); PUT_LINE(X.TACK);
         return wd(1..l-z);
      else
         --PUT("In SUBTRACT_TACKON    NO    hit   "); PUT_LINE(X.TACK);
         return w;
      end if;
   end subtract_tackon;

   function subtract_prefix(w : string; x : prefix_item) return stem_type is
      wd : constant string := trim(w);
      xf : constant string := trim(x.fix);
      z  : constant integer := xf'length;
      st : stem_type := head(wd, max_stem_size);
   begin
      if words_mdev(use_prefixes) and then
        x /= null_prefix_item and then
        wd'length > z  and then
        --WD(1..Z) = XF(1..Z)  and then
        equ(wd(1..z),  xf(1..z)) and then
        ( (x.connect = ' ') or (wd(z+1) = x.connect) )  then
         st(1..wd'length-z) := wd(z+1..wd'last);
         st(wd'length-z+1..max_stem_size) :=
           null_stem_type(wd'length-z+1..max_stem_size);
      end if;
      --PUT_LINE("SUBTRACT_PREFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
      return st;
   end subtract_prefix;

   function subtract_suffix(w : string; x : suffix_item) return stem_type is
      wd : constant string := trim(w);
      l  : constant integer := wd'length;
      xf : constant string := trim(x.fix);
      z  : constant integer := xf'length;
      st : stem_type := head(wd, max_stem_size);
   begin
      --PUT_LINE("In SUBTRACT_SUFFIX  Z = " & INTEGER'IMAGE(Z) &
      --"  CONNECT >" & X.CONNECT & '<');
      if words_mdev(use_suffixes) and then
        x /= null_suffix_item and then
        wd'length > z  and then
        --WD(L-Z+1..L) = XF(1..Z)  and then
        equ(wd(l-z+1..l),  xf(1..z))  and then
        ( (x.connect = ' ') or (wd(l-z) = x.connect) )  then
         --PUT_LINE("In SUBTRACT_SUFFIX we got a hit");
         st(1..wd'length-z) := wd(1..wd'length-z);
         st(wd'length-z+1..max_stem_size) :=
           null_stem_type(wd'length-z+1..max_stem_size);
      end if;
      --PUT_LINE("SUBTRACT_SUFFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
      return st;
   end subtract_suffix;

   function add_prefix(stem : stem_type;
                       prefix : prefix_item) return stem_type is
      fpx : constant string := trim(prefix.fix) & stem;
   begin
      if words_mdev(use_prefixes)  then
         return head(fpx, max_stem_size);
      else
         return stem;
      end if;
   end add_prefix;

   function add_suffix(stem : stem_type;
                       suffix : suffix_item) return stem_type is
      fpx : constant string := trim(stem) & suffix.fix;
   begin
      if words_mdev(use_suffixes)  then
         return head(fpx, max_stem_size);
      else
         return stem;
      end if;
   end add_suffix;

   --  package body TARGET_ENTRY_IO is separate;

   --  package body TACKON_ENTRY_IO is separate;

   --  package body TACKON_LINE_IO is separate;

   --  package body PREFIX_ENTRY_IO is separate;

   --  package body PREFIX_LINE_IO is separate;

   --  package body SUFFIX_ENTRY_IO is separate;

   --  package body SUFFIX_LINE_IO is separate;

   package body target_entry_io is
      use noun_entry_io;
      use pronoun_entry_io;
      use propack_entry_io;
      use adjective_entry_io;
      use numeral_entry_io;
      use adverb_entry_io;
      use verb_entry_io;
      --  use KIND_ENTRY_IO;
      --
      --  use NOUN_KIND_TYPE_IO;
      --  use PRONOUN_KIND_TYPE_IO;
      --  use INFLECTIONS_PACKAGE.INTEGER_IO;
      --  use VERB_KIND_TYPE_IO;

      spacer : character := ' ';

      noun  : noun_entry;
      pronoun : pronoun_entry;
      propack : propack_entry;
      adjective : adjective_entry;
      numeral : numeral_entry;
      adverb : adverb_entry;
      verb : verb_entry;

      --  NOUN_KIND  : NOUN_KIND_TYPE;
      --  PRONOUN_KIND : PRONOUN_KIND_TYPE;
      --  PROPACK_KIND : PRONOUN_KIND_TYPE;
      --  NUMERAL_VALUE : NUMERAL_VALUE_TYPE;
      --  VERB_KIND : VERB_KIND_TYPE;

      --KIND : KIND_ENTRY;

      procedure get(f : in file_type; p : out target_entry) is
         ps : target_pofs_type := x;
      begin
         get(f, ps);
         get(f, spacer);
         case ps is
            when n =>
               get(f, noun);
               --GET(F, NOUN_KIND);
               p := (n, noun);  --, NOUN_KIND);
            when pron =>
               get(f, pronoun);
               --GET(F, PRONOUN_KIND);
               p := (pron, pronoun);  --, PRONOUN_KIND);
            when pack =>
               get(f, propack);
               --GET(F, PROPACK_KIND);
               p := (pack, propack);  --, PROPACK_KIND);
            when adj =>
               get(f, adjective);
               p := (adj, adjective);
            when num =>
               get(f, numeral);
               --GET(F, NUMERAL_VALUE);
               p := (num, numeral);  --, NUMERAL_VALUE);
            when adv =>
               get(f, adverb);
               p := (adv, adverb);
            when v =>
               get(f, verb);
               --GET(F, VERB_KIND);
               p := (v, verb);  --, VERB_KIND);
            when x =>
               p := (pofs=> x);
         end case;
         return;
      end get;

      procedure get(p : out target_entry) is
         ps : target_pofs_type := x;
      begin
         get(ps);
         get(spacer);
         case ps is
            when n =>
               get(noun);
               --GET(NOUN_KIND);
               p := (n, noun);  --, NOUN_KIND);
            when pron =>
               get(pronoun);
               --GET(PRONOUN_KIND);
               p := (pron, pronoun);  --, PRONOUN_KIND);
            when pack =>
               get(propack);
               --GET(PROPACK_KIND);
               p := (pack, propack);  --, PROPACK_KIND);
            when adj =>
               get(adjective);
               p := (adj, adjective);
            when num =>
               get(numeral);
               --GET(NUMERAL_VALUE);
               p := (num, numeral);  --, NUMERAL_VALUE);
            when adv =>
               get(adverb);
               p := (adv, adverb);
            when v =>
               get(verb);
               --GET(VERB_KIND);
               p := (v, verb);  --, VERB_KIND);
            when x =>
               p := (pofs=> x);
         end case;
         return;
      end get;

      procedure put(f : in file_type; p : in target_entry) is
         c : constant positive := positive(col(f));
      begin
         put(f, p.pofs);
         put(f, ' ');
         case p.pofs is
            when n =>
               put(f, p.n);
               --PUT(F, P.NOUN_KIND);
            when pron =>
               put(f, p.pron);
               --PUT(F, P.PRONOUN_KIND);
            when pack =>
               put(f, p.pack);
               --PUT(F, P.PROPACK_KIND);
            when adj =>
               put(f, p.adj);
            when num =>
               put(f, p.num);
               --PUT(F, P.NUMERAL_VALUE);
            when adv =>
               put(f, p.adv);
            when v =>
               put(f, p.v);
               --PUT(F, P.VERB_KIND);
            when others =>
               null;
         end case;
         put(f, string'((integer(col(f))..target_entry_io.default_width+c-1 => ' ')));
         return;
      end put;

      procedure put(p : in target_entry) is
         c : constant positive := positive(col);
      begin
         put(p.pofs);
         put(' ');
         case p.pofs is
            when n =>
               put(p.n);
               --PUT(P.NOUN_KIND);
            when pron =>
               put(p.pron);
               --PUT(P.PRONOUN_KIND);
            when pack =>
               put(p.pack);
               --PUT(P.PROPACK_KIND);
            when adj =>
               put(p.adj);
            when num =>
               put(p.num);
               --PUT(P.NUMERAL_VALUE);
            when adv =>
               put(p.adv);
            when v =>
               put(p.v);
               --PUT(P.VERB_KIND);
            when others =>
               null;
         end case;
         put(string'((integer(col)..target_entry_io.default_width+c-1 => ' ')));
         return;
      end put;

      procedure get(s : in string; p : out target_entry; last : out integer) is
         l : integer := s'first - 1;
         ps : target_pofs_type := x;
      begin
         get(s, ps, l);
         l := l + 1;
         case ps is
            when n =>
               get(s(l+1..s'last), noun, last);
               --GET(S(L+1..S'LAST), NOUN_KIND, LAST);
               p := (n, noun);  --, NOUN_KIND);
            when pron =>
               get(s(l+1..s'last), pronoun, last);
               --GET(S(L+1..S'LAST), PRONOUN_KIND, LAST);
               p := (pron, pronoun);  --, PRONOUN_KIND);
            when pack =>
               get(s(l+1..s'last), propack, last);
               --GET(S(L+1..S'LAST), PROPACK_KIND, LAST);
               p := (pack, propack);  --, PROPACK_KIND);
            when adj =>
               get(s(l+1..s'last), adjective, last);
               p := (adj, adjective);
            when num =>
               get(s(l+1..s'last), numeral, last);
               --GET(S(L+1..S'LAST), NUMERAL_VALUE, LAST);
               p := (num, numeral);  --, NUMERAL_VALUE);
            when adv =>
               get(s(l+1..s'last), adverb, last);
               p := (adv, adverb);
            when v =>
               get(s(l+1..s'last), verb, last);
               --GET(S(L+1..S'LAST), VERB_KIND, LAST);
               p := (v, verb);  --, VERB_KIND);
            when x =>
               p := (pofs=> x);
         end case;
         return;
      end get;

      procedure put(s : out string; p : in target_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + part_of_speech_type_io.default_width;
         put(s(l+1..m), p.pofs);
         l := m + 1;
         s(l) :=  ' ';
         case p.pofs is
            when n =>
               m := l + noun_entry_io.default_width;
               put(s(l+1..m), p.n);
               --        M := L + NOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
               --        PUT(S(L+1..M), P.NOUN_KIND);
            when pron =>
               m := l + pronoun_entry_io.default_width;
               put(s(l+1..m), p.pron);
               --        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
               --        PUT(S(L+1..M), P.PRONOUN_KIND);
            when pack =>
               m := l + propack_entry_io.default_width;
               put(s(l+1..m), p.pack);
               --        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
               --        PUT(S(L+1..M), P.PROPACK_KIND);
            when adj =>
               m := l + adjective_entry_io.default_width;
               put(s(l+1..m), p.adj);
            when num =>
               m := l + numeral_entry_io.default_width;
               put(s(l+1..m), p.num);
               --        M := L + NUMERAL_VALUE_TYPE_IO_DEFAULT_WIDTH;
               --        PUT(S(L+1..M), P.PRONOUN_KIND);
            when adv =>
               m := l + adverb_entry_io.default_width;
               put(s(l+1..m), p.adv);
            when v =>
               m := l + verb_entry_io.default_width;
               put(s(l+1..m), p.v);
               --        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
               --        PUT(S(L+1..M), P.PRONOUN_KIND);
            when others =>
               null;
         end case;
         s(m+1..s'last) := (others => ' ');
      end put;

   end target_entry_io;

   package body tackon_entry_io is
      procedure get(f : in file_type; i : out tackon_entry) is
      begin
         get(f, i.base);
      end get;

      procedure get(i : out tackon_entry) is
      begin
         get(i.base);
      end get;

      procedure put(f : in file_type; i : in tackon_entry) is
      begin
         put(f, i.base);
      end put;

      procedure put(i : in tackon_entry) is
      begin
         put(i.base);
      end put;

      procedure get(s : in string; i : out tackon_entry; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         get(s(l+1..s'last), i.base, last);
      end get;

      procedure put(s : out string; i : in tackon_entry) is
         l : constant integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + target_entry_io.default_width;
         put(s(l+1..m), i.base);
         s(s'first..s'last) := (others => ' ');
      end put;

   end tackon_entry_io;

   package body prefix_entry_io is
      spacer : character := ' ';

      procedure get(f : in file_type; p : out prefix_entry) is
      begin
         get(f, p.root);
         get(f, spacer);
         get(f, p.target);
      end get;

      procedure get(p : out prefix_entry) is
      begin
         get(p.root);
         get(spacer);
         get(p.target);
      end get;

      procedure put(f : in file_type; p : in prefix_entry) is
      begin
         put(f, p.root);
         put(f, ' ');
         put(f, p.target);
      end put;

      procedure put(p : in prefix_entry) is
      begin
         put(p.root);
         put(' ');
         put(p.target);
      end put;

      procedure get(s : in string; p : out prefix_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), p.root, l);
         l := l + 1;
         get(s(l+1..s'last), p.target, last);
      end get;

      procedure put(s : out string; p : in prefix_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + part_of_speech_type_io.default_width;
         put(s(l+1..m), p.root);
         l := m + 1;
         s(l) :=  ' ';
         m := l + part_of_speech_type_io.default_width;
         put(s(l+1..m), p.target);
         s(m+1..s'last) := (others => ' ');
      end put;

   end prefix_entry_io;

   package body suffix_entry_io is
      spacer : character := ' ';

      procedure get(f : in file_type; p : out suffix_entry) is
      begin
         get(f, p.root);
         get(f, spacer);
         get(f, p.root_key);
         get(f, spacer);
         get(f, p.target);
         get(f, spacer);
         get(f, p.target_key);
      end get;

      procedure get(p : out suffix_entry) is
      begin
         get(p.root);
         get(spacer);
         get(p.root_key);
         get(spacer);
         get(p.target);
         get(spacer);
         get(p.target_key);
      end get;

      procedure put(f : in file_type; p : in suffix_entry) is
      begin
         put(f, p.root);
         put(f, ' ');
         put(f, p.root_key, 2);
         put(f, ' ');
         put(f, p.target);
         put(f, ' ');
         put(f, p.target_key, 2);
      end put;

      procedure put(p : in suffix_entry) is
      begin
         put(p.root);
         put(' ');
         put(p.root_key, 2);
         put(' ');
         put(p.target);
         put(' ');
         put(p.target_key, 2);
      end put;

      procedure get(s : in string; p : out suffix_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         --TEXT_IO.PUT("#1" & INTEGER'IMAGE(L));
         get(s(l+1..s'last), p.root, l);
         --TEXT_IO.PUT("#2" & INTEGER'IMAGE(L));
         l := l + 1;
         get(s(l+1..s'last), p.root_key, l);
         --TEXT_IO.PUT("#3" & INTEGER'IMAGE(L));
         l := l + 1;
         get(s(l+1..s'last), p.target, l);
         --TEXT_IO.PUT("#4" & INTEGER'IMAGE(L));
         l := l + 1;
         get(s(l+1..s'last), p.target_key, last);
         --TEXT_IO.PUT("#5" & INTEGER'IMAGE(LAST));
      end get;

      procedure put(s : out string; p : in suffix_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + part_of_speech_type_io.default_width;
         put(s(l+1..m), p.root);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 2;
         put(s(l+1..m), p.root_key);
         l := m + 1;
         s(l) :=  ' ';
         m := l + target_entry_io.default_width;
         put(s(l+1..m), p.target);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 2;
         put(s(l+1..m), p.target_key);
         s(m+1..s'last) := (others => ' ');
      end put;

   end suffix_entry_io;

begin    --  Initiate body of ADDONS_PACKAGE
         --TEXT_IO.PUT_LINE("Initializing ADDONS_PACKAGE");

   prefix_entry_io.default_width := part_of_speech_type_io.default_width + 1 +
     part_of_speech_type_io.default_width;
   target_entry_io.default_width := part_of_speech_type_io.default_width + 1 +
     numeral_entry_io.default_width; --  Largest

   suffix_entry_io.default_width := part_of_speech_type_io.default_width + 1 +
     2 + 1 +
     target_entry_io.default_width + 1 +
     2;
   tackon_entry_io.default_width := target_entry_io.default_width;

end addons_package;
