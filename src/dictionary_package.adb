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
package body dictionary_package is
   use stem_key_type_io;

   mnpc_io_default_width : constant natural := 6;
   numeral_value_type_io_default_width : constant natural := 5;
   --PART_WIDTH : NATURAL;

   function number_of_stems(p : part_of_speech_type) return stem_key_type is
   begin
      case p is
         when n       => return 2;
         when pron    => return 2;
         when pack    => return 2;
         when adj     => return 4;
         when num     => return 4;
         when adv     => return 3;
         when v       => return 4;
         when vpar    => return 0;
         when supine  => return 0;
         when prep    => return 1;
         when conj    => return 1;
         when interj  => return 1;
         when others  => return 0;
      end case;
   end number_of_stems;

   package body parse_record_io is
      use inflection_record_io;
      use dictionary_kind_io;
      use mnpc_io;
      spacer : character := ' ';

      procedure get(f : in text_io.file_type; pr: out parse_record) is
      begin
         get(f, pr.stem);
         get(f, spacer);
         get(f, pr.ir);
         get(f, spacer);
         get(f, pr.d_k);
         get(f, spacer);
         get(f, pr.mnpc);
      end get;

      procedure get(pr : out parse_record) is
      begin
         get(pr.stem);
         get(spacer);
         get(pr.ir);
         get(spacer);
         get(pr.d_k);
         get(spacer);
         get(pr.mnpc);
      end get;

      procedure put(f : in text_io.file_type; pr : in parse_record) is
      begin
         put(f, pr.stem);
         put(f, ' ');
         put(f, pr.ir);
         put(f, ' ');
         put(f, pr.d_k);
         put(f, ' ');
         put(f, pr.mnpc);
      end put;

      procedure put(pr : in parse_record) is
      begin
         text_io.put(pr.stem);
         text_io.put(' ');
         inflection_record_io.put(pr.ir);
         text_io.put(' ');
         dictionary_kind_io.put(pr.d_k);
         text_io.put(' ');
         mnpc_io.put(pr.mnpc);
      end put;

      procedure get(s : in string; pr : out parse_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         stem_type_io.get(s, pr.stem, l);
         l := l + 1;
         get(s(l+1..s'last), pr.ir, l);
         l := l + 1;
         get(s(l+1..s'last), pr.d_k, l);
         l := l + 1;
         get(s(l+1..s'last), pr.mnpc, last);
      end get;

      procedure put(s : out string; pr : in parse_record) is
         l : integer := 0;
         m : integer := 0;
      begin
         m := l + max_stem_size;
         s(s'first + l .. s'first - 1 + m) := pr.stem;
         l := m + 1;
         s(s'first - 1 + l) :=  ' ';
         m := l + inflection_record_io.default_width;
         put(s(s'first + l .. s'first - 1 + m), pr.ir);
         l := m + 1;
         s(s'first - 1 + l) :=  ' ';
         m := l + dictionary_kind_io.default_width;
         put(s(s'first + l .. s'first - 1 + m), pr.d_k);
         l := m + 1;
         s(s'first - 1 + l) :=  ' ';
         m := l + mnpc_io_default_width;
         put(s(s'first + l .. s'first - 1 + m), pr.mnpc);
         s(m+1..s'last) := (others => ' ');
      end put;

   end parse_record_io;

   package body noun_entry_io is
      use decn_record_io;
      use gender_type_io;
      use noun_kind_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; n : out noun_entry) is
      begin
         get(f, n.decl);
         get(f, spacer);
         get(f, n.gender);
         get(f, spacer);
         get(f, n.kind);
      end get;

      procedure get(n : out noun_entry) is
      begin
         get(n.decl);
         get(spacer);
         get(n.gender);
         get(spacer);
         get(n.kind);
      end get;

      procedure put(f : in file_type; n : in noun_entry) is
      begin
         put(f, n.decl);
         put(f, ' ');
         put(f, n.gender);
         put(f, ' ');
         put(f, n.kind);
      end put;

      procedure put(n : in noun_entry) is
      begin
         put(n.decl);
         put(' ');
         put(n.gender);
         put(' ');
         put(n.kind);
      end put;

      procedure get(s : in string; n : out noun_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), n.decl, l);
         l := l + 1;
         get(s(l+1..s'last), n.gender, l);
         l := l + 1;
         get(s(l+1..s'last), n.kind, last);
      end get;

      procedure put(s : out string; n : in noun_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), n.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), n.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + noun_kind_type_io.default_width;
         put(s(l+1..m), n.kind);
         s(m+1..s'last) := (others => ' ');
      end put;

   end noun_entry_io;

   package body pronoun_entry_io is
      use decn_record_io;
      use pronoun_kind_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; p : out pronoun_entry) is
      begin
         get(f, p.decl);
         get(f, spacer);
         get(f, p.kind);
      end get;

      procedure get(p : out pronoun_entry) is
      begin
         get(p.decl);
         get(spacer);
         get(p.kind);
      end get;

      procedure put(f : in file_type; p : in pronoun_entry) is
      begin
         put(f, p.decl);
         put(f, ' ');
         put(f, p.kind);
      end put;

      procedure put(p : in pronoun_entry) is
      begin
         put(p.decl);
         put(' ');
         put(p.kind);
      end put;

      procedure get(s : in string; p : out pronoun_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), p.decl, l);
         l := l + 1;
         get(s(l+1..s'last), p.kind, last);
      end get;

      procedure put(s : out string; p : in pronoun_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), p.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + pronoun_kind_type_io.default_width;
         put(s(l+1..m), p.kind);
         s(m+1..s'last) := (others => ' ');
      end put;

   end pronoun_entry_io;

   package body propack_entry_io is
      use decn_record_io;
      use pronoun_kind_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; p : out propack_entry) is
      begin
         get(f, p.decl);
         get(f, spacer);
         get(f, p.kind);
      end get;

      procedure get(p : out propack_entry) is
      begin
         get(p.decl);
         get(spacer);
         get(p.kind);
      end get;

      procedure put(f : in file_type; p : in propack_entry) is
      begin
         put(f, p.decl);
         put(f, ' ');
         put(f, p.kind);
      end put;

      procedure put(p : in propack_entry) is
      begin
         put(p.decl);
         put(' ');
         put(p.kind);
      end put;

      procedure get(s : in string; p : out propack_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), p.decl, l);
         l := l + 1;
         get(s(l+1..s'last), p.kind, last);
      end get;

      procedure put(s : out string; p : in propack_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), p.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + pronoun_kind_type_io.default_width;
         put(s(l+1..m), p.kind);
         s(m+1..s'last) := (others => ' ');
      end put;

   end propack_entry_io;

   package body adjective_entry_io is
      use decn_record_io;
      use gender_type_io;
      use case_type_io;
      use number_type_io;
      use comparison_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; a : out adjective_entry) is
      begin
         get(f, a.decl);
         get(f, spacer);
         get(f, a.co);
      end get;

      procedure get(a : out adjective_entry) is
      begin
         get(a.decl);
         get(spacer);
         get(a.co);
      end get;

      procedure put(f : in file_type; a : in adjective_entry) is
      begin
         put(f, a.decl);
         put(f, ' ');
         put(f, a.co);
      end put;

      procedure put(a : in adjective_entry) is
      begin
         put(a.decl);
         put(' ');
         put(a.co);
      end put;

      procedure get(s : in string; a : out adjective_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), a.decl, l);
         l := l + 1;
         get(s(l+1..s'last), a.co, last);
      end get;

      procedure put(s : out string; a : in adjective_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), a.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + comparison_type_io.default_width;
         put(s(l+1..m), a.co);
         s(m+1..s'last) := (others => ' ');
      end put;

   end adjective_entry_io;

   package body numeral_entry_io is
      use decn_record_io;
      use numeral_sort_type_io;
      use inflections_package.integer_io;
      spacer : character := ' ';

      num_out_size : constant := 5;    --  Set in spec  !!!!!!!!!!!!!!!!!!!!!!!!!

      procedure get(f : in file_type; num : out numeral_entry) is
      begin
         get(f, num.decl);
         get(f, spacer);
         get(f, num.sort);
         get(f, spacer);
         get(f, num.value);
      end get;

      procedure get(num : out numeral_entry) is
      begin
         get(num.decl);
         get(spacer);
         get(num.sort);
         get(spacer);
         get(num.value);
      end get;

      procedure put(f : in file_type; num : in numeral_entry) is
      begin
         put(f, num.decl);
         put(f, ' ');
         put(f, num.sort);
         put(f, ' ');
         put(f, num.value, num_out_size);
      end put;

      procedure put(num : in numeral_entry) is
      begin
         put(num.decl);
         put(' ');
         put(num.sort);
         put(' ');
         put(num.value, num_out_size);
      end put;

      procedure get(s : in string; num : out numeral_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         --TEXT_IO.PUT("+1");
         get(s(l+1..s'last), num.decl, l);
         --TEXT_IO.PUT("+2");
         l := l + 1;
         get(s(l+1..s'last), num.sort, l);
         --TEXT_IO.PUT("+3");
         l := l + 1;
         get(s(l+1..s'last), num.value, last);
         --TEXT_IO.PUT("+4");
      end get;

      procedure put(s : out string; num : in numeral_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), num.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + numeral_sort_type_io.default_width;
         put(s(l+1..m), num.sort);
         l := m + 1;
         s(l) :=  ' ';
         --M := L + NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH;
         m := l + num_out_size;
         put(s(l+1..m), num.value);
         s(m+1..s'last) := (others => ' ');
      end put;

   end numeral_entry_io;

   package body adverb_entry_io is
      use comparison_type_io;

      procedure get(f : in file_type; a : out adverb_entry) is
      begin
         get(f, a.co);
      end get;

      procedure get(a : out adverb_entry) is
      begin
         get(a.co);
      end get;

      procedure put(f : in file_type; a : in adverb_entry) is
      begin
         put(f, a.co);
      end put;

      procedure put(a : in adverb_entry) is
      begin
         put(a.co);
      end put;

      procedure get(s : in string; a : out adverb_entry; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         get(s(l+1..s'last), a.co, last);
      end get;

      procedure put(s : out string; a : in adverb_entry) is
         l : constant integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + comparison_type_io.default_width;
         put(s(l+1..m), a.co);
         s(m+1..s'last) := (others => ' ');
      end put;

   end adverb_entry_io;

   package body verb_entry_io is
      use decn_record_io;
      use verb_kind_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; v : out verb_entry) is
      begin
         get(f, v.con);
         get(f, spacer);
         get(f, v.kind);
      end get;

      procedure get(v : out verb_entry) is
      begin
         get(v.con);
         get(spacer);
         get(v.kind);
      end get;

      procedure put(f : in file_type; v : in verb_entry) is
      begin
         put(f, v.con);
         put(f, ' ');
         put(f, v.kind);
      end put;

      procedure put(v : in verb_entry) is
      begin
         put(v.con);
         put(' ');
         put(v.kind);
      end put;

      procedure get(s : in string; v : out verb_entry; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), v.con, l);
         l := l + 1;
         get(s(l+1..s'last), v.kind, last);
      end get;

      procedure put(s : out string; v : in verb_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), v.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + verb_kind_type_io.default_width;
         put(s(l+1..m), v.kind);
         s(m+1..s'last) := (others => ' ');
      end put;

   end verb_entry_io;

   package body preposition_entry_io is
      use case_type_io;

      procedure get(f : in file_type; p : out preposition_entry) is
      begin
         get(f, p.obj);
      end get;

      procedure get(p : out preposition_entry) is
      begin
         get(p.obj);
      end get;

      procedure put(f : in file_type; p : in preposition_entry) is
      begin
         put(f, p.obj);
      end put;

      procedure put(p : in preposition_entry) is
      begin
         put(p.obj);
      end put;

      procedure get(s : in string; p : out preposition_entry; last : out integer) is
      begin
         get(s, p.obj, last);
      end get;

      procedure put(s : out string; p : in preposition_entry) is
         l : constant integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + case_type_io.default_width;
         put(s(l+1..m), p.obj);
         s(m+1..s'last) := (others => ' ');
      end put;

   end preposition_entry_io;

   package body conjunction_entry_io is
      null_conjunction_entry : conjunction_entry;
      
      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; c : out conjunction_entry) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         c := null_conjunction_entry;
      end get;

      procedure get(c : out conjunction_entry) is
      begin
         c := null_conjunction_entry;
      end get;

      procedure put(f : in file_type; c : in conjunction_entry) is
      begin
         null;
      end put;

      procedure put(c : in conjunction_entry) is
      begin
         null;
      end put;

      procedure get(s : in string; c : out conjunction_entry; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         c := null_conjunction_entry;
         last := l;
      end get;

      pragma Warnings (Off, "formal parameter ""c"" is not referenced");
      procedure put(s : out string; c : in conjunction_entry) is
         pragma Warnings (On, "formal parameter ""c"" is not referenced");    
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end conjunction_entry_io;

   package body interjection_entry_io is
      null_interjection_entry : interjection_entry;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; i : out interjection_entry) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");    
      begin
         i := null_interjection_entry;
      end get;

      procedure get(i : out interjection_entry) is
      begin
         i := null_interjection_entry;
      end get;

      procedure put(f : in file_type; i : in interjection_entry) is
      begin
         null;
      end put;

      procedure put(i : in interjection_entry) is
      begin
         null;
      end put;

      procedure get(s : in string; i : out interjection_entry; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         i := null_interjection_entry;
         last := l;
      end get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure put(s : out string; i : in interjection_entry) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");    
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end interjection_entry_io;

   function "<" (left, right : part_entry) return boolean is
   begin
      if left.pofs = right.pofs  then
         case left.pofs is
            when n =>
               if left.n.decl < right.n.decl  or else
                 (left.n.decl = right.n.decl  and then
                 left.n.gender < right.n.gender)  or else
                 ((left.n.decl = right.n.decl  and
                 left.n.gender = right.n.gender)  and then
                 left.n.kind < right.n.kind)  then
                  return true;
               end if;
            when pron =>
               if left.pron.decl < right.pron.decl  or else
                 (left.pron.decl = right.pron.decl  and then
                 left.pron.kind < right.pron.kind)  then
                  return true;
               end if;
            when pack =>
               if left.pack.decl < right.pack.decl  or else
                 (left.pack.decl = right.pack.decl  and then
                 left.pack.kind < right.pack.kind)  then
                  return true;
               end if;
            when adj =>
               if left.adj.decl < right.adj.decl   or else
                 (left.adj.decl = right.adj.decl  and then
                 left.adj.co < right.adj.co)   then
                  return true;
               end if;
            when num =>
               if left.num.decl < right.num.decl  or else
                 (left.num.decl = right.num.decl  and then
                 left.num.sort < right.num.sort)  or else
                 ((left.num.decl = right.num.decl)  and then
                 (left.num.sort = right.num.sort)   and then
                 left.num.value < right.num.value)   then
                  return true;
               end if;when adv =>
                  return left.adv.co < right.adv.co;
            when v =>
               if (left.v.con < right.v.con)  or else
                 (left.v.con = right.v.con  and then
                 left.v.kind < right.v.kind)  then
                  return true;
               end if;
            when prep =>
               return left.prep.obj < right.prep.obj;
            when others =>
               null;
         end case;
      else
         return left.pofs < right.pofs;
      end if;
      return false;
   exception
      when constraint_error  =>
         return left.pofs < right.pofs;
   end "<";

   package body part_entry_io is
      use part_of_speech_type_io;
      use noun_entry_io;
      use pronoun_entry_io;
      use propack_entry_io;
      use adjective_entry_io;
      use numeral_entry_io;
      use adverb_entry_io;
      use verb_entry_io;
      use preposition_entry_io;
      use conjunction_entry_io;
      use interjection_entry_io;
      spacer : character := ' ';

      noun : noun_entry;
      pronoun : pronoun_entry;
      propack : propack_entry;
      adjective : adjective_entry;
      numeral : numeral_entry;
      adverb : adverb_entry;
      verb : verb_entry;
      preposition : preposition_entry;
      conjunction : conjunction_entry;
      interjection : interjection_entry;

      procedure get(f : in file_type; p : out part_entry) is
         ps : part_of_speech_type := x;
         c : constant positive_count := col(f);
      begin
         get(f, ps);
         get(f, spacer);
         case ps is
            when n =>
               get(f, noun);
               p := (n, noun);
            when pron =>
               get(f, pronoun);
               p := (pron, pronoun);
            when pack =>
               get(f, propack);
               p := (pack, propack);
            when adj =>
               get(f, adjective);
               p := (adj, adjective);
            when num =>
               get(f, numeral);
               p := (num, numeral);
            when adv =>
               get(f, adverb);
               p := (adv, adverb);
            when v =>
               get(f, verb);
               p := (v, verb);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               get(f, preposition);
               p := (prep, preposition);
            when conj =>
               get(f, conjunction);
               p := (conj, conjunction);
            when interj =>
               get(f, interjection);
               p := (interj, interjection);
            when prefix =>
               p := (pofs => prefix);
            when suffix =>
               p := (pofs => suffix);
            when tackon =>
               p := (pofs => tackon);
            when x =>
               p := (pofs => x);
         end case;
         set_col(f, positive_count(part_entry_io.default_width)+c);
         return;
      end get;

      procedure get(p : out part_entry) is
         ps : part_of_speech_type := x;
      begin
         get(ps);
         get(spacer);
         case ps is
            when n =>
               get(noun);
               p := (n, noun);
            when pron =>
               get(pronoun);
               p := (pron, pronoun);
            when pack =>
               get(propack);
               p := (pack, propack);
            when adj =>
               get(adjective);
               p := (adj, adjective);
            when num =>
               get(numeral);
               p := (num, numeral);
            when adv =>
               get(adverb);
               p := (adv, adverb);
            when v =>
               get(verb);
               p := (v, verb);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               get(preposition);
               p := (prep, preposition);
            when conj =>
               get(conjunction);
               p := (conj, conjunction);
            when interj =>
               get(interjection);
               p := (interj, interjection);
            when prefix =>
               p := (pofs => prefix);
            when suffix =>
               p := (pofs => suffix);
            when tackon =>
               p := (pofs => tackon);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end get;

      procedure put(f : in file_type; p : in part_entry) is
      begin
         put(f, p.pofs);
         put(f, ' ');
         case p.pofs is
            when n =>
               put(f, p.n);
            when pron =>
               put(f, p.pron);
            when pack =>
               put(f, p.pack);
            when adj =>
               put(f, p.adj);
            when num =>
               put(f, p.num);
            when adv =>
               put(f, p.adv);
            when v =>
               put(f, p.v);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               put(f, p.prep);
            when conj =>
               put(f, p.conj);
            when interj =>
               put(f, p.interj);
            when others =>
               null;
         end case;
         --PUT(F, STRING'((INTEGER(COL(F))..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
         return;
      end put;

      procedure put(p : in part_entry) is
      begin
         put(p.pofs);
         put(' ');
         case p.pofs is
            when n =>
               put(p.n);
            when pron =>
               put(p.pron);
            when pack =>
               put(p.pack);
            when adj =>
               put(p.adj);
            when num =>
               put(p.num);
            when adv =>
               put(p.adv);
            when v =>
               put(p.v);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               put(p.prep);
            when conj =>
               put(p.conj);
            when interj =>
               put(p.interj);
            when others =>
               null;
         end case;
         --PUT(STRING'((INTEGER(COL)..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
         return;
      end put;

      procedure get(s : in string; p : out part_entry; last : out integer) is
         l : integer := s'first - 1;
         ps : part_of_speech_type := x;
      begin
         last := l;      --  In case it is not set later
         get(s, ps, l);
         l := l + 1;
         case ps is
            when n =>
               get(s(l+1..s'last), noun, last);
               p := (n, noun);
            when pron =>
               get(s(l+1..s'last), pronoun, last);
               p := (pron, pronoun);
            when pack =>
               get(s(l+1..s'last), propack, last);
               p := (pack, propack);
            when adj =>
               get(s(l+1..s'last), adjective, last);
               p := (adj, adjective);
            when num =>
               get(s(l+1..s'last), numeral, last);
               p := (num, numeral);
            when adv =>
               get(s(l+1..s'last), adverb, last);
               p := (adv, adverb);
            when v =>
               get(s(l+1..s'last), verb, last);
               p := (v, verb);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               get(s(l+1..s'last), preposition, last);
               p := (prep, preposition);
            when conj =>
               get(s(l+1..s'last), conjunction, last);
               p := (conj, conjunction);
            when interj =>
               get(s(l+1..s'last), interjection, last);
               p := (interj, interjection);
            when prefix =>
               p := (pofs => prefix);
            when suffix =>
               p := (pofs => suffix);
            when tackon =>
               p := (pofs => tackon);
            when x =>
               p := (pofs => x);
         end case;
      end get;

      procedure put(s : out string; p : in part_entry) is
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
            when pron =>
               m := l + pronoun_entry_io.default_width;
               put(s(l+1..m), p.pron);
            when pack =>
               m := l + propack_entry_io.default_width;
               put(s(l+1..m), p.pack);
            when adj =>
               m := l + adjective_entry_io.default_width;
               put(s(l+1..m), p.adj);
            when num =>
               m := l + numeral_entry_io.default_width;
               put(s(l+1..m), p.num);
            when adv =>
               m := l + adverb_entry_io.default_width;
               put(s(l+1..m), p.adv);
            when v =>
               m := l + verb_entry_io.default_width;
               put(s(l+1..m), p.v);
            when vpar =>
               null;                --  No VAPR entryR
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               m := l + preposition_entry_io.default_width;
               put(s(l+1..m), p.prep);
            when conj =>
               m := l + conjunction_entry_io.default_width;
               put(s(l+1..m), p.conj);
            when interj =>
               m := l + interjection_entry_io.default_width;
               put(s(l+1..m), p.interj);
            when others =>
               null;
         end case;
         --S(M+1..S'LAST) := (others => ' ');
      end put;

   end part_entry_io;

   package body kind_entry_io is
      use noun_kind_type_io;
      use pronoun_kind_type_io;
      use inflections_package.integer_io;
      use verb_kind_type_io;

      noun_kind  : noun_kind_type;
      pronoun_kind : pronoun_kind_type;
      propack_kind : pronoun_kind_type;
      verb_kind : verb_kind_type;
      vpar_kind : verb_kind_type;
      supine_kind : verb_kind_type;
      numeral_value : numeral_value_type;

      procedure get(f : in file_type;
                    ps : in part_of_speech_type; p : out kind_entry) is
      begin
         case ps is
            when n =>
               get(f, noun_kind);
               p := (n, noun_kind);
            when pron =>
               get(f, pronoun_kind);
               p := (pron, pronoun_kind);
            when pack =>
               get(f, propack_kind);
               p := (pack, propack_kind);
            when adj =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => adj);
            when num =>
               get(f, numeral_value);
               p := (num, numeral_value);
            when adv =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => adv);
            when v =>
               get(f, verb_kind);
               p := (v, verb_kind);
            when vpar =>
               get(f, vpar_kind);
               p := (vpar, vpar_kind);
            when supine =>
               get(f, supine_kind);
               p := (supine, supine_kind);
            when prep =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => prep);
            when conj =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => conj);
            when interj =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => interj);
            when tackon =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => tackon);
            when prefix =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => prefix);
            when suffix =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => suffix);
            when x =>
               set_col(f, col(f) + positive_count(kind_entry_io.default_width));
               p := (pofs => x);
         end case;
         return;
      end get;

      procedure get(ps : in part_of_speech_type; p : out kind_entry) is
      begin
         case ps is
            when n =>
               get(noun_kind);
               p := (n, noun_kind);
            when pron =>
               get(pronoun_kind);
               p := (pron, pronoun_kind);
            when pack =>
               get(propack_kind);
               p := (pack, propack_kind);
            when adj =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => adj);
            when num =>
               get(numeral_value);
               p := (num, numeral_value);
            when adv =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => adv);
            when v =>
               get(verb_kind);
               p := (v, verb_kind);
            when vpar =>
               get(vpar_kind);
               p := (vpar, vpar_kind);
            when supine =>
               get(supine_kind);
               p := (supine, supine_kind);
            when prep =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => prep);
            when conj =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => conj);
            when interj =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => interj);
            when tackon =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => tackon);
            when prefix =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => prefix);
            when suffix =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => suffix);
            when x =>
               set_col(col + positive_count(kind_entry_io.default_width));
               p := (pofs => x);
         end case;
         return;
      end get;

      pragma Warnings (Off, "formal parameter ""ps"" is not referenced");
      procedure put(f : in file_type;
                    ps : in part_of_speech_type; p : in kind_entry) is
         pragma Warnings (On, "formal parameter ""ps"" is not referenced");   
         c : constant positive := positive(col(f));
      begin
         case p.pofs is
            when n =>
               put(f, p.n_kind);
            when pron =>
               put(f, p.pron_kind);
            when pack =>
               put(f, p.pack_kind);
            when num =>
               put(f, p.num_value, numeral_value_type_io_default_width);
            when v =>
               put(f, p.v_kind);
            when vpar =>
               put(f, p.vpar_kind);
            when supine =>
               put(f, p.supine_kind);
            when others =>
               null;
         end case;
         put(f, string'((integer(col(f))..kind_entry_io.default_width+c-1 => ' ')));
         return;
      end put;

      pragma Warnings (Off, "formal parameter ""ps"" is not referenced");
      procedure put(ps : in part_of_speech_type; p : in kind_entry) is
         pragma Warnings (On, "formal parameter ""ps"" is not referenced");   
         c : constant positive := positive(col);
      begin
         case p.pofs is
            when n =>
               put(p.n_kind);
            when pron =>
               put(p.pron_kind);
            when pack =>
               put(p.pack_kind);
            when num =>
               put(p.num_value, numeral_value_type_io_default_width);
            when v =>
               put(p.v_kind);
            when vpar =>
               put(p.vpar_kind);
            when supine =>
               put(p.supine_kind);
            when others =>
               null;
         end case;
         put(string'((integer(col)..kind_entry_io.default_width+c-1 => ' ')));
         return;
      end put;

      procedure get(s : in string; ps : in part_of_speech_type;
                                   p : out kind_entry; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         last := l;         --  In case it is not set later
         case ps is
            when n =>
               get(s(l+1..s'last), noun_kind, last);
               p := (n, noun_kind);
            when pron =>
               get(s(l+1..s'last), pronoun_kind, last);
               p := (pron, pronoun_kind);
            when pack =>
               get(s(l+1..s'last), propack_kind, last);
               p := (pack, propack_kind);
            when adj =>
               p := (pofs => adj);
            when num =>
               get(s(l+1..s'last), numeral_value, last);
               p := (num, numeral_value);
            when adv =>
               p := (pofs => adv);
            when v =>
               get(s(l+1..s'last), verb_kind, last);
               p := (v, verb_kind);
            when vpar =>
               get(s(l+1..s'last), vpar_kind, last);
               p := (vpar, vpar_kind);
            when supine =>
               get(s(l+1..s'last), supine_kind, last);
               p := (supine, supine_kind);
            when prep =>
               p := (pofs => prep);
            when conj =>
               p := (pofs => conj);
            when interj =>
               p := (pofs => interj);
            when tackon =>
               p := (pofs => tackon);
            when prefix =>
               p := (pofs => prefix);
            when suffix =>
               p := (pofs => suffix);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end get;

      pragma Warnings (Off, "formal parameter ""ps"" is not referenced");
      procedure put(s : out string;
                    ps : in part_of_speech_type; p : in kind_entry) is
         pragma Warnings (On, "formal parameter ""ps"" is not referenced");   
         l : constant integer := s'first - 1;
         m : integer := 0;
      begin
         case p.pofs is
            when n =>
               m := l + noun_kind_type_io.default_width;
               put(s(l+1..m), p.n_kind);
            when pron =>
               m := l + pronoun_kind_type_io.default_width;
               put(s(l+1..m), p.pron_kind);
            when pack =>
               m := l + pronoun_kind_type_io.default_width;
               put(s(l+1..m), p.pack_kind);
            when num =>
               m := l + numeral_value_type_io_default_width;
               put(s(l+1..m), p.num_value);
            when v =>
               m := l + verb_kind_type_io.default_width;
               put(s(l+1..m), p.v_kind);
            when vpar =>
               m := l + verb_kind_type_io.default_width;
               put(s(l+1..m), p.vpar_kind);
            when supine =>
               m := l + verb_kind_type_io.default_width;
               put(s(l+1..m), p.supine_kind);
            when others =>
               null;
         end case;
         s(m+1..s'last) := (others => ' ');
      end put;

   end kind_entry_io;

   package body translation_record_io is
      use age_type_io;
      use area_type_io;
      use geo_type_io;
      use frequency_type_io;
      use source_type_io;
      spacer : character := ' ';
      --LINE : STRING(1..250);

      procedure get(f : in text_io.file_type; tr: out translation_record) is
      begin
         get(f, tr.age);
         get(f, spacer);
         get(f, tr.area);
         get(f, spacer);
         get(f, tr.geo);
         get(f, spacer);
         get(f, tr.freq);
         get(f, spacer);
         get(f, tr.source);
         --GET(F, SPACER);
         --GET_LINE(F, LINE, LAST);
         --TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
      end get;

      procedure get(tr : out translation_record) is
      begin
         get(tr.age);
         get(spacer);
         get(tr.area);
         get(spacer);
         get(tr.geo);
         get(spacer);
         get(tr.freq);
         get(spacer);
         get(tr.source);
         --GET(SPACER);
         --GET_LINE(LINE, LAST);
         --TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
      end get;

      procedure put(f : in text_io.file_type; tr : in translation_record) is
      begin
         put(f, tr.age);
         put(f, ' ');
         put(f, tr.area);
         put(f, ' ');
         put(f, tr.geo);
         put(f, ' ');
         put(f, tr.freq);
         put(f, ' ');
         put(f, tr.source);
         --PUT(F, ' ');
         --PUT(F, TR.MEAN);
      end put;

      procedure put(tr : in translation_record) is
      begin
         age_type_io.put(tr.age);
         text_io.put(' ');
         area_type_io.put(tr.area);
         text_io.put(' ');
         geo_type_io.put(tr.geo);
         text_io.put(' ');
         frequency_type_io.put(tr.freq);
         text_io.put(' ');
         source_type_io.put(tr.source);
         --TEXT_IO.PUT(' ');
         --TEXT_IO.PUT(TR.MEAN);
      end put;

      procedure get(s : in string; tr : out translation_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), tr.age, l);
         --PUT(TR.AGE); TEXT_IO.PUT('-');
         l := l + 1;
         get(s(l+1..s'last), tr.area, l);
         --PUT(TR.AREA); TEXT_IO.PUT('-');
         l := l + 1;
         get(s(l+1..s'last), tr.geo, l);
         --PUT(TR.GEO); TEXT_IO.PUT('-');
         l := l + 1;
         get(s(l+1..s'last), tr.freq, l);
         --PUT(TR.FREQ); TEXT_IO.PUT('-');
         l := l + 1;
         get(s(l+1..s'last), tr.source, last);
         --PUT(TR.SOURCE); TEXT_IO.PUT('-');
         --L := M + 1;
         --M := L + MAX_MEANING_SIZE;
         --TR.MEAN := HEAD(S(L+1..S'LAST), MAX_MEANING_SIZE);
         --LAST := M;
      end get;

      procedure put(s : out string; tr : in translation_record) is
         l : integer := 0;
         m : integer := 0;
      begin
         m := l + age_type_io.default_width;
         put(s(s'first + l .. m), tr.age);
         l := m + 1;
         s(l) :=  ' ';
         m := l + area_type_io.default_width;
         put(s(l+1..m), tr.area);
         l := m + 1;
         s(l) :=  ' ';
         m := l + geo_type_io.default_width;
         put(s(s'first + l .. m), tr.geo);
         l := m + 1;
         s(l) :=  ' ';
         m := l + frequency_type_io.default_width;
         put(s(l+1..m), tr.freq);
         l := m + 1;
         s(l) :=  ' ';
         m := l + source_type_io.default_width;
         put(s(l+1..m), tr.source);
         --L := M + 1;
         --S(L) :=  ' ';
         --M := L + MAX_MEANING_SIZE;
         --S(L+1..M) :=  TR.MEAN;
         s(m+1..s'last) := (others => ' ');
      end put;

   end translation_record_io;

   package body dictionary_entry_io is
      use part_entry_io;
      use translation_record_io;
      --use KIND_ENTRY_IO;

      spacer : character := ' ';
      part_col : natural := 0;

      procedure get(f : in file_type; d : out dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            get(f, d.stems(i));
            get(f, spacer);
         end loop;
         get(f, d.part);
         --    GET(F, SPACER);
         --    GET(F, D.PART.POFS, D.KIND);
         get(f, spacer);
         get(f, d.tran);
         get(f, spacer);
         get(f, d.mean);
      end get;

      procedure get(d : out dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            get(d.stems(i));
            get(spacer);
         end loop;
         get(d.part);
         --    GET(SPACER);
         --    GET(D.PART.POFS, D.KIND);
         get(spacer);
         get(d.tran);
         get(spacer);
         get(d.mean);
      end get;

      procedure put(f : in file_type; d : in dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            put(f, d.stems(i));
            put(f, ' ');
         end loop;
         part_col := natural(col(f));
         put(f, d.part);
         --    PUT(F, ' ');
         --    PUT(F, D.PART.POFS, D.KIND);
         set_col(f, count(part_col + part_entry_io.default_width + 1));
         put(f, d.tran);
         put(f, ' ');
         put(f, d.mean);
      end put;

      procedure put(d : in dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            put(d.stems(i));
            put(' ');
         end loop;
         part_col := natural(col);
         put(d.part);
         --    PUT(' ');
         --    PUT(D.PART.POFS, D.KIND);
         set_col(count(part_col + part_entry_io.default_width + 1));
         put(d.tran);
         put(' ');
         put(d.mean);
      end put;

      procedure get(s : in string; d : out dictionary_entry; last : out integer) is
         l : integer := s'first - 1;
         i : integer := 0;
      begin
         for i in stem_key_type range 1..4  loop
            stem_type_io.get(s(l+1..s'last), d.stems(i), l);
         end loop;
         get(s(l+1..s'last), d.part, l);
         --    L := L + 1;
         --    GET(S(L+1..S'LAST), D.PART.POFS, D.KIND, L);
         l := l + 1;
         get(s(l+1..s'last), d.tran, l);
         l := l + 1;
         d.mean := head(s(l+1..s'last), max_meaning_size);
         i := l+1;
         while s(i) = ' ' loop
            i := i + 1;
         end loop;
         while (s(i) not in 'A'..'Z') and
           (s(i) not in 'a'..'z')     loop
            last := i;
            i := i + 1;
            exit;
         end loop;
      end get;

      procedure put(s : out string; d : in dictionary_entry) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         for i in stem_key_type range 1..4  loop
            m := l + max_stem_size;
            s(l+1..m) := d.stems(i);
            l := m + 1;
            s(l) :=  ' ';
         end loop;
         part_col := l + 1;
         m := l + part_entry_io.default_width;
         put(s(l+1..m), d.part);
         --    L := M + 1;
         --    S(L) :=  ' ';
         --    M := L + KIND_ENTRY_IO_DEFAULT_WIDTH;
         --    PUT(S(L+1..M), D.PART.POFS, D.KIND);
         l := part_col + part_entry_io.default_width + 1;
         m := l + translation_record_io.default_width;
         put(s(l+1..m), d.tran);
         l := m + 1;
         s(l) :=  ' ';
         m := m + max_meaning_size;
         s(l+1..m) := d.mean;
         s(m+1..s'last) := (others => ' ');
      end put;

   end dictionary_entry_io;

   function "<=" (left, right : area_type) return boolean is
   begin
      if right = left  or else
        right = x  then
         return true;
      else
         return false;
      end if;
   end "<=";

begin     --  initialization of body of DICTIONARY_PACKAGE
          --TEXT_IO.PUT_LINE("Initializing DICTIONARY_PACKAGE");

   dictionary_kind_io.default_width := dictionary_kind'width;

   --NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH := 5;

   area_type_io.default_width := area_type'width;

   geo_type_io.default_width := geo_type'width;

   frequency_type_io.default_width := frequency_type'width;

   source_type_io.default_width := source_type'width;

   parse_record_io.default_width :=
     stem_type_io.default_width + 1 +
     inflection_record_io.default_width + 1 +
     dictionary_kind_io.default_width + 1 +
     mnpc_io_default_width;
   noun_entry_io.default_width :=
     decn_record_io.default_width + 1 +
     gender_type_io.default_width + 1 +
     noun_kind_type_io.default_width;
   pronoun_entry_io.default_width :=
     decn_record_io.default_width + 1 +
     pronoun_kind_type_io.default_width;
   propack_entry_io.default_width :=
     decn_record_io.default_width + 1 +
     pronoun_kind_type_io.default_width;
   adjective_entry_io.default_width :=
     decn_record_io.default_width + 1 +
     comparison_type_io.default_width;
   adverb_entry_io.default_width :=
     comparison_type_io.default_width;
   verb_entry_io.default_width :=
     decn_record_io.default_width + 1 +
     verb_kind_type_io.default_width;
   preposition_entry_io.default_width := 0;
   conjunction_entry_io.default_width := 0;

   interjection_entry_io.default_width := 0;
   numeral_entry_io.default_width :=
     decn_record_io.default_width + 1 +
     numeral_sort_type_io.default_width + 1 +
     numeral_value_type_io_default_width;

   part_entry_io.default_width := part_of_speech_type_io.default_width + 1 +
     numeral_entry_io.default_width;     --  Largest

   --  Should make up a MAX of PART_ENTRY + KIND_ENTRY (same POFS) WIDTHS

   translation_record_io.default_width :=
     age_type_io.default_width + 1 +
     area_type_io.default_width + 1 +
     geo_type_io.default_width + 1 +
     frequency_type_io.default_width + 1 +
     source_type_io.default_width;

   dictionary_entry_io.default_width := 4 * (max_stem_size + 1) +
     part_entry_io.default_width + 1 +
     translation_record_io.default_width + 1 +
     max_meaning_size;

   --TEXT_IO.PUT_LINE("Initialized  DICTIONARY_PACKAGE");

end dictionary_package;
