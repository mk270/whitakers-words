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
package body Dictionary_Package is
   use stem_key_type_io;

   MNPC_IO_Default_Width : constant Natural := 6;
   numeral_value_type_io_Default_Width : constant Natural := 5;
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

   package body Parse_Record_IO is separate;

   package body Noun_Entry_IO is separate;

   package body Pronoun_Entry_IO is separate;

   package body Propack_Entry_IO is separate;

   package body Adjective_Entry_IO is separate;

   package body numeral_entry_io is
      use Decn_Record_IO;
      use numeral_sort_type_io;
      use inflections_package.Integer_IO;
      spacer : Character := ' ';

      num_out_size : constant := 5;    --  Set in spec  !!!!!!!!!!!!!!!!!!!!!!!!!

      procedure Get(f : in File_Type; num : out numeral_entry) is
      begin
         Get(f, num.decl);
         Get(f, spacer);
         Get(f, num.sort);
         Get(f, spacer);
         Get(f, num.value);
      end Get;

      procedure Get(num : out numeral_entry) is
      begin
         Get(num.decl);
         Get(spacer);
         Get(num.sort);
         Get(spacer);
         Get(num.value);
      end Get;

      procedure Put(f : in File_Type; num : in numeral_entry) is
      begin
         Put(f, num.decl);
         Put(f, ' ');
         Put(f, num.sort);
         Put(f, ' ');
         Put(f, num.value, num_out_size);
      end Put;

      procedure Put(num : in numeral_entry) is
      begin
         Put(num.decl);
         Put(' ');
         Put(num.sort);
         Put(' ');
         Put(num.value, num_out_size);
      end Put;

      procedure Get(s : in String; num : out numeral_entry; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         --TEXT_IO.PUT("+1");
         Get(s(l+1..s'Last), num.decl, l);
         --TEXT_IO.PUT("+2");
         l := l + 1;
         Get(s(l+1..s'Last), num.sort, l);
         --TEXT_IO.PUT("+3");
         l := l + 1;
         Get(s(l+1..s'Last), num.value, last);
         --TEXT_IO.PUT("+4");
      end Get;

      procedure Put(s : out String; num : in numeral_entry) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), num.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + numeral_sort_type_io.Default_Width;
         Put(s(l+1..m), num.sort);
         l := m + 1;
         s(l) :=  ' ';
         --M := L + NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH;
         m := l + num_out_size;
         Put(s(l+1..m), num.value);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end numeral_entry_io;

   package body adverb_entry_io is
      use Comparison_Type_IO;

      procedure Get(f : in File_Type; a : out adverb_entry) is
      begin
         Get(f, a.co);
      end Get;

      procedure Get(a : out adverb_entry) is
      begin
         Get(a.co);
      end Get;

      procedure Put(f : in File_Type; a : in adverb_entry) is
      begin
         Put(f, a.co);
      end Put;

      procedure Put(a : in adverb_entry) is
      begin
         Put(a.co);
      end Put;

      procedure Get(s : in String; a : out adverb_entry; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), a.co, last);
      end Get;

      procedure Put(s : out String; a : in adverb_entry) is
         l : constant Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Comparison_Type_IO.Default_Width;
         Put(s(l+1..m), a.co);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end adverb_entry_io;

   package body verb_entry_io is
      use Decn_Record_IO;
      use verb_kind_type_io;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; v : out verb_entry) is
      begin
         Get(f, v.con);
         Get(f, spacer);
         Get(f, v.kind);
      end Get;

      procedure Get(v : out verb_entry) is
      begin
         Get(v.con);
         Get(spacer);
         Get(v.kind);
      end Get;

      procedure Put(f : in File_Type; v : in verb_entry) is
      begin
         Put(f, v.con);
         Put(f, ' ');
         Put(f, v.kind);
      end Put;

      procedure Put(v : in verb_entry) is
      begin
         Put(v.con);
         Put(' ');
         Put(v.kind);
      end Put;

      procedure Get(s : in String; v : out verb_entry; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), v.con, l);
         l := l + 1;
         Get(s(l+1..s'Last), v.kind, last);
      end Get;

      procedure Put(s : out String; v : in verb_entry) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), v.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + verb_kind_type_io.Default_Width;
         Put(s(l+1..m), v.kind);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end verb_entry_io;

   package body preposition_entry_io is
      use case_type_io;

      procedure Get(f : in File_Type; p : out preposition_entry) is
      begin
         Get(f, p.obj);
      end Get;

      procedure Get(p : out preposition_entry) is
      begin
         Get(p.obj);
      end Get;

      procedure Put(f : in File_Type; p : in preposition_entry) is
      begin
         Put(f, p.obj);
      end Put;

      procedure Put(p : in preposition_entry) is
      begin
         Put(p.obj);
      end Put;

      procedure Get(s : in String; p : out preposition_entry; last : out Integer) is
      begin
         Get(s, p.obj, last);
      end Get;

      procedure Put(s : out String; p : in preposition_entry) is
         l : constant Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), p.obj);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end preposition_entry_io;

   package body conjunction_entry_io is
      null_conjunction_entry : conjunction_entry;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; c : out conjunction_entry) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         c := null_conjunction_entry;
      end Get;

      procedure Get(c : out conjunction_entry) is
      begin
         c := null_conjunction_entry;
      end Get;

      procedure Put(f : in File_Type; c : in conjunction_entry) is
      begin
         null;
      end Put;

      procedure Put(c : in conjunction_entry) is
      begin
         null;
      end Put;

      procedure Get(s : in String; c : out conjunction_entry; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         c := null_conjunction_entry;
         last := l;
      end Get;

      pragma Warnings (Off, "formal parameter ""c"" is not referenced");
      procedure Put(s : out String; c : in conjunction_entry) is
         pragma Warnings (On, "formal parameter ""c"" is not referenced");
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end conjunction_entry_io;

   package body interjection_entry_io is
      null_interjection_entry : interjection_entry;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; i : out interjection_entry) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         i := null_interjection_entry;
      end Get;

      procedure Get(i : out interjection_entry) is
      begin
         i := null_interjection_entry;
      end Get;

      procedure Put(f : in File_Type; i : in interjection_entry) is
      begin
         null;
      end Put;

      procedure Put(i : in interjection_entry) is
      begin
         null;
      end Put;

      procedure Get(s : in String; i : out interjection_entry; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         i := null_interjection_entry;
         last := l;
      end Get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure Put(s : out String; i : in interjection_entry) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end interjection_entry_io;

   function "<" (left, right : part_entry) return Boolean is
   begin
      if left.pofs = right.pofs  then
         case left.pofs is
            when n =>
               if left.n.Decl < right.n.Decl  or else
                 (left.n.Decl = right.n.Decl  and then
                 left.n.Gender < right.n.Gender)  or else
                 ((left.n.Decl = right.n.Decl  and
                 left.n.Gender = right.n.Gender)  and then
                 left.n.Kind < right.n.Kind)
               then
                  return True;
               end if;
            when pron =>
               if left.pron.Decl < right.pron.Decl  or else
                 (left.pron.Decl = right.pron.Decl  and then
                 left.pron.Kind < right.pron.Kind)
               then
                  return True;
               end if;
            when pack =>
               if left.pack.Decl < right.pack.Decl  or else
                 (left.pack.Decl = right.pack.Decl  and then
                 left.pack.Kind < right.pack.Kind)
               then
                  return True;
               end if;
            when adj =>
               if left.adj.Decl < right.adj.Decl   or else
                 (left.adj.Decl = right.adj.Decl  and then
                 left.adj.Co < right.adj.Co)
               then
                  return True;
               end if;
            when num =>
               if left.num.decl < right.num.decl  or else
                 (left.num.decl = right.num.decl  and then
                 left.num.sort < right.num.sort)  or else
                 ((left.num.decl = right.num.decl)  and then
                 (left.num.sort = right.num.sort)   and then
                 left.num.value < right.num.value)
               then
                  return True;
               end if;when adv =>
                  return left.adv.co < right.adv.co;
            when v =>
               if (left.v.con < right.v.con)  or else
                 (left.v.con = right.v.con  and then
                 left.v.kind < right.v.kind)
               then
                  return True;
               end if;
            when prep =>
               return left.prep.obj < right.prep.obj;
            when others =>
               null;
         end case;
      else
         return left.pofs < right.pofs;
      end if;
      return False;
   exception
      when Constraint_Error  =>
         return left.pofs < right.pofs;
   end "<";

   package body part_entry_io is
      use part_of_speech_type_io;
      use Noun_Entry_IO;
      use Pronoun_Entry_IO;
      use Propack_Entry_IO;
      use Adjective_Entry_IO;
      use numeral_entry_io;
      use adverb_entry_io;
      use verb_entry_io;
      use preposition_entry_io;
      use conjunction_entry_io;
      use interjection_entry_io;
      spacer : Character := ' ';

      noun : Noun_Entry;
      Pronoun : Pronoun_Entry;
      propack : Propack_Entry;
      adjective : Adjective_Entry;
      numeral : numeral_entry;
      adverb : adverb_entry;
      verb : verb_entry;
      preposition : preposition_entry;
      conjunction : conjunction_entry;
      interjection : interjection_entry;

      procedure Get(f : in File_Type; p : out part_entry) is
         ps : part_of_speech_type := x;
         c : constant Positive_Count := Col(f);
      begin
         Get(f, ps);
         Get(f, spacer);
         case ps is
            when n =>
               Get(f, noun);
               p := (n, noun);
            when pron =>
               Get(f, Pronoun);
               p := (pron, Pronoun);
            when pack =>
               Get(f, propack);
               p := (pack, propack);
            when adj =>
               Get(f, adjective);
               p := (adj, adjective);
            when num =>
               Get(f, numeral);
               p := (num, numeral);
            when adv =>
               Get(f, adverb);
               p := (adv, adverb);
            when v =>
               Get(f, verb);
               p := (v, verb);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               Get(f, preposition);
               p := (prep, preposition);
            when conj =>
               Get(f, conjunction);
               p := (conj, conjunction);
            when interj =>
               Get(f, interjection);
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
         Set_Col(f, Positive_Count(part_entry_io.Default_Width)+c);
         return;
      end Get;

      procedure Get(p : out part_entry) is
         ps : part_of_speech_type := x;
      begin
         Get(ps);
         Get(spacer);
         case ps is
            when n =>
               Get(noun);
               p := (n, noun);
            when pron =>
               Get (Pronoun);
               p := (pron, Pronoun);
            when pack =>
               Get(propack);
               p := (pack, propack);
            when adj =>
               Get(adjective);
               p := (adj, adjective);
            when num =>
               Get(numeral);
               p := (num, numeral);
            when adv =>
               Get(adverb);
               p := (adv, adverb);
            when v =>
               Get(verb);
               p := (v, verb);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               Get(preposition);
               p := (prep, preposition);
            when conj =>
               Get(conjunction);
               p := (conj, conjunction);
            when interj =>
               Get(interjection);
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
      end Get;

      procedure Put(f : in File_Type; p : in part_entry) is
      begin
         Put(f, p.pofs);
         Put(f, ' ');
         case p.pofs is
            when n =>
               Put(f, p.n);
            when pron =>
               Put(f, p.pron);
            when pack =>
               Put(f, p.pack);
            when adj =>
               Put(f, p.adj);
            when num =>
               Put(f, p.num);
            when adv =>
               Put(f, p.adv);
            when v =>
               Put(f, p.v);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               Put(f, p.prep);
            when conj =>
               Put(f, p.conj);
            when interj =>
               Put(f, p.interj);
            when others =>
               null;
         end case;
         --PUT(F, STRING'((INTEGER(COL(F))..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
         return;
      end Put;

      procedure Put(p : in part_entry) is
      begin
         Put(p.pofs);
         Put(' ');
         case p.pofs is
            when n =>
               Put(p.n);
            when pron =>
               Put(p.pron);
            when pack =>
               Put(p.pack);
            when adj =>
               Put(p.adj);
            when num =>
               Put(p.num);
            when adv =>
               Put(p.adv);
            when v =>
               Put(p.v);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               Put(p.prep);
            when conj =>
               Put(p.conj);
            when interj =>
               Put(p.interj);
            when others =>
               null;
         end case;
         --PUT(STRING'((INTEGER(COL)..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
         return;
      end Put;

      procedure Get(s : in String; p : out part_entry; last : out Integer) is
         l : Integer := s'First - 1;
         ps : part_of_speech_type := x;
      begin
         last := l;      --  In case it is not set later
         Get(s, ps, l);
         l := l + 1;
         case ps is
            when n =>
               Get(s(l+1..s'Last), noun, last);
               p := (n, noun);
            when pron =>
               Get(s(l+1..s'Last), Pronoun, last);
               p := (pron, Pronoun);
            when pack =>
               Get(s(l+1..s'Last), propack, last);
               p := (pack, propack);
            when adj =>
               Get(s(l+1..s'Last), adjective, last);
               p := (adj, adjective);
            when num =>
               Get(s(l+1..s'Last), numeral, last);
               p := (num, numeral);
            when adv =>
               Get(s(l+1..s'Last), adverb, last);
               p := (adv, adverb);
            when v =>
               Get(s(l+1..s'Last), verb, last);
               p := (v, verb);
            when vpar =>
               null;                --  No VAPR entry
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               Get(s(l+1..s'Last), preposition, last);
               p := (prep, preposition);
            when conj =>
               Get(s(l+1..s'Last), conjunction, last);
               p := (conj, conjunction);
            when interj =>
               Get(s(l+1..s'Last), interjection, last);
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
      end Get;

      procedure Put(s : out String; p : in part_entry) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + part_of_speech_type_io.Default_Width;
         Put(s(l+1..m), p.pofs);
         l := m + 1;
         s(l) :=  ' ';
         case p.pofs is
            when n =>
               m := l + Noun_Entry_IO.Default_Width;
               Put(s(l+1..m), p.n);
            when pron =>
               m := l + Pronoun_Entry_IO.Default_Width;
               Put(s(l+1..m), p.pron);
            when pack =>
               m := l + Propack_Entry_IO.Default_Width;
               Put(s(l+1..m), p.pack);
            when adj =>
               m := l + Adjective_Entry_IO.Default_Width;
               Put(s(l+1..m), p.adj);
            when num =>
               m := l + numeral_entry_io.Default_Width;
               Put(s(l+1..m), p.num);
            when adv =>
               m := l + adverb_entry_io.Default_Width;
               Put(s(l+1..m), p.adv);
            when v =>
               m := l + verb_entry_io.Default_Width;
               Put(s(l+1..m), p.v);
            when vpar =>
               null;                --  No VAPR entryR
            when supine =>
               null;                --  No SUPINE entry
            when prep =>
               m := l + preposition_entry_io.Default_Width;
               Put(s(l+1..m), p.prep);
            when conj =>
               m := l + conjunction_entry_io.Default_Width;
               Put(s(l+1..m), p.conj);
            when interj =>
               m := l + interjection_entry_io.Default_Width;
               Put(s(l+1..m), p.interj);
            when others =>
               null;
         end case;
         --S(M+1..S'LAST) := (others => ' ');
      end Put;

   end part_entry_io;

   package body kind_entry_io is
      use Noun_Kind_Type_IO;
      use Pronoun_Kind_Type_IO;
      use inflections_package.Integer_IO;
      use verb_kind_type_io;

      noun_kind  : Noun_Kind_Type;
      pronoun_kind : Pronoun_Kind_Type;
      propack_kind : Pronoun_Kind_Type;
      verb_kind : verb_kind_type;
      vpar_kind : verb_kind_type;
      supine_kind : verb_kind_type;
      numeral_value : numeral_value_type;

      procedure Get(f : in File_Type;
                    ps : in part_of_speech_type; p : out kind_entry) is
      begin
         case ps is
            when n =>
               Get(f, noun_kind);
               p := (n, noun_kind);
            when pron =>
               Get(f, pronoun_kind);
               p := (pron, pronoun_kind);
            when pack =>
               Get(f, propack_kind);
               p := (pack, propack_kind);
            when adj =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => adj);
            when num =>
               Get(f, numeral_value);
               p := (num, numeral_value);
            when adv =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => adv);
            when v =>
               Get(f, verb_kind);
               p := (v, verb_kind);
            when vpar =>
               Get(f, vpar_kind);
               p := (vpar, vpar_kind);
            when supine =>
               Get(f, supine_kind);
               p := (supine, supine_kind);
            when prep =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => prep);
            when conj =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => conj);
            when interj =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => interj);
            when tackon =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => tackon);
            when prefix =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => prefix);
            when suffix =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => suffix);
            when x =>
               Set_Col(f, Col(f) + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => x);
         end case;
         return;
      end Get;

      procedure Get(ps : in part_of_speech_type; p : out kind_entry) is
      begin
         case ps is
            when n =>
               Get(noun_kind);
               p := (n, noun_kind);
            when pron =>
               Get(pronoun_kind);
               p := (pron, pronoun_kind);
            when pack =>
               Get(propack_kind);
               p := (pack, propack_kind);
            when adj =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => adj);
            when num =>
               Get(numeral_value);
               p := (num, numeral_value);
            when adv =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => adv);
            when v =>
               Get(verb_kind);
               p := (v, verb_kind);
            when vpar =>
               Get(vpar_kind);
               p := (vpar, vpar_kind);
            when supine =>
               Get(supine_kind);
               p := (supine, supine_kind);
            when prep =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => prep);
            when conj =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => conj);
            when interj =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => interj);
            when tackon =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => tackon);
            when prefix =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => prefix);
            when suffix =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => suffix);
            when x =>
               Set_Col(Col + Positive_Count(kind_entry_io.Default_Width));
               p := (pofs => x);
         end case;
         return;
      end Get;

      pragma Warnings (Off, "formal parameter ""ps"" is not referenced");
      procedure Put(f : in File_Type;
                    ps : in part_of_speech_type; p : in kind_entry) is
         pragma Warnings (On, "formal parameter ""ps"" is not referenced");
         c : constant Positive := Positive(Col(f));
      begin
         case p.pofs is
            when n =>
               Put(f, p.n_kind);
            when pron =>
               Put(f, p.pron_kind);
            when pack =>
               Put(f, p.pack_kind);
            when num =>
               Put(f, p.num_value, numeral_value_type_io_Default_Width);
            when v =>
               Put(f, p.v_kind);
            when vpar =>
               Put(f, p.vpar_kind);
            when supine =>
               Put(f, p.supine_kind);
            when others =>
               null;
         end case;
         Put(f, String'((Integer(Col(f))..kind_entry_io.Default_Width+c-1 => ' ')));
         return;
      end Put;

      pragma Warnings (Off, "formal parameter ""ps"" is not referenced");
      procedure Put(ps : in part_of_speech_type; p : in kind_entry) is
         pragma Warnings (On, "formal parameter ""ps"" is not referenced");
         c : constant Positive := Positive(Col);
      begin
         case p.pofs is
            when n =>
               Put(p.n_kind);
            when pron =>
               Put(p.pron_kind);
            when pack =>
               Put(p.pack_kind);
            when num =>
               Put(p.num_value, numeral_value_type_io_Default_Width);
            when v =>
               Put(p.v_kind);
            when vpar =>
               Put(p.vpar_kind);
            when supine =>
               Put(p.supine_kind);
            when others =>
               null;
         end case;
         Put(String'((Integer(Col)..kind_entry_io.Default_Width+c-1 => ' ')));
         return;
      end Put;

      procedure Get(s : in String; ps : in part_of_speech_type;
                                   p : out kind_entry; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         last := l;         --  In case it is not set later
         case ps is
            when n =>
               Get(s(l+1..s'Last), noun_kind, last);
               p := (n, noun_kind);
            when pron =>
               Get(s(l+1..s'Last), pronoun_kind, last);
               p := (pron, pronoun_kind);
            when pack =>
               Get(s(l+1..s'Last), propack_kind, last);
               p := (pack, propack_kind);
            when adj =>
               p := (pofs => adj);
            when num =>
               Get(s(l+1..s'Last), numeral_value, last);
               p := (num, numeral_value);
            when adv =>
               p := (pofs => adv);
            when v =>
               Get(s(l+1..s'Last), verb_kind, last);
               p := (v, verb_kind);
            when vpar =>
               Get(s(l+1..s'Last), vpar_kind, last);
               p := (vpar, vpar_kind);
            when supine =>
               Get(s(l+1..s'Last), supine_kind, last);
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
      end Get;

      pragma Warnings (Off, "formal parameter ""ps"" is not referenced");
      procedure Put(s : out String;
                    ps : in part_of_speech_type; p : in kind_entry) is
         pragma Warnings (On, "formal parameter ""ps"" is not referenced");
         l : constant Integer := s'First - 1;
         m : Integer := 0;
      begin
         case p.pofs is
            when n =>
               m := l + Noun_Kind_Type_IO.Default_Width;
               Put(s(l+1..m), p.n_kind);
            when pron =>
               m := l + Pronoun_Kind_Type_IO.Default_Width;
               Put(s(l+1..m), p.pron_kind);
            when pack =>
               m := l + Pronoun_Kind_Type_IO.Default_Width;
               Put(s(l+1..m), p.pack_kind);
            when num =>
               m := l + numeral_value_type_io_Default_Width;
               Put(s(l+1..m), p.num_value);
            when v =>
               m := l + verb_kind_type_io.Default_Width;
               Put(s(l+1..m), p.v_kind);
            when vpar =>
               m := l + verb_kind_type_io.Default_Width;
               Put(s(l+1..m), p.vpar_kind);
            when supine =>
               m := l + verb_kind_type_io.Default_Width;
               Put(s(l+1..m), p.supine_kind);
            when others =>
               null;
         end case;
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end kind_entry_io;

   package body translation_record_io is
      use age_type_io;
      use area_type_io;
      use geo_type_io;
      use frequency_type_io;
      use source_type_io;
      spacer : Character := ' ';
      --LINE : STRING(1..250);

      procedure Get(f : in Text_IO.File_Type; tr: out translation_record) is
      begin
         Get(f, tr.age);
         Get(f, spacer);
         Get(f, tr.area);
         Get(f, spacer);
         Get(f, tr.geo);
         Get(f, spacer);
         Get(f, tr.freq);
         Get(f, spacer);
         Get(f, tr.source);
         --GET(F, SPACER);
         --GET_LINE(F, LINE, LAST);
         --TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
      end Get;

      procedure Get(tr : out translation_record) is
      begin
         Get(tr.age);
         Get(spacer);
         Get(tr.area);
         Get(spacer);
         Get(tr.geo);
         Get(spacer);
         Get(tr.freq);
         Get(spacer);
         Get(tr.source);
         --GET(SPACER);
         --GET_LINE(LINE, LAST);
         --TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
      end Get;

      procedure Put(f : in Text_IO.File_Type; tr : in translation_record) is
      begin
         Put(f, tr.age);
         Put(f, ' ');
         Put(f, tr.area);
         Put(f, ' ');
         Put(f, tr.geo);
         Put(f, ' ');
         Put(f, tr.freq);
         Put(f, ' ');
         Put(f, tr.source);
         --PUT(F, ' ');
         --PUT(F, TR.MEAN);
      end Put;

      procedure Put(tr : in translation_record) is
      begin
         age_type_io.Put(tr.age);
         Text_IO.Put(' ');
         area_type_io.Put(tr.area);
         Text_IO.Put(' ');
         geo_type_io.Put(tr.geo);
         Text_IO.Put(' ');
         frequency_type_io.Put(tr.freq);
         Text_IO.Put(' ');
         source_type_io.Put(tr.source);
         --TEXT_IO.PUT(' ');
         --TEXT_IO.PUT(TR.MEAN);
      end Put;

      procedure Get(s : in String; tr : out translation_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), tr.age, l);
         --PUT(TR.AGE); TEXT_IO.PUT('-');
         l := l + 1;
         Get(s(l+1..s'Last), tr.area, l);
         --PUT(TR.AREA); TEXT_IO.PUT('-');
         l := l + 1;
         Get(s(l+1..s'Last), tr.geo, l);
         --PUT(TR.GEO); TEXT_IO.PUT('-');
         l := l + 1;
         Get(s(l+1..s'Last), tr.freq, l);
         --PUT(TR.FREQ); TEXT_IO.PUT('-');
         l := l + 1;
         Get(s(l+1..s'Last), tr.source, last);
         --PUT(TR.SOURCE); TEXT_IO.PUT('-');
         --L := M + 1;
         --M := L + MAX_MEANING_SIZE;
         --TR.MEAN := HEAD(S(L+1..S'LAST), MAX_MEANING_SIZE);
         --LAST := M;
      end Get;

      procedure Put(s : out String; tr : in translation_record) is
         l : Integer := 0;
         m : Integer := 0;
      begin
         m := l + age_type_io.Default_Width;
         Put(s(s'First + l .. m), tr.age);
         l := m + 1;
         s(l) :=  ' ';
         m := l + area_type_io.Default_Width;
         Put(s(l+1..m), tr.area);
         l := m + 1;
         s(l) :=  ' ';
         m := l + geo_type_io.Default_Width;
         Put(s(s'First + l .. m), tr.geo);
         l := m + 1;
         s(l) :=  ' ';
         m := l + frequency_type_io.Default_Width;
         Put(s(l+1..m), tr.freq);
         l := m + 1;
         s(l) :=  ' ';
         m := l + source_type_io.Default_Width;
         Put(s(l+1..m), tr.source);
         --L := M + 1;
         --S(L) :=  ' ';
         --M := L + MAX_MEANING_SIZE;
         --S(L+1..M) :=  TR.MEAN;
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end translation_record_io;

   package body dictionary_entry_io is
      use part_entry_io;
      use translation_record_io;
      --use KIND_ENTRY_IO;

      spacer : Character := ' ';
      part_col : Natural := 0;

      procedure Get(f : in File_Type; d : out dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            Get(f, d.stems(i));
            Get(f, spacer);
         end loop;
         Get(f, d.part);
         --    GET(F, SPACER);
         --    GET(F, D.PART.POFS, D.KIND);
         Get(f, spacer);
         Get(f, d.tran);
         Get(f, spacer);
         Get(f, d.mean);
      end Get;

      procedure Get(d : out dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            Get(d.stems(i));
            Get(spacer);
         end loop;
         Get(d.part);
         --    GET(SPACER);
         --    GET(D.PART.POFS, D.KIND);
         Get(spacer);
         Get(d.tran);
         Get(spacer);
         Get(d.mean);
      end Get;

      procedure Put(f : in File_Type; d : in dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            Put(f, d.stems(i));
            Put(f, ' ');
         end loop;
         part_col := Natural(Col(f));
         Put(f, d.part);
         --    PUT(F, ' ');
         --    PUT(F, D.PART.POFS, D.KIND);
         Set_Col(f, Count(part_col + part_entry_io.Default_Width + 1));
         Put(f, d.tran);
         Put(f, ' ');
         Put(f, d.mean);
      end Put;

      procedure Put(d : in dictionary_entry) is
      begin
         for i in stem_key_type range 1..4  loop
            Put(d.stems(i));
            Put(' ');
         end loop;
         part_col := Natural(Col);
         Put(d.part);
         --    PUT(' ');
         --    PUT(D.PART.POFS, D.KIND);
         Set_Col(Count(part_col + part_entry_io.Default_Width + 1));
         Put(d.tran);
         Put(' ');
         Put(d.mean);
      end Put;

      procedure Get(s : in String; d : out dictionary_entry; last : out Integer) is
         l : Integer := s'First - 1;
         i : Integer := 0;
      begin
         for i in stem_key_type range 1..4  loop
            Stem_Type_IO.Get(s(l+1..s'Last), d.stems(i), l);
         end loop;
         Get(s(l+1..s'Last), d.part, l);
         --    L := L + 1;
         --    GET(S(L+1..S'LAST), D.PART.POFS, D.KIND, L);
         l := l + 1;
         Get(s(l+1..s'Last), d.tran, l);
         l := l + 1;
         d.mean := Head(s(l+1..s'Last), Max_Meaning_Size);
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
      end Get;

      procedure Put(s : out String; d : in dictionary_entry) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         for i in stem_key_type range 1..4  loop
            m := l + Max_Stem_Size;
            s(l+1..m) := d.stems(i);
            l := m + 1;
            s(l) :=  ' ';
         end loop;
         part_col := l + 1;
         m := l + part_entry_io.Default_Width;
         Put(s(l+1..m), d.part);
         --    L := M + 1;
         --    S(L) :=  ' ';
         --    M := L + KIND_ENTRY_IO_DEFAULT_WIDTH;
         --    PUT(S(L+1..M), D.PART.POFS, D.KIND);
         l := part_col + part_entry_io.Default_Width + 1;
         m := l + translation_record_io.Default_Width;
         Put(s(l+1..m), d.tran);
         l := m + 1;
         s(l) :=  ' ';
         m := m + Max_Meaning_Size;
         s(l+1..m) := d.mean;
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end dictionary_entry_io;

   overriding function "<=" (left, right : area_type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

begin     --  initialization of body of DICTIONARY_PACKAGE
   --TEXT_IO.PUT_LINE("Initializing DICTIONARY_PACKAGE");

   Dictionary_Kind_IO.Default_Width := Dictionary_Kind'Width;

   --NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH := 5;

   area_type_io.Default_Width := area_type'Width;

   geo_type_io.Default_Width := geo_type'Width;

   frequency_type_io.Default_Width := frequency_type'Width;

   source_type_io.Default_Width := source_type'Width;

   Parse_Record_IO.Default_Width :=
     Stem_Type_IO.Default_Width + 1 +
     Inflection_Record_IO.Default_Width + 1 +
     Dictionary_Kind_IO.Default_Width + 1 +
     MNPC_IO_Default_Width;
   Noun_Entry_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Noun_Kind_Type_IO.Default_Width;
   Pronoun_Entry_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Pronoun_Kind_Type_IO.Default_Width;
   Propack_Entry_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Pronoun_Kind_Type_IO.Default_Width;
   Adjective_Entry_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Comparison_Type_IO.Default_Width;
   adverb_entry_io.Default_Width :=
     Comparison_Type_IO.Default_Width;
   verb_entry_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     verb_kind_type_io.Default_Width;
   preposition_entry_io.Default_Width := 0;
   conjunction_entry_io.Default_Width := 0;

   interjection_entry_io.Default_Width := 0;
   numeral_entry_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     numeral_sort_type_io.Default_Width + 1 +
     numeral_value_type_io_Default_Width;

   part_entry_io.Default_Width := part_of_speech_type_io.Default_Width + 1 +
     numeral_entry_io.Default_Width;     --  Largest

   --  Should make up a MAX of PART_ENTRY + KIND_ENTRY (same POFS) WIDTHS

   translation_record_io.Default_Width :=
     age_type_io.Default_Width + 1 +
     area_type_io.Default_Width + 1 +
     geo_type_io.Default_Width + 1 +
     frequency_type_io.Default_Width + 1 +
     source_type_io.Default_Width;

   dictionary_entry_io.Default_Width := 4 * (Max_Stem_Size + 1) +
     part_entry_io.Default_Width + 1 +
     translation_record_io.Default_Width + 1 +
     Max_Meaning_Size;

   --TEXT_IO.PUT_LINE("Initialized  DICTIONARY_PACKAGE");
end Dictionary_Package;
