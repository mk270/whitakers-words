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
   use Stem_Key_Type_IO;

   MNPC_IO_Default_Width : constant Natural := 6;
   Numeral_Value_Type_IO_Default_Width : constant Natural := 5;
   --PART_WIDTH : NATURAL;

   function Number_Of_Stems (Part : Part_Of_Speech_Type) return Stem_Key_Type is
   begin
      case Part is
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
         when x       => return 0;
         when tackon .. suffix => return 0;
      end case;
   end Number_Of_Stems;

   package body Parse_Record_IO is separate;

   package body Noun_Entry_IO is separate;

   package body Pronoun_Entry_IO is separate;

   package body Propack_Entry_IO is separate;

   package body Adjective_Entry_IO is separate;

   package body Numeral_Entry_IO is separate;

   package body Adverb_Entry_IO is separate;

   package body Verb_Entry_IO is separate;

   package body Preposition_Entry_IO is separate;

   package body Conjunction_Entry_IO is separate;

   package body Interjection_Entry_IO is separate;

   function "<" (left, right : Part_Entry) return Boolean is
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
               if left.num.Decl < right.num.Decl  or else
                 (left.num.Decl = right.num.Decl  and then
                 left.num.Sort < right.num.Sort)  or else
                 ((left.num.Decl = right.num.Decl)  and then
                 (left.num.Sort = right.num.Sort)   and then
                 left.num.Value < right.num.Value)
               then
                  return True;
               end if;
            when adv =>
                  return left.adv.Co < right.adv.Co;
            when v =>
               if (left.v.Con < right.v.Con)  or else
                 (left.v.Con = right.v.Con  and then
                 left.v.Kind < right.v.Kind)
               then
                  return True;
               end if;
            when prep =>
               return left.prep.Obj < right.prep.Obj;
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

   package body Part_Entry_IO is separate;

   package body Kind_Entry_IO is separate;

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
      use Part_Entry_IO;
      use translation_record_io;
      --use KIND_ENTRY_IO;

      spacer : Character := ' ';
      part_col : Natural := 0;

      procedure Get(f : in File_Type; d : out dictionary_entry) is
      begin
         for i in Stem_Key_Type range 1..4  loop
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
         for i in Stem_Key_Type range 1..4  loop
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
         for i in Stem_Key_Type range 1..4  loop
            Put(f, d.stems(i));
            Put(f, ' ');
         end loop;
         part_col := Natural(Col(f));
         Put(f, d.part);
         --    PUT(F, ' ');
         --    PUT(F, D.PART.POFS, D.KIND);
         Set_Col(f, Count(part_col + Part_Entry_IO.Default_Width + 1));
         Put(f, d.tran);
         Put(f, ' ');
         Put(f, d.mean);
      end Put;

      procedure Put(d : in dictionary_entry) is
      begin
         for i in Stem_Key_Type range 1..4  loop
            Put(d.stems(i));
            Put(' ');
         end loop;
         part_col := Natural(Col);
         Put(d.part);
         --    PUT(' ');
         --    PUT(D.PART.POFS, D.KIND);
         Set_Col(Count(part_col + Part_Entry_IO.Default_Width + 1));
         Put(d.tran);
         Put(' ');
         Put(d.mean);
      end Put;

      procedure Get(s : in String; d : out dictionary_entry; last : out Integer) is
         l : Integer := s'First - 1;
         i : Integer := 0;
      begin
         for i in Stem_Key_Type range 1..4  loop
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
         for i in Stem_Key_Type range 1..4  loop
            m := l + Max_Stem_Size;
            s(l+1..m) := d.stems(i);
            l := m + 1;
            s(l) :=  ' ';
         end loop;
         part_col := l + 1;
         m := l + Part_Entry_IO.Default_Width;
         Put(s(l+1..m), d.part);
         --    L := M + 1;
         --    S(L) :=  ' ';
         --    M := L + KIND_ENTRY_IO_DEFAULT_WIDTH;
         --    PUT(S(L+1..M), D.PART.POFS, D.KIND);
         l := part_col + Part_Entry_IO.Default_Width + 1;
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
   Adverb_Entry_IO.Default_Width :=
     Comparison_Type_IO.Default_Width;
   Verb_Entry_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Verb_Kind_Type_IO.Default_Width;
   Preposition_Entry_IO.Default_Width := 0;
   Conjunction_Entry_IO.Default_Width := 0;

   Interjection_Entry_IO.Default_Width := 0;
   Numeral_Entry_IO.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Numeral_Sort_Type_IO.Default_Width + 1 +
     Numeral_Value_Type_IO_Default_Width;

   Part_Entry_IO.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Numeral_Entry_IO.Default_Width;     --  Largest

   --  Should make up a MAX of PART_ENTRY + KIND_ENTRY (same POFS) WIDTHS

   translation_record_io.Default_Width :=
     age_type_io.Default_Width + 1 +
     area_type_io.Default_Width + 1 +
     geo_type_io.Default_Width + 1 +
     frequency_type_io.Default_Width + 1 +
     source_type_io.Default_Width;

   dictionary_entry_io.Default_Width := 4 * (Max_Stem_Size + 1) +
     Part_Entry_IO.Default_Width + 1 +
     translation_record_io.Default_Width + 1 +
     Max_Meaning_Size;

   --TEXT_IO.PUT_LINE("Initialized  DICTIONARY_PACKAGE");
end Dictionary_Package;
