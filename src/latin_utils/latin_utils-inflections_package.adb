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

with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Preface;
package body Latin_Utils.Inflections_Package is

   ---------------------------------------------------------------------------

   function "<" (Left, Right : Decn_Record) return Boolean is
   begin
      if Left.Which < Right.Which  or else
        (Left.Which = Right.Which  and then
           Left.Var < Right.Var)
      then
         return True;
      else
         return False;
      end if;
   end "<";

   ---------------------------------------------------------------------------

   function "<" (left, right : quality_record) return Boolean is
   begin
      if left.pofs = right.pofs  then
         case left.pofs is
            when N =>
               if left.N.decl.Which < right.N.decl.Which  or else
                 (left.N.decl.Which = right.N.decl.Which  and then
                    left.N.decl.Var < right.N.decl.Var)  or else
                 (left.N.decl.Which = right.N.decl.Which  and then
                    left.N.decl.Var = right.N.decl.Var  and then
                    left.N.number < right.N.number) or else
                 (left.N.decl.Which = right.N.decl.Which  and then
                    left.N.decl.Var = right.N.decl.Var  and then
                    left.N.number = right.N.number and then
                    left.N.cs < right.N.cs) or else
                 (left.N.decl.Which = right.N.decl.Which  and then
                    left.N.decl.Var = right.N.decl.Var  and then
                    left.N.number = right.N.number and then
                    left.N.cs = right.N.cs and then
                    left.N.gender < right.N.gender)
               then
                  return True;
               end if;
            when Pron =>
               if left.Pron.decl.Which < right.Pron.decl.Which  or else
                 (left.Pron.decl.Which = right.Pron.decl.Which  and then
                    left.Pron.decl.Var < right.Pron.decl.Var)  or else
                 (left.Pron.decl.Which = right.Pron.decl.Which  and then
                    left.Pron.decl.Var = right.Pron.decl.Var  and then
                    left.Pron.number < right.Pron.number) or else
                 (left.Pron.decl.Which = right.Pron.decl.Which  and then
                    left.Pron.decl.Var = right.Pron.decl.Var  and then
                    left.Pron.number = right.Pron.number and then
                    left.Pron.cs < right.Pron.cs) or else
                 (left.Pron.decl.Which = right.Pron.decl.Which  and then
                    left.Pron.decl.Var = right.Pron.decl.Var  and then
                    left.Pron.number = right.Pron.number and then
                    left.Pron.cs = right.Pron.cs and then
                    left.Pron.gender < right.Pron.gender)
               then
                  return True;
               end if;
            when Pack =>
               if left.Pack.decl.Which < right.Pack.decl.Which  or else
                 (left.Pack.decl.Which = right.Pack.decl.Which  and then
                    left.Pack.decl.Var < right.Pack.decl.Var)  or else
                 (left.Pack.decl.Which = right.Pack.decl.Which  and then
                    left.Pack.decl.Var = right.Pack.decl.Var  and then
                    left.Pack.number < right.Pack.number) or else
                 (left.Pack.decl.Which = right.Pack.decl.Which  and then
                    left.Pack.decl.Var = right.Pack.decl.Var  and then
                    left.Pack.number = right.Pack.number and then
                    left.Pack.cs < right.Pack.cs) or else
                 (left.Pack.decl.Which = right.Pack.decl.Which  and then
                    left.Pack.decl.Var = right.Pack.decl.Var  and then
                    left.Pack.number = right.Pack.number and then
                    left.Pack.cs = right.Pack.cs and then
                    left.Pack.gender < right.Pack.gender)
               then
                  return True;
               end if;
            when Adj =>
               if left.Adj.decl.Which < right.Adj.decl.Which  or else
                 (left.Adj.decl.Which = right.Adj.decl.Which  and then
                    left.Adj.decl.Var < right.Adj.decl.Var)  or else
                 (left.Adj.decl.Which = right.Adj.decl.Which  and then
                    left.Adj.decl.Var = right.Adj.decl.Var  and then
                    left.Adj.number < right.Adj.number) or else
                 (left.Adj.decl.Which = right.Adj.decl.Which  and then
                    left.Adj.decl.Var = right.Adj.decl.Var  and then
                    left.Adj.number = right.Adj.number and then
                    left.Adj.cs < right.Adj.cs) or else
                 (left.Adj.decl.Which = right.Adj.decl.Which  and then
                    left.Adj.decl.Var = right.Adj.decl.Var  and then
                    left.Adj.number = right.Adj.number and then
                    left.Adj.cs = right.Adj.cs and then
                    left.Adj.gender < right.Adj.gender)  or else
                 (left.Adj.decl.Which = right.Adj.decl.Which  and then
                    left.Adj.decl.Var = right.Adj.decl.Var  and then
                    left.Adj.number = right.Adj.number and then
                    left.Adj.cs = right.Adj.cs and then
                    left.Adj.gender = right.Adj.gender  and then
                    left.Adj.co < right.Adj.co)
               then
                  return True;
               end if;
            when Adv =>
               return left.Adv.co < right.Adv.co;
            when V =>
               if (left.V.con.Which < right.V.con.Which)  or else
                 (left.V.con.Which = right.V.con.Which  and then
                    left.V.con.Var < right.V.con.Var)  or else
                 (left.V.con.Which = right.V.con.Which  and then
                    left.V.con.Var = right.V.con.Var  and then
                    left.V.number < right.V.number) or else
                 (left.V.con.Which = right.V.con.Which  and then
                    left.V.con.Var = right.V.con.Var  and then
                    left.V.number = right.V.number and then
                    left.V.tense_voice_mood.tense < right.V.tense_voice_mood.tense) or else
                 (left.V.con.Which = right.V.con.Which  and then
                    left.V.con.Var = right.V.con.Var  and then
                    left.V.number = right.V.number and then
                    left.V.tense_voice_mood.tense = right.V.tense_voice_mood.tense and then
                    left.V.tense_voice_mood.voice < right.V.tense_voice_mood.voice) or else
                 (left.V.con.Which = right.V.con.Which  and then
                    left.V.con.Var = right.V.con.Var  and then
                    left.V.number = right.V.number and then
                    left.V.tense_voice_mood.tense = right.V.tense_voice_mood.tense and then
                    left.V.tense_voice_mood.voice = right.V.tense_voice_mood.voice and then
                    left.V.tense_voice_mood.mood   < right.V.tense_voice_mood.mood )  or else
                 (left.V.con.Which = right.V.con.Which  and then
                    left.V.con.Var = right.V.con.Var  and then
                    left.V.number = right.V.number and then
                    left.V.tense_voice_mood.tense = right.V.tense_voice_mood.tense and then
                    left.V.tense_voice_mood.voice = right.V.tense_voice_mood.voice and then
                    left.V.tense_voice_mood.mood   = right.V.tense_voice_mood.mood   and then
                    left.V.person < right.V.person)
               then
                  return True;
               end if;
            when Vpar =>
               if left.Vpar.con.Which < right.Vpar.con.Which  or else
                 (left.Vpar.con.Which = right.Vpar.con.Which  and then
                    left.Vpar.con.Var < right.Vpar.con.Var)  or else
                 (left.Vpar.con.Which = right.Vpar.con.Which  and then
                    left.Vpar.con.Var = right.Vpar.con.Var  and then
                    left.Vpar.number < right.Vpar.number) or else
                 (left.Vpar.con.Which = right.Vpar.con.Which  and then
                    left.Vpar.con.Var = right.Vpar.con.Var  and then
                    left.Vpar.number = right.Vpar.number and then
                    left.Vpar.cs < right.Vpar.cs) or else
                 (left.Vpar.con.Which = right.Vpar.con.Which  and then
                    left.Vpar.con.Var = right.Vpar.con.Var  and then
                    left.Vpar.number = right.Vpar.number and then
                    left.Vpar.cs = right.Vpar.cs and then
                    left.Vpar.gender < right.Vpar.gender)
               then
                  return True;
               end if;
            when Supine =>
               if left.Supine.con.Which < right.Supine.con.Which  or else
                 (left.Supine.con.Which = right.Supine.con.Which  and then
                    left.Supine.con.Var < right.Supine.con.Var)  or else
                 (left.Supine.con.Which = right.Supine.con.Which  and then
                    left.Supine.con.Var = right.Supine.con.Var  and then
                    left.Supine.number < right.Supine.number) or else
                 (left.Supine.con.Which = right.Supine.con.Which  and then
                    left.Supine.con.Var = right.Supine.con.Var  and then
                    left.Supine.number = right.Supine.number and then
                    left.Supine.cs < right.Supine.cs) or else
                 (left.Supine.con.Which = right.Supine.con.Which  and then
                    left.Supine.con.Var = right.Supine.con.Var  and then
                    left.Supine.number = right.Supine.number and then
                    left.Supine.cs = right.Supine.cs and then
                    left.Supine.gender < right.Supine.gender)
               then
                  return True;
               end if;
            when Prep =>
               return left.Prep.obj < right.Prep.obj;
            when Conj =>
               null;
            when Interj =>
               null;
            when Num =>
               if left.Num.decl.Which < right.Num.decl.Which  or else
                 (left.Num.decl.Which = right.Num.decl.Which  and then
                    left.Num.decl.Var < right.Num.decl.Var)  or else
                 (left.Num.decl.Which = right.Num.decl.Which  and then
                    left.Num.decl.Var = right.Num.decl.Var  and then
                    left.Num.number < right.Num.number) or else
                 (left.Num.decl.Which = right.Num.decl.Which  and then
                    left.Num.decl.Var = right.Num.decl.Var  and then
                    left.Num.number = right.Num.number and then
                    left.Num.cs < right.Num.cs) or else
                 (left.Num.decl.Which = right.Num.decl.Which  and then
                    left.Num.decl.Var = right.Num.decl.Var  and then
                    left.Num.number = right.Num.number and then
                    left.Num.cs = right.Num.cs and then
                    left.Num.gender < right.Num.gender)  or else
                 (left.Num.decl.Which = right.Num.decl.Which  and then
                    left.Num.decl.Var = right.Num.decl.Var  and then
                    left.Num.number = right.Num.number and then
                    left.Num.cs = right.Num.cs and then
                    left.Num.gender = right.Num.gender  and then
                    left.Num.sort < right.Num.sort)
               then
                  return True;
               end if;
            when Tackon .. Suffix =>
               null;
            when X =>
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

   overriding function "<=" (left, right : Part_Of_Speech_Type) return Boolean is
   begin
      if right = left  or else
        (left = Pack and right = Pron)  or else
         right = X
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   function "<=" (left, right : Decn_Record) return Boolean is
   begin
      if right = left  or else
        (right = Decn_Record'(0, 0)  and left.Which /= 9)  or else
        right = Decn_Record'(left.Which, 0)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Gender_Type) return Boolean is
   begin
      if right = left  or else
        right = X     or else
        (right = C  and then (left = M or left = F))
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Case_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Number_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Person_Type) return Boolean is
   begin
      if right = left or else right = 0 then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Comparison_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   function "<=" (left, right : tense_voice_mood_record)  return Boolean is
   begin
      if (right.tense = left.tense or else right.tense = x) and then
         (right.voice = left.voice or else right.voice = x) and then
         (right.mood = left.mood or else right.mood = x)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Noun_Kind_Type)   return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Pronoun_Kind_Type)   return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Stem_Key_Type)   return Boolean is
   begin            --  Only works for 2 stem parts, not verbs
      if right = left or else right = 0 then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Age_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Frequency_Type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   package body Stem_Type_IO is separate;

   package body Decn_Record_IO is separate;

   package body tense_voice_mood_record_io is
      use tense_type_io;
      use voice_type_io;
      use mood_type_io;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; t : out tense_voice_mood_record) is
      begin
         Get(f, t.tense);
         Get(f, spacer);
         Get(f, t.voice);
         Get(f, spacer);
         Get(f, t.mood);
      end Get;

      procedure Get(t : out tense_voice_mood_record) is
      begin
         Get(t.tense);
         Get(spacer);
         Get(t.voice);
         Get(spacer);
         Get(t.mood);
      end Get;

      procedure Put(f : in File_Type; t : in tense_voice_mood_record) is
      begin
         Put(f, t.tense);
         Put(f, ' ');
         Put(f, t.voice);
         Put(f, ' ');
         Put(f, t.mood);
      end Put;

      procedure Put(t : in tense_voice_mood_record) is
      begin
         Put(t.tense);
         Put(' ');
         Put(t.voice);
         Put(' ');
         Put(t.mood);
      end Put;

      procedure Get(s : in String;
                    t : out tense_voice_mood_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), t.tense, l);
         l := l + 1;
         Get(s(l+1..s'Last), t.voice, l);
         l := l + 1;
         Get(s(l+1..s'Last), t.mood, last);
      end Get;

      procedure Put(s : out String; t : in tense_voice_mood_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + tense_type_io.Default_Width;
         Put(s(l+1..m), t.tense);
         l := m + 1;
         s(l) :=  ' ';
         m := l + voice_type_io.Default_Width;
         Put(s(l+1..m), t.voice);
         l := m + 1;
         s(l) :=  ' ';
         m := l + mood_type_io.Default_Width;
         Put(s(l+1..m), t.mood);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end tense_voice_mood_record_io;

   package body noun_record_io is
      use Decn_Record_IO;
      use Case_Type_IO;
      use Gender_Type_IO;
      use Number_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; n : out noun_record) is
      begin
         Get(f, n.decl);
         Get(f, spacer);
         Get(f, n.cs);
         Get(f, spacer);
         Get(f, n.number);
         Get(f, spacer);
         Get(f, n.gender);
      end Get;

      procedure Get(n : out noun_record) is
      begin
         Get(n.decl);
         Get(spacer);
         Get(n.cs);
         Get(spacer);
         Get(n.number);
         Get(spacer);
         Get(n.gender);
      end Get;

      procedure Put(f : in File_Type; n : in noun_record) is
      begin
         Put(f, n.decl);
         Put(f, ' ');
         Put(f, n.cs);
         Put(f, ' ');
         Put(f, n.number);
         Put(f, ' ');
         Put(f, n.gender);
      end Put;

      procedure Put(n : in noun_record) is
      begin
         Put(n.decl);
         Put(' ');
         Put(n.cs);
         Put(' ');
         Put(n.number);
         Put(' ');
         Put(n.gender);
      end Put;

      procedure Get(s : in String; n : out noun_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), n.decl, l);
         l := l + 1;
         Get(s(l+1..s'Last), n.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), n.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), n.gender, last);
      end Get;

      procedure Put(s : out String; n : in noun_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), n.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), n.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), n.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), n.gender);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end noun_record_io;

   package body pronoun_record_io is
      use Decn_Record_IO;
      use Case_Type_IO;
      use Gender_Type_IO;
      use Number_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; p : out pronoun_record) is
      begin
         Get(f, p.decl);
         Get(f, spacer);
         Get(f, p.cs);
         Get(f, spacer);
         Get(f, p.number);
         Get(f, spacer);
         Get(f, p.gender);
      end Get;

      procedure Get(p : out pronoun_record) is
      begin
         Get(p.decl);
         Get(spacer);
         Get(p.cs);
         Get(spacer);
         Get(p.number);
         Get(spacer);
         Get(p.gender);
      end Get;

      procedure Put(f : in File_Type; p : in pronoun_record) is
      begin
         Put(f, p.decl);
         Put(f, ' ');
         Put(f, p.cs);
         Put(f, ' ');
         Put(f, p.number);
         Put(f, ' ');
         Put(f, p.gender);
      end Put;

      procedure Put(p : in pronoun_record) is
      begin
         Put(p.decl);
         Put(' ');
         Put(p.cs);
         Put(' ');
         Put(p.number);
         Put(' ');
         Put(p.gender);
      end Put;

      procedure Get(s : in String; p : out pronoun_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), p.decl, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.gender, last);
      end Get;

      procedure Put(s : out String; p : in pronoun_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), p.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), p.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), p.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), p.gender);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end pronoun_record_io;

   package body propack_record_io is
      use Decn_Record_IO;
      use Case_Type_IO;
      use Number_Type_IO;
      use Gender_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; p : out propack_record) is
      begin
         Get(f, p.decl);
         Get(f, spacer);
         Get(f, p.cs);
         Get(f, spacer);
         Get(f, p.number);
         Get(f, spacer);
         Get(f, p.gender);
      end Get;

      procedure Get(p : out propack_record) is
      begin
         Get(p.decl);
         Get(spacer);
         Get(p.cs);
         Get(spacer);
         Get(p.number);
         Get(spacer);
         Get(p.gender);
      end Get;

      procedure Put(f : in File_Type; p : in propack_record) is
      begin
         Put(f, p.decl);
         Put(f, ' ');
         Put(f, p.cs);
         Put(f, ' ');
         Put(f, p.number);
         Put(f, ' ');
         Put(f, p.gender);
      end Put;

      procedure Put(p : in propack_record) is
      begin
         Put(p.decl);
         Put(' ');
         Put(p.cs);
         Put(' ');
         Put(p.number);
         Put(' ');
         Put(p.gender);
      end Put;

      procedure Get(s : in String; p : out propack_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), p.decl, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.gender, last);
      end Get;

      procedure Put(s : out String; p : in propack_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), p.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), p.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), p.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), p.gender);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end propack_record_io;

   package body adjective_record_io is
      use Decn_Record_IO;
      use Gender_Type_IO;
      use Case_Type_IO;
      use Number_Type_IO;
      use Comparison_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; a : out adjective_record) is
      begin
         Get(f, a.decl);
         Get(f, spacer);
         Get(f, a.cs);
         Get(f, spacer);
         Get(f, a.number);
         Get(f, spacer);
         Get(f, a.gender);
         Get(f, spacer);
         Get(f, a.co);
      end Get;

      procedure Get(a : out adjective_record) is
      begin
         Get(a.decl);
         Get(spacer);
         Get(a.cs);
         Get(spacer);
         Get(a.number);
         Get(spacer);
         Get(a.gender);
         Get(spacer);
         Get(a.co);
      end Get;

      procedure Put(f : in File_Type; a : in adjective_record) is
      begin
         Put(f, a.decl);
         Put(f, ' ');
         Put(f, a.cs);
         Put(f, ' ');
         Put(f, a.number);
         Put(f, ' ');
         Put(f, a.gender);
         Put(f, ' ');
         Put(f, a.co);
      end Put;

      procedure Put(a : in adjective_record) is
      begin
         Put(a.decl);
         Put(' ');
         Put(a.cs);
         Put(' ');
         Put(a.number);
         Put(' ');
         Put(a.gender);
         Put(' ');
         Put(a.co);
      end Put;

      procedure Get(s : in String; a : out adjective_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), a.decl, l);
         l := l + 1;
         Get(s(l+1..s'Last), a.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), a.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), a.gender, l);
         l := l + 1;
         Get(s(l+1..s'Last), a.co, last);
      end Get;

      procedure Put(s : out String; a : in adjective_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), a.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), a.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), a.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), a.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Comparison_Type_IO.Default_Width;
         Put(s(l+1..m), a.co);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end adjective_record_io;

   package body numeral_record_io is
      use Decn_Record_IO;
      use Case_Type_IO;
      use Number_Type_IO;
      use Gender_Type_IO;
      use Numeral_Sort_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; num : out numeral_record) is
      begin
         Get(f, num.decl);
         Get(f, spacer);
         Get(f, num.cs);
         Get(f, spacer);
         Get(f, num.number);
         Get(f, spacer);
         Get(f, num.gender);
         Get(f, spacer);
         Get(f, num.sort);
      end Get;

      procedure Get(num : out numeral_record) is
      begin
         Get(num.decl);
         Get(spacer);
         Get(spacer);
         Get(num.number);
         Get(spacer);
         Get(num.gender);
         Get(spacer);
         Get(num.sort);
      end Get;

      procedure Put(f : in File_Type; num : in numeral_record) is
      begin
         Put(f, num.decl);
         Put(f, ' ');
         Put(f, num.cs);
         Put(f, ' ');
         Put(f, num.number);
         Put(f, ' ');
         Put(f, num.gender);
         Put(f, ' ');
         Put(f, num.sort);
      end Put;

      procedure Put(num : in numeral_record) is
      begin
         Put(num.decl);
         Put(' ');
         Put(num.cs);
         Put(' ');
         Put(num.number);
         Put(' ');
         Put(num.gender);
         Put(' ');
         Put(num.sort);
      end Put;

      procedure Get(s : in String; num : out numeral_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), num.decl, l);
         l := l + 1;
         Get(s(l+1..s'Last), num.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), num.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), num.gender, l);
         l := l + 1;
         Get(s(l+1..s'Last), num.sort, last);
      end Get;

      procedure Put(s : out String; num : in numeral_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), num.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), num.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), num.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), num.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Numeral_Sort_Type_IO.Default_Width;
         Put(s(l+1..m), num.sort);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end numeral_record_io;

   package body adverb_record_io is
      use Comparison_Type_IO;

      procedure Get(f : in File_Type; a : out adverb_record) is
      begin
         Get(f, a.co);
      end Get;

      procedure Get(a : out adverb_record) is
      begin
         Get(a.co);
      end Get;

      procedure Put(f : in File_Type; a : in adverb_record) is
      begin
         Put(f, a.co);
      end Put;

      procedure Put(a : in adverb_record) is
      begin
         Put(a.co);
      end Put;

      procedure Get(s : in String; a : out adverb_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), a.co, last);
      end Get;

      procedure Put(s : out String; a : in adverb_record) is
         l : constant Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Comparison_Type_IO.Default_Width;
         Put(s(l+1..m), a.co);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end adverb_record_io;

   package body verb_record_io is
      use Decn_Record_IO;
      use tense_voice_mood_record_io;
      use Person_Type_IO;
      use Number_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; v : out verb_record) is
      begin
         Get(f, v.con);
         Get(f, spacer);
         Get(f, v.tense_voice_mood);
         Get(f, spacer);
         Get(f, v.person);
         Get(f, spacer);
         Get(f, v.number);
      end Get;

      procedure Get(v : out verb_record) is
      begin
         Get(v.con);
         Get(spacer);
         Get(v.tense_voice_mood);
         Get(spacer);
         Get(v.person);
         Get(spacer);
         Get(v.number);
      end Get;

      procedure Put(f : in File_Type; v : in verb_record) is
      begin
         Put(f, v.con);
         Put(f, ' ');
         Put(f, v.tense_voice_mood);
         Put(f, ' ');
         Put(f, v.person);
         Put(f, ' ');
         Put(f, v.number);
      end Put;

      procedure Put(v : in verb_record) is
      begin
         Put(v.con);
         Put(' ');
         Put(v.tense_voice_mood);
         Put(' ');
         Put(v.person);
         Put(' ');
         Put(v.number);
      end Put;

      procedure Get(s : in String; v : out verb_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), v.con, l);
         l := l + 1;
         Get(s(l+1..s'Last), v.tense_voice_mood, l);
         l := l + 1;
         Get(s(l+1..s'Last), v.person, l);
         l := l + 1;
         Get(s(l+1..s'Last), v.number, last);
      end Get;

      procedure Put(s : out String; v : in verb_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), v.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + tense_voice_mood_record_io.Default_Width;
         Put(s(l+1..m), v.tense_voice_mood);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Person_Type_IO.Default_Width;
         Put(s(l+1..m), v.person);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), v.number);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end verb_record_io;

   package body vpar_record_io is
      use Decn_Record_IO;
      use Case_Type_IO;
      use Number_Type_IO;
      use Gender_Type_IO;
      use tense_voice_mood_record_io;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; vp : out vpar_record) is
      begin
         Get(f, vp.con);
         Get(f, spacer);
         Get(f, vp.cs);
         Get(f, spacer);
         Get(f, vp.number);
         Get(f, spacer);
         Get(f, vp.gender);
         Get(f, spacer);
         Get(f, vp.tense_voice_mood);
      end Get;

      procedure Get(vp : out vpar_record) is
      begin
         Get(vp.con);
         Get(spacer);
         Get(vp.cs);
         Get(spacer);
         Get(vp.number);
         Get(spacer);
         Get(vp.gender);
         Get(spacer);
         Get(vp.tense_voice_mood);
      end Get;

      procedure Put(f : in File_Type; vp : in vpar_record) is
      begin
         Put(f, vp.con);
         Put(f, ' ');
         Put(f, vp.cs);
         Put(f, ' ');
         Put(f, vp.number);
         Put(f, ' ');
         Put(f, vp.gender);
         Put(f, ' ');
         Put(f, vp.tense_voice_mood);
      end Put;

      procedure Put(vp : in vpar_record) is
      begin
         Put(vp.con);
         Put(' ');
         Put(vp.cs);
         Put(' ');
         Put(vp.number);
         Put(' ');
         Put(vp.gender);
         Put(' ');
         Put(vp.tense_voice_mood);
      end Put;

      procedure Get(s : in String; vp : out vpar_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), vp.con, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.gender, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.tense_voice_mood, last);
      end Get;

      procedure Put(s : out String; vp : in vpar_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), vp.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), vp.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), vp.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), vp.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + tense_voice_mood_record_io.Default_Width;
         Put(s(l+1..m), vp.tense_voice_mood);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end vpar_record_io;

   package body supine_record_io is
      use Decn_Record_IO;
      use Case_Type_IO;
      use Number_Type_IO;
      use Gender_Type_IO;
      spacer : Character := ' ';

      procedure Get(f : in File_Type; vp : out supine_record) is
      begin
         Get(f, vp.con);
         Get(f, spacer);
         Get(f, vp.cs);
         Get(f, spacer);
         Get(f, vp.number);
         Get(f, spacer);
         Get(f, vp.gender);
      end Get;

      procedure Get(vp : out supine_record) is
      begin
         Get(vp.con);
         Get(spacer);
         Get(vp.cs);
         Get(spacer);
         Get(vp.number);
         Get(spacer);
         Get(vp.gender);
      end Get;

      procedure Put(f : in File_Type; vp : in supine_record) is
      begin
         Put(f, vp.con);
         Put(f, ' ');
         Put(f, vp.cs);
         Put(f, ' ');
         Put(f, vp.number);
         Put(f, ' ');
         Put(f, vp.gender);
      end Put;

      procedure Put(vp : in supine_record) is
      begin
         Put(vp.con);
         Put(' ');
         Put(vp.cs);
         Put(' ');
         Put(vp.number);
         Put(' ');
         Put(vp.gender);
      end Put;

      procedure Get(s : in String; vp : out supine_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), vp.con, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.cs, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.number, l);
         l := l + 1;
         Get(s(l+1..s'Last), vp.gender, last);
      end Get;

      procedure Put(s : out String; vp : in supine_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Decn_Record_IO.Default_Width;
         Put(s(l+1..m), vp.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), vp.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Number_Type_IO.Default_Width;
         Put(s(l+1..m), vp.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), vp.gender);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end supine_record_io;

   package body preposition_record_io is
      use Case_Type_IO;

      procedure Get(f : in File_Type; p : out preposition_record) is
      begin
         Get(f, p.obj);
      end Get;

      procedure Get(p : out preposition_record) is
      begin
         Get(p.obj);
      end Get;

      procedure Put(f : in File_Type; p : in preposition_record) is
      begin
         Put(f, p.obj);
      end Put;

      procedure Put(p : in preposition_record) is
      begin
         Put(p.obj);
      end Put;

      procedure Get(s : in String; p : out preposition_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         Get(s(l+1..s'Last), p.obj, last);
      end Get;

      procedure Put(s : out String; p : in preposition_record) is
         l : constant Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Case_Type_IO.Default_Width;
         Put(s(l+1..m), p.obj);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end preposition_record_io;

   package body conjunction_record_io is
      null_conjunction_record : conjunction_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; c : out conjunction_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
         --  There is actually nothing to a CONJUNCTION_RECORD, no compoonents
      begin
         c := null_conjunction_record;
      end Get;

      procedure Get(c : out conjunction_record) is
      begin
         c := null_conjunction_record;
      end Get;

      procedure Put(f : in File_Type; c : in conjunction_record) is
      begin
         null;
      end Put;

      procedure Put(c : in conjunction_record) is
      begin
         null;
      end Put;

      procedure Get(s : in String; c : out conjunction_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         c := null_conjunction_record;
         last := l - 1;  --  LAST did not even Get to S'FIRST, since nothing to read
      end Get;

      pragma Warnings (Off, "formal parameter ""c"" is not referenced");
      procedure Put(s : out String; c : in conjunction_record) is
         pragma Warnings (On, "formal parameter ""c"" is not referenced");
         --  Since there is no component, just make the out String blank
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end conjunction_record_io;

   package body interjection_record_io is
      null_interjection_record : interjection_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; i : out interjection_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         i := null_interjection_record;
      end Get;

      procedure Get(i : out interjection_record) is
      begin
         i := null_interjection_record;
      end Get;

      procedure Put(f : in File_Type; i : in interjection_record) is
      begin
         null;
      end Put;

      procedure Put(i : in interjection_record) is
      begin
         null;
      end Put;

      procedure Get(s : in String; i : out interjection_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         i := null_interjection_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure Put(s : out String; i : in interjection_record) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end interjection_record_io;

   package body tackon_record_io is
      null_tackon_record : tackon_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; i : out tackon_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         i := null_tackon_record;
      end Get;

      procedure Get(i : out tackon_record) is
      begin
         i := null_tackon_record;
      end Get;

      procedure Put(f : in File_Type; i : in tackon_record) is
      begin
         null;
      end Put;

      procedure Put(i : in tackon_record) is
      begin
         null;
      end Put;

      procedure Get(s : in String; i : out tackon_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         i := null_tackon_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure Put(s : out String; i : in tackon_record) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end tackon_record_io;

   package body prefix_record_io is

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; p : out prefix_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         p := null_prefix_record;
      end Get;

      procedure Get(p : out prefix_record) is
      begin
         p := null_prefix_record;
      end Get;

      procedure Put(f : in File_Type; p : in prefix_record) is
      begin
         null;
      end Put;

      procedure Put(p : in prefix_record) is
      begin
         null;
      end Put;

      procedure Get(s : in String; p : out prefix_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         p := null_prefix_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""p"" is not referenced");
      procedure Put(s : out String; p : in prefix_record) is
         pragma Warnings (On, "formal parameter ""p"" is not referenced");
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end prefix_record_io;

   package body suffix_record_io is

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure Get(f : in File_Type; p : out suffix_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");
      begin
         p := null_suffix_record;
      end Get;

      procedure Get(p : out suffix_record) is
      begin
         p := null_suffix_record;
      end Get;

      procedure Put(f : in File_Type; p : in suffix_record) is
      begin
         null;
      end Put;

      procedure Put(p : in suffix_record) is
      begin
         null;
      end Put;

      procedure Get(s : in String; p : out suffix_record; last : out Integer) is
         l : constant Integer := s'First - 1;
      begin
         p := null_suffix_record;
         last := l - 1;
      end Get;

      pragma Warnings (Off, "formal parameter ""p"" is not referenced");
      procedure Put(s : out String; p : in suffix_record) is
         pragma Warnings (On, "formal parameter ""p"" is not referenced");
      begin
         s(s'First..s'Last) := (others => ' ');
      end Put;

   end suffix_record_io;

   package body quality_record_io is
      use Part_Of_Speech_Type_IO;
      use noun_record_io;
      use pronoun_record_io;
      use propack_record_io;
      use adjective_record_io;
      use numeral_record_io;
      use adverb_record_io;
      use verb_record_io;
      use vpar_record_io;
      use supine_record_io;
      use preposition_record_io;
      use conjunction_record_io;
      use interjection_record_io;
      use tackon_record_io;
      use prefix_record_io;
      use suffix_record_io;
      spacer : Character := ' ';

      noun  : noun_record;
      pronoun : pronoun_record;
      propack : propack_record;
      adjective : adjective_record;
      adverb : adverb_record;
      verb : verb_record;
      vparticiple : vpar_record;
      supin : supine_record;
      preposition : preposition_record;
      conjunction : conjunction_record;
      interjection : interjection_record;
      numeral : numeral_record;
      tackn : tackon_record;
      prefx : prefix_record;
      suffx : suffix_record;

      procedure Get(f : in File_Type; p : out quality_record) is
         ps : Part_Of_Speech_Type := X;
      begin
         Get(f, ps);
         Get(f, spacer);
         case ps is
            when N =>
               Get(f, noun);
               p := (N, noun);
            when Pron =>
               Get(f, pronoun);
               p := (Pron, pronoun);
            when Pack =>
               Get(f, propack);
               p := (Pack, propack);
            when Adj =>
               Get(f, adjective);
               p := (Adj, adjective);
            when Num =>
               Get(f, numeral);
               p := (Num, numeral);
            when Adv =>
               Get(f, adverb);
               p := (Adv, adverb);
            when V =>
               Get(f, verb);
               p := (V, verb);
            when Vpar =>
               Get(f, vparticiple);
               p := (Vpar, vparticiple);
            when Supine =>
               Get(f, supin);
               p := (Supine, supin);
            when Prep =>
               Get(f, preposition);
               p := (Prep, preposition);
            when Conj =>
               Get(f, conjunction);
               p := (Conj, conjunction);
            when Interj =>
               Get(f, interjection);
               p := (Interj, interjection);
            when Tackon =>
               Get(f, tackn);
               p := (Tackon, tackn);
            when Prefix =>
               Get(f, prefx);
               p := (Prefix, prefx);
            when Suffix =>
               Get(f, suffx);
               p := (Suffix, suffx);
            when X =>
               p := (pofs => X);
         end case;
         return;
      end Get;

      procedure Get(p : out quality_record) is
         ps : Part_Of_Speech_Type := X;
      begin
         Get (ps);
         Get (spacer);
         case ps is
            when N =>
               Get (noun);
               p := (N, noun);
            when Pron =>
               Get (pronoun);
               p := (Pron, pronoun);
            when Pack =>
               Get (propack);
               p := (Pack, propack);
            when Adj =>
               Get(adjective);
               p := (Adj, adjective);
            when Num =>
               Get(numeral);
               p := (Num, numeral);
            when Adv =>
               Get(adverb);
               p := (Adv, adverb);
            when V =>
               Get(verb);
               p := (V, verb);
            when Vpar =>
               Get(vparticiple);
               p := (Vpar, vparticiple);
            when Supine =>
               Get(supin);
               p := (Supine, supin);
            when Prep =>
               Get(preposition);
               p := (Prep, preposition);
            when Conj =>
               Get(conjunction);
               p := (Conj, conjunction);
            when Interj =>
               Get(interjection);
               p := (Interj, interjection);
            when Tackon =>
               Get(tackn);
               p := (Tackon, tackn);
            when Prefix =>
               Get(prefx);
               p := (Prefix, prefx);
            when Suffix =>
               Get(suffx);
               p := (Suffix, suffx);
            when X =>
               p := (pofs => X);
         end case;
         return;
      end Get;

      procedure Put(f : in File_Type; p : in quality_record) is
         c : constant Positive := Positive(Col(f));
      begin
         Put(f, p.pofs);
         Put(f, ' ');
         case p.pofs is
            when N =>
               Put(f, p.N);
            when Pron =>
               Put(f, p.Pron);
            when Pack =>
               Put(f, p.Pack);
            when Adj =>
               Put(f, p.Adj);
            when Num =>
               Put(f, p.Num);
            when Adv =>
               Put(f, p.Adv);
            when V =>
               Put(f, p.V);
            when Vpar =>
               Put(f, p.Vpar);
            when Supine =>
               Put(f, p.Supine);
            when Prep =>
               Put(f, p.Prep);
            when Conj =>
               Put(f, p.Conj);
            when Interj =>
               Put(f, p.Interj);
            when Tackon =>
               Put(f, p.Tackon);
            when Prefix =>
               Put(f, p.Prefix);
            when Suffix =>
               Put(f, p.Suffix);
            when others =>
               null;
         end case;
         Put(f, String'((Integer(Col(f))..quality_record_io.Default_Width+c-1 => ' ')));
         return;
      end Put;

      procedure Put(p : in quality_record) is
         c : constant Positive := Positive(Col);
      begin
         Put(p.pofs);
         Put(' ');
         case p.pofs is
            when N =>
               Put(p.N);
            when Pron =>
               Put(p.Pron);
            when Pack =>
               Put(p.Pack);
            when Adj =>
               Put(p.Adj);
            when Num =>
               Put(p.Num);
            when Adv =>
               Put(p.Adv);
            when V =>
               Put(p.V);
            when Vpar =>
               Put(p.Vpar);
            when Supine =>
               Put(p.Supine);
            when Prep =>
               Put(p.Prep);
            when Conj =>
               Put(p.Conj);
            when Interj =>
               Put(p.Interj);
            when Tackon =>
               Put(p.Tackon);
            when Prefix =>
               Put(p.Prefix);
            when Suffix =>
               Put(p.Suffix);
            when others =>
               null;
         end case;
         Put(String'((Integer(Col)..quality_record_io.Default_Width+c-1 => ' ')));
         return;
      end Put;

      procedure Get(s : in String; p : out quality_record; last : out Integer) is
         l : Integer := s'First - 1;
         ps : Part_Of_Speech_Type := X;
      begin
         Get(s, ps, l);
         last := l;         --  In case it is not set later
         l := l + 1;
         case ps is
            when N =>
               Get(s(l+1..s'Last), noun, last);
               p := (N, noun);
            when Pron =>
               Get(s(l+1..s'Last), pronoun, last);
               p := (Pron, pronoun);
            when Pack =>
               Get(s(l+1..s'Last), propack, last);
               p := (Pack, propack);
            when Adj =>
               Get(s(l+1..s'Last), adjective, last);
               p := (Adj, adjective);
            when Num =>
               Get(s(l+1..s'Last), numeral, last);
               p := (Num, numeral);
            when Adv =>
               Get(s(l+1..s'Last), adverb, last);
               p := (Adv, adverb);
            when V =>
               Get(s(l+1..s'Last), verb, last);
               p := (V, verb);
            when Vpar =>
               Get(s(l+1..s'Last), vparticiple, last);
               p := (Vpar, vparticiple);
            when Supine =>
               Get(s(l+1..s'Last), supin, last);
               p := (Supine, supin);
            when Prep =>
               Get(s(l+1..s'Last), preposition, last);
               p := (Prep, preposition);
            when Conj =>
               Get(s(l+1..s'Last), conjunction, last);
               p := (Conj, conjunction);
            when Interj =>
               Get(s(l+1..s'Last), interjection, last);
               p := (Interj, interjection);
            when Tackon =>
               Get(s(l+1..s'Last), tackn, last);
               p := (Tackon, tackn);
            when Prefix =>
               Get(s(l+1..s'Last), prefx, last);
               p := (Prefix, prefx);
            when Suffix =>
               Get(s(l+1..s'Last), suffx, last);
               p := (Suffix, suffx);
            when X =>
               p := (pofs => X);
         end case;
         return;
      end Get;

      procedure Put(s : out String; p : in quality_record) is
         --  Note that this does not Put with a uniform width
         --  which would require a constant QUALITY_RECORD_IO.DEFAULT_WIDTH
         --  Rather we Put to minimal size with NOUN_RECORD_IO.DEFAULT_WIDTH,
         --  PRONOUN_RECORD_IO,DEFAULT_WIDTH, ...
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + Part_Of_Speech_Type_IO.Default_Width;
         Put(s(l+1..m), p.pofs);
         l := m + 1;
         s(l) :=  ' ';
         case p.pofs is
            when N =>
               m := l + noun_record_io.Default_Width;
               Put(s(l+1..m), p.N);
            when Pron =>
               m := l + pronoun_record_io.Default_Width;
               Put(s(l+1..m), p.Pron);
            when Pack =>
               m := l + propack_record_io.Default_Width;
               Put(s(l+1..m), p.Pack);
            when Adj =>
               m := l + adjective_record_io.Default_Width;
               Put(s(l+1..m), p.Adj);
            when Num =>
               m := l + numeral_record_io.Default_Width;
               Put(s(l+1..m), p.Num);
            when Adv =>
               m := l + adverb_record_io.Default_Width;
               Put(s(l+1..m), p.Adv);
            when V =>
               m := l + verb_record_io.Default_Width;
               Put(s(l+1..m), p.V);
            when Vpar =>
               m := l + vpar_record_io.Default_Width;
               Put(s(l+1..m), p.Vpar);
            when Supine =>
               m := l + supine_record_io.Default_Width;
               Put(s(l+1..m), p.Supine);
            when Prep =>
               m := l + preposition_record_io.Default_Width;
               Put(s(l+1..m), p.Prep);
            when Conj =>
               m := l + conjunction_record_io.Default_Width;
               Put(s(l+1..m), p.Conj);
            when Interj =>
               m := l + interjection_record_io.Default_Width;
               Put(s(l+1..m), p.Interj);
            when Tackon =>
               m := l + tackon_record_io.Default_Width;
               Put(s(l+1..m), p.Tackon);
            when Prefix =>
               m := l + prefix_record_io.Default_Width;
               Put(s(l+1..m), p.Prefix);
            when Suffix =>
               m := l + suffix_record_io.Default_Width;
               Put(s(l+1..m), p.Suffix);
            when others =>
               null;
         end case;
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end quality_record_io;

   package body ending_record_io is
      use Integer_IO;
      spacer : Character := ' ';

      sf : ending := (others => ' ');
      blanks : constant ending := (others => ' ');
      n : ending_size_type := 0;

      procedure Get(f : in File_Type; x : out ending_record) is
      begin
         sf := blanks;
         Get(f, n);
         if n = 0  then
            x := null_ending_record;
         else
            Get(f, spacer);             --  Note this means exactly one blank
            Get(f, sf(1..n));
            x := (n, sf);
         end if;
      end Get;

      procedure Get(x : out ending_record) is
      begin
         sf := blanks;
         Get(n);
         if n = 0  then
            x := null_ending_record;
         else
            Get(spacer);
            Get(sf(1..n));
            x := (n, sf);
         end if;
      end Get;

      procedure Put(f : in File_Type; x : in ending_record) is
      begin
         Put(f, x.size, 1);
         Put(f, ' ');
         Put(f, x.suf(1..x.size) & blanks(x.size+1..max_ending_size));
      end Put;

      procedure Put(x : in ending_record) is
      begin
         Put(x.size, 1);
         Put(' ');
         Put(x.suf(1..x.size) & blanks(x.size+1..max_ending_size));
      end Put;

      procedure Get(s : in String; x : out ending_record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         sf := blanks;
         Get(s(l+1..s'Last), n, l);
         if n = 0  then
            x := null_ending_record;
            last := l;
         else
            l := l + 1;
            --if S(L+N-1) = ' '  or else
            --   S(L+N+1) /= ' '  then
            --if
            --   S(L+N+1) /= ' '  then
            -- TEXT_IO.PUT_LINE("ERROR in INFLECTION =>" & S);
            --else
            sf := s(l+1..l+n) & blanks(n+1..max_ending_size);
            last := l + n;
            x := (n, sf(1..n) & blanks(n+1..max_ending_size));
            --end if;
         end if;
      exception
         when others =>
            Ada.Text_IO.Put_Line("ENDING ERRROR " & s);
      end Get;

      procedure Put(s : out String; x : in ending_record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + 2;
         Put(s(l+1..m), x.size);
         m := m  + 1;
         s(m) := ' ';
         if x.size > 0  then
            l := m;
            m := l + x.size;
            s(l+1..m) := x.suf(1..x.size);
         end if;
         --  Being very careful here, first to fill out to the MAX_ENDING_SIZE
         l := m;
         m := l + max_ending_size - x.size;
         s(l+1..m) := (others => ' ');
         --  Then to fill out the rest of the out String, if any
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end ending_record_io;

   package body Inflection_Record_IO is
      use quality_record_io;
      use Stem_Key_Type_IO;
      use ending_record_io;
      use Age_Type_IO;
      use Frequency_Type_IO;
      spacer : Character := ' ';

      pe : Inflection_Record;

      procedure Get(f : in File_Type; p : out Inflection_Record) is
      begin
         Get(f, p.qual);
         Get(f, spacer);
         Get(f, p.key);
         Get(f, spacer);
         Get(f, p.ending);
         Get(f, spacer);
         Get(f, p.age);
         Get(f, spacer);
         Get(f, p.freq);
      end Get;

      procedure Get(p : out Inflection_Record) is
      begin
         Get(p.qual);
         Get(spacer);
         Get(p.key);
         Get(spacer);
         Get(p.ending);
         Get(spacer);
         Get(p.age);
         Get(spacer);
         Get(p.freq);
      end Get;

      procedure Put(f : in File_Type; p : in Inflection_Record) is
      begin
         Put(f, p.qual);
         Put(f, ' ');
         Put(f, p.key, 1);
         Put(f, ' ');
         Put(f, p.ending);
         Put(f, ' ');
         Put(f, p.age);
         Put(f, ' ');
         Put(f, p.freq);
      end Put;

      procedure Put(p : in Inflection_Record) is
      begin
         Put(p.qual);
         Put(' ');
         Put(p.key, 1);
         Put(' ');
         Put(p.ending);
         Put(' ');
         Put(p.age);
         Put(' ');
         Put(p.freq);
      end Put;

      procedure Get(s : in String; p : out Inflection_Record; last : out Integer) is
         l : Integer := s'First - 1;
      begin
         last := 0;
         p := pe;
         Get(s(l+1..s'Last), p.qual, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.key, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.ending, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.age, l);
         l := l + 1;
         Get(s(l+1..s'Last), p.freq, last);
      end Get;

      procedure Put(s : out String; p : in Inflection_Record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         m := l + quality_record_io.Default_Width;
         Put(s(l+1..m), p.qual);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 1;
         Put(s(l+1..m), p.key);
         l := m + 1;
         s(l) :=  ' ';
         m := l + ending_record_io.Default_Width;
         Put(s(l+1..m), p.ending);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 1;
         Put(s(l+1..m), p.age);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 1;
         Put(s(l+1..m), p.freq);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end Inflection_Record_IO;

   procedure establish_inflections_section  is
      --  Loads the inflection array from the file prepared in FILE_INFLECTIONS_SECTION
      --  If N = 0 (an artifical flag for the section for blank inflections = 5)
      --  comPutes the LELL..LELF indices for use in WORD
      use Inflection_Record_IO;
      use lel_section_io;

      procedure load_lel_indexes is
         --  Load arrays from file
         i  : Integer := 0;
         --IR : INFLECTION_RECORD;
         n, xn : Integer := 0;
         ch, xch : Character := ' ';
         inflections_sections_file : lel_section_io.File_Type;
      begin
         Open(inflections_sections_file, In_File, inflections_sections_name);
         number_of_inflections := 0;

         lel_section_io.Read(inflections_sections_file,
                             lel,
                             lel_section_io.Positive_Count(5));

         i := 1;
         belf(0, ' ') := i;
         bell(0, ' ') := 0;
         loop
            exit when lel(i) = Null_Inflection_Record;
            bel(i) := lel(i);

            bell(0, ' ') := i;
            i := i + 1;
         end loop;

         number_of_inflections := number_of_inflections + i - 1;

         lel_section_io.Read(inflections_sections_file,
                             lel,
                             lel_section_io.Positive_Count(1));

         i := 1;
         n := lel(i).ending.size;

         ch := lel(i).ending.suf(n);

         xn := n;
         xch := ch;
         lelf(n, ch) := i;

         c1_loop:
         loop
            n1_loop:
            loop
               exit c1_loop when lel(i) = Null_Inflection_Record;

               n := lel(i).ending.size;

               ch := lel(i).ending.suf(n);

               if ch /= xch  then
                  lell(xn, xch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xch := ch;
                  xn := n;
               elsif n /= xn  then
                  lell(xn, ch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xn := n;
                  exit n1_loop;
               end if;

               i := i + 1;

            end loop n1_loop;

         end loop c1_loop;

         lell(xn, xch) := i - 1;

         number_of_inflections := number_of_inflections + i - 1;

         lel_section_io.Read(inflections_sections_file,
                             lel,
                             lel_section_io.Positive_Count(2));

         i := 1;

         n := lel(i).ending.size;

         ch := lel(i).ending.suf(n);

         xn := n;
         xch := ch;
         lelf(n, ch) := i;

         c2_loop:
         loop
            n2_loop:
            loop
               exit c2_loop when lel(i) = Null_Inflection_Record;

               n := lel(i).ending.size;

               ch := lel(i).ending.suf(n);
               exit n2_loop when ch > 'r';

               if ch /= xch  then
                  lell(xn, xch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xch := ch;
                  xn := n;
               elsif n /= xn  then
                  lell(xn, ch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xn := n;
                  exit n2_loop;
               end if;

               i := i + 1;

            end loop n2_loop;

         end loop c2_loop;

         lell(xn, xch) := i - 1;

         number_of_inflections := number_of_inflections + i - 1;

         lel_section_io.Read(inflections_sections_file,
                             lel,
                             lel_section_io.Positive_Count(3));

         i := 1;

         n := lel(i).ending.size;

         ch := lel(i).ending.suf(n);

         xn := n;
         xch := ch;
         lelf(n, ch) := i;

         c3_loop:
         loop
            n3_loop:
            loop
               exit c3_loop when lel(i) = Null_Inflection_Record;

               n := lel(i).ending.size;

               ch := lel(i).ending.suf(n);
               exit n3_loop when ch > 's';

               if ch /= xch  then
                  lell(xn, xch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xch := ch;
                  xn := n;
               elsif n /= xn  then
                  lell(xn, ch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xn := n;
                  exit n3_loop;
               end if;

               i := i + 1;

            end loop n3_loop;

         end loop c3_loop;

         lell(xn, xch) := i - 1;

         number_of_inflections := number_of_inflections + i - 1;

         lel_section_io.Read(inflections_sections_file,
                             lel,
                             lel_section_io.Positive_Count(4));

         i := 1;

         n := lel(i).ending.size;

         ch := lel(i).ending.suf(n);

         xn := n;
         xch := ch;
         lelf(n, ch) := i;

         c4_loop:
         loop
            n4_loop:
            loop
               exit c4_loop when  lel(i).qual.pofs = Pron  and then
                 (lel(i).qual.Pron.decl.Which = 1  or
                    lel(i).qual.Pron.decl.Which = 2);

               n := lel(i).ending.size;

               ch := lel(i).ending.suf(n);

               if ch /= xch  then
                  lell(xn, xch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xch := ch;
                  xn := n;
               elsif n /= xn  then
                  lell(xn, ch) := i - 1;
                  lelf(n, ch) := i;
                  lell(n, ch) := 0;
                  xn := n;
                  exit n4_loop;
               end if;

               i := i + 1;

            end loop n4_loop;

         end loop c4_loop;

         lell(xn, xch) := i - 1;

         begin

            n := lel(i).ending.size;

            ch := lel(i).ending.suf(n);

            xn := n;
            xch := ch;
            pelf(n,  ch) := i;
            pell(n,  ch) := 0;

            c_p_loop:
            loop
               n_p_loop:
               loop
                  exit c_p_loop when lel(i) = Null_Inflection_Record;

                  n := lel(i).ending.size;

                  ch := lel(i).ending.suf(n);

                  if ch /= xch  then
                     pell(xn, xch) := i - 1;
                     pelf(n, ch) := i;
                     pell(n, ch) := 0;
                     xch := ch;
                     xn := n;
                  elsif n /= xn  then
                     pell(xn, ch) := i - 1;
                     pelf(n, ch) := i;
                     pell(n, ch) := 0;
                     xn  := n;
                     exit n_p_loop;
                  end if;

                  i := i + 1;
               end loop n_p_loop;
            end loop c_p_loop;
         exception
            when Constraint_Error => null;
         end;

         pell(xn, xch) := i - 1;
         number_of_inflections := number_of_inflections + i - 1;
         Close(inflections_sections_file);
      end load_lel_indexes;

   begin
      Preface.Put("INFLECTION_ARRAY being loaded");
      Preface.Set_Col(33);
      Preface.Put("--  ");
      load_lel_indexes;                    --  Makes indexes from array
      Preface.Put(number_of_inflections, 6);
      Preface.Put(" entries");
      Preface.Set_Col(55); Preface.Put_Line("--  Loaded correctly");
   exception
      when Ada.Text_IO.Name_Error  =>
         New_Line;
         Put_Line("There is no " & inflections_sections_name & " file.");
         Put_Line("The program cannot work without one.");
         Put_Line("Make sure you are in the subdirectory containing the files");
         Put_Line("for inflections, dictionary, addons and uniques.");
         raise give_up;
   end establish_inflections_section;

begin  --  initialization of body of INFLECTIONS_PACKAGE
   --TEXT_IO.PUT_LINE("Initializing INFLECTIONS_PACKAGE");

   Part_Of_Speech_Type_IO.Default_Width := Part_Of_Speech_Type'Width;
   Gender_Type_IO.Default_Width := Gender_Type'Width;
   Case_Type_IO.Default_Width := Case_Type'Width;
   Number_Type_IO.Default_Width := Number_Type'Width;
   Person_Type_IO.Default_Width := 1;
   Comparison_Type_IO.Default_Width := Comparison_Type'Width;
   tense_type_io.Default_Width := tense_type'Width;
   voice_type_io.Default_Width := voice_type'Width;
   mood_type_io.Default_Width := mood_type'Width;
   Noun_Kind_Type_IO.Default_Width := Noun_Kind_Type'Width;
   Pronoun_Kind_Type_IO.Default_Width := Pronoun_Kind_Type'Width;
   Verb_Kind_Type_IO.Default_Width := Verb_Kind_Type'Width;
   Numeral_Sort_Type_IO.Default_Width := Numeral_Sort_Type'Width;
   Age_Type_IO.Default_Width := Age_Type'Width;
   Frequency_Type_IO.Default_Width := Frequency_Type'Width;

   Decn_Record_IO.Default_Width :=
     1 + 1 +   --WHICH_TYPE_IO_DEFAULT_WIDTH + 1 +
     1;        --VARIANT_TYPE_IO_DEFAULT_WIDTH;
   tense_voice_mood_record_io.Default_Width :=
     tense_type_io.Default_Width + 1 +
     voice_type_io.Default_Width + 1 +
     mood_type_io.Default_Width;
   noun_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   pronoun_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   propack_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   adjective_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Comparison_Type_IO.Default_Width;
   adverb_record_io.Default_Width :=
     Comparison_Type_IO.Default_Width;
   verb_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     tense_voice_mood_record_io.Default_Width + 1 +
     Person_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width;
   vpar_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     tense_voice_mood_record_io.Default_Width;
   supine_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   preposition_record_io.Default_Width := Case_Type_IO.Default_Width;
   conjunction_record_io.Default_Width := 0;
   interjection_record_io.Default_Width := 0;
   numeral_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     Case_Type_IO.Default_Width + 1 +
     Number_Type_IO.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Numeral_Sort_Type_IO.Default_Width;
   tackon_record_io.Default_Width := 0;
   prefix_record_io.Default_Width := 0;
   suffix_record_io.Default_Width := 0;
   quality_record_io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     vpar_record_io.Default_Width; --  Largest
   ending_record_io.Default_Width := 3 + 1 +
     max_ending_size;
   Inflection_Record_IO.Default_Width := quality_record_io.Default_Width + 1 +
     1  + 1 +
     ending_record_io.Default_Width + 1 +
     Age_Type_IO.Default_Width + 1 +
     Frequency_Type_IO.Default_Width;

end Latin_Utils.Inflections_Package;
