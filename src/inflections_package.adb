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

with Latin_File_Names; use Latin_File_Names;
with preface;
package body Inflections_Package is

   function "<" (left, right : Decn_Record) return Boolean is
   begin
      if left.which < right.which  or else
        (left.which = right.which  and then
           left.var < right.var)
      then
         return True;
      else
         return False;
      end if;
   end "<";

   function "<" (left, right : quality_record) return Boolean is
   begin
      if left.pofs = right.pofs  then
         case left.pofs is
            when n =>
               if left.n.decl.which < right.n.decl.which  or else
                 (left.n.decl.which = right.n.decl.which  and then
                    left.n.decl.var < right.n.decl.var)  or else
                 (left.n.decl.which = right.n.decl.which  and then
                    left.n.decl.var = right.n.decl.var  and then
                    left.n.number < right.n.number) or else
                 (left.n.decl.which = right.n.decl.which  and then
                    left.n.decl.var = right.n.decl.var  and then
                    left.n.number = right.n.number and then
                    left.n.cs < right.n.cs) or else
                 (left.n.decl.which = right.n.decl.which  and then
                    left.n.decl.var = right.n.decl.var  and then
                    left.n.number = right.n.number and then
                    left.n.cs = right.n.cs and then
                    left.n.gender < right.n.gender)
               then
                  return True;
               end if;
            when pron =>
               if left.pron.decl.which < right.pron.decl.which  or else
                 (left.pron.decl.which = right.pron.decl.which  and then
                    left.pron.decl.var < right.pron.decl.var)  or else
                 (left.pron.decl.which = right.pron.decl.which  and then
                    left.pron.decl.var = right.pron.decl.var  and then
                    left.pron.number < right.pron.number) or else
                 (left.pron.decl.which = right.pron.decl.which  and then
                    left.pron.decl.var = right.pron.decl.var  and then
                    left.pron.number = right.pron.number and then
                    left.pron.cs < right.pron.cs) or else
                 (left.pron.decl.which = right.pron.decl.which  and then
                    left.pron.decl.var = right.pron.decl.var  and then
                    left.pron.number = right.pron.number and then
                    left.pron.cs = right.pron.cs and then
                    left.pron.gender < right.pron.gender)
               then
                  return True;
               end if;
            when pack =>
               if left.pack.decl.which < right.pack.decl.which  or else
                 (left.pack.decl.which = right.pack.decl.which  and then
                    left.pack.decl.var < right.pack.decl.var)  or else
                 (left.pack.decl.which = right.pack.decl.which  and then
                    left.pack.decl.var = right.pack.decl.var  and then
                    left.pack.number < right.pack.number) or else
                 (left.pack.decl.which = right.pack.decl.which  and then
                    left.pack.decl.var = right.pack.decl.var  and then
                    left.pack.number = right.pack.number and then
                    left.pack.cs < right.pack.cs) or else
                 (left.pack.decl.which = right.pack.decl.which  and then
                    left.pack.decl.var = right.pack.decl.var  and then
                    left.pack.number = right.pack.number and then
                    left.pack.cs = right.pack.cs and then
                    left.pack.gender < right.pack.gender)
               then
                  return True;
               end if;
            when adj =>
               if left.adj.decl.which < right.adj.decl.which  or else
                 (left.adj.decl.which = right.adj.decl.which  and then
                    left.adj.decl.var < right.adj.decl.var)  or else
                 (left.adj.decl.which = right.adj.decl.which  and then
                    left.adj.decl.var = right.adj.decl.var  and then
                    left.adj.number < right.adj.number) or else
                 (left.adj.decl.which = right.adj.decl.which  and then
                    left.adj.decl.var = right.adj.decl.var  and then
                    left.adj.number = right.adj.number and then
                    left.adj.cs < right.adj.cs) or else
                 (left.adj.decl.which = right.adj.decl.which  and then
                    left.adj.decl.var = right.adj.decl.var  and then
                    left.adj.number = right.adj.number and then
                    left.adj.cs = right.adj.cs and then
                    left.adj.gender < right.adj.gender)  or else
                 (left.adj.decl.which = right.adj.decl.which  and then
                    left.adj.decl.var = right.adj.decl.var  and then
                    left.adj.number = right.adj.number and then
                    left.adj.cs = right.adj.cs and then
                    left.adj.gender = right.adj.gender  and then
                    left.adj.co < right.adj.co)
               then
                  return True;
               end if;
            when adv =>
               return left.adv.co < right.adv.co;
            when v =>
               if (left.v.con.which < right.v.con.which)  or else
                 (left.v.con.which = right.v.con.which  and then
                    left.v.con.var < right.v.con.var)  or else
                 (left.v.con.which = right.v.con.which  and then
                    left.v.con.var = right.v.con.var  and then
                    left.v.number < right.v.number) or else
                 (left.v.con.which = right.v.con.which  and then
                    left.v.con.var = right.v.con.var  and then
                    left.v.number = right.v.number and then
                    left.v.tense_voice_mood.tense < right.v.tense_voice_mood.tense) or else
                 (left.v.con.which = right.v.con.which  and then
                    left.v.con.var = right.v.con.var  and then
                    left.v.number = right.v.number and then
                    left.v.tense_voice_mood.tense = right.v.tense_voice_mood.tense and then
                    left.v.tense_voice_mood.voice < right.v.tense_voice_mood.voice) or else
                 (left.v.con.which = right.v.con.which  and then
                    left.v.con.var = right.v.con.var  and then
                    left.v.number = right.v.number and then
                    left.v.tense_voice_mood.tense = right.v.tense_voice_mood.tense and then
                    left.v.tense_voice_mood.voice = right.v.tense_voice_mood.voice and then
                    left.v.tense_voice_mood.mood   < right.v.tense_voice_mood.mood )  or else
                 (left.v.con.which = right.v.con.which  and then
                    left.v.con.var = right.v.con.var  and then
                    left.v.number = right.v.number and then
                    left.v.tense_voice_mood.tense = right.v.tense_voice_mood.tense and then
                    left.v.tense_voice_mood.voice = right.v.tense_voice_mood.voice and then
                    left.v.tense_voice_mood.mood   = right.v.tense_voice_mood.mood   and then
                    left.v.person < right.v.person)
               then
                  return True;
               end if;
            when vpar =>
               if left.vpar.con.which < right.vpar.con.which  or else
                 (left.vpar.con.which = right.vpar.con.which  and then
                    left.vpar.con.var < right.vpar.con.var)  or else
                 (left.vpar.con.which = right.vpar.con.which  and then
                    left.vpar.con.var = right.vpar.con.var  and then
                    left.vpar.number < right.vpar.number) or else
                 (left.vpar.con.which = right.vpar.con.which  and then
                    left.vpar.con.var = right.vpar.con.var  and then
                    left.vpar.number = right.vpar.number and then
                    left.vpar.cs < right.vpar.cs) or else
                 (left.vpar.con.which = right.vpar.con.which  and then
                    left.vpar.con.var = right.vpar.con.var  and then
                    left.vpar.number = right.vpar.number and then
                    left.vpar.cs = right.vpar.cs and then
                    left.vpar.gender < right.vpar.gender)
               then
                  return True;
               end if;
            when supine =>
               if left.supine.con.which < right.supine.con.which  or else
                 (left.supine.con.which = right.supine.con.which  and then
                    left.supine.con.var < right.supine.con.var)  or else
                 (left.supine.con.which = right.supine.con.which  and then
                    left.supine.con.var = right.supine.con.var  and then
                    left.supine.number < right.supine.number) or else
                 (left.supine.con.which = right.supine.con.which  and then
                    left.supine.con.var = right.supine.con.var  and then
                    left.supine.number = right.supine.number and then
                    left.supine.cs < right.supine.cs) or else
                 (left.supine.con.which = right.supine.con.which  and then
                    left.supine.con.var = right.supine.con.var  and then
                    left.supine.number = right.supine.number and then
                    left.supine.cs = right.supine.cs and then
                    left.supine.gender < right.supine.gender)
               then
                  return True;
               end if;
            when prep =>
               return left.prep.obj < right.prep.obj;
            when conj =>
               null;
            when interj =>
               null;
            when num =>
               if left.num.decl.which < right.num.decl.which  or else
                 (left.num.decl.which = right.num.decl.which  and then
                    left.num.decl.var < right.num.decl.var)  or else
                 (left.num.decl.which = right.num.decl.which  and then
                    left.num.decl.var = right.num.decl.var  and then
                    left.num.number < right.num.number) or else
                 (left.num.decl.which = right.num.decl.which  and then
                    left.num.decl.var = right.num.decl.var  and then
                    left.num.number = right.num.number and then
                    left.num.cs < right.num.cs) or else
                 (left.num.decl.which = right.num.decl.which  and then
                    left.num.decl.var = right.num.decl.var  and then
                    left.num.number = right.num.number and then
                    left.num.cs = right.num.cs and then
                    left.num.gender < right.num.gender)  or else
                 (left.num.decl.which = right.num.decl.which  and then
                    left.num.decl.var = right.num.decl.var  and then
                    left.num.number = right.num.number and then
                    left.num.cs = right.num.cs and then
                    left.num.gender = right.num.gender  and then
                    left.num.sort < right.num.sort)
               then
                  return True;
               end if;
            when tackon =>
               null;
            when prefix =>
               null;
            when suffix =>
               null;
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

   overriding function "<=" (left, right : Part_Of_Speech_Type) return Boolean is
   begin
      if right = left  or else
        (left = pack and right = pron)  or else
         right = x
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   function "<=" (left, right : Decn_Record) return Boolean is
   begin
      if right = left  or else
        (right = Decn_Record'(0, 0)  and left.which /= 9)  or else
        right = Decn_Record'(left.which, 0)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : Gender_Type) return Boolean is
   begin
      if right = left  or else
        right = x     or else
        (right = c  and then (left = m or left = f))
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : case_type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : number_type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : person_type) return Boolean is
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

   overriding function "<=" (left, right : age_type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (left, right : frequency_type) return Boolean is
   begin
      if right = left or else right = x then
         return True;
      else
         return False;
      end if;
   end "<=";

   package body Stem_Type_IO is
      procedure Get(f : in File_Type; d : out Stem_Type) is
         c : Character := ' ';
      begin
         d := Null_Stem_Type;
         for i in 1..Stem_Type_IO.Default_Width  loop
            Get(f, c);
            if (c not in 'A'..'Z') and (c not in 'a'..'z')  then
               exit;
            else
               d(i) := c;
            end if;
         end loop;
      end Get;

      procedure Get(d : out Stem_Type) is
         c : Character := ' ';
      begin
         d := Null_Stem_Type;
         for i in 1..Stem_Type_IO.Default_Width loop
            Text_IO.Get(c);
            if (c not in 'A'..'Z') and (c not in 'a'..'z') then
               exit;
            else
               d(i) := c;
            end if;
         end loop;
      end Get;

      procedure Put(f : in File_Type; d : in Stem_Type) is
      begin
         Text_IO.Put(f, d);
      end Put;

      procedure Put(d : in Stem_Type) is
      begin
         Text_IO.Put(d);
      end Put;

      procedure Get(s : in String; d : out Stem_Type;
                                   last : out Integer) is
         c : Character;
      begin
         d := Null_Stem_Type;
         last := 0;
         for i in 1..Stem_Type_IO.Default_Width  loop
            c := s(i);
            if (c not in 'A'..'Z') and (c not in 'a'..'z')  then
               exit;
            else
               d(i) := c;
               last := i;
            end if;
         end loop;
      end Get;

      procedure Put(s : out String; d : in Stem_Type) is
      begin
         s(s'First..s'First+Stem_Type_IO.Default_Width-1) := d;
      end Put;

   end Stem_Type_IO;

   package body Decn_Record_IO is
      --  This package will carry the documentation for all the following packages
      --  Must have "use" for _IO for each of the components of the record
      use Integer_IO;
      --  This is a dummy used to GET the space Character PUT between components
      spacer : Character := ' ';

      --  The standard 6 procedures are defined as in TEXT_IO

      procedure Get(f : in File_Type; d : out Decn_Record) is
         --  Get from a file
      begin
         --  Get the first component
         Get(f, d.which);
         --  Then Get (and ignore) space Character which is Put between components
         Get(f, spacer);
         --  Get the next component
         Get(f, d.var);
      end Get;

      procedure Get(d : out Decn_Record) is
         --  Get from the current Input, in the same manner
      begin
         Get(d.which);
         Get(spacer);
         Get(d.var);
      end Get;

      procedure Put(f : in File_Type; d : in Decn_Record) is
         --  Put to a file
      begin
         --  Put the first component, with whatever Put is applicable (and use'd)
         Put(f, d.which, 1);
         --  Put the blank Character between components
         Put(f, ' ');
         --  Put the next component
         Put(f, d.var, 1);
      end Put;

      procedure Put(d : in Decn_Record) is
         --  Likewise for Put to current Output
      begin
         Put(d.which, 1);
         Put(' ');
         Put(d.var, 1);
      end Put;

      procedure Get(s : in String;
                    d : out Decn_Record; last : out Integer) is
         --  Get from a String
         --  Initialize the String position parameter
         --  Make it First-1 so the first String specification looks like later ones
         l : Integer := s'First - 1;
      begin
         --  Get with the use'd _IO package the first component
         Get(s(l+1..s'Last), d.which, l);
         --  The L is the last position read, so add one to skip the spacer
         l := l + 1;
         --  Get the next component
         Get(s(l+1..s'Last), d.var, last);
      end Get;

      procedure Put(s : out String; d : in Decn_Record) is
         l : Integer := s'First - 1;
         m : Integer := 0;
      begin
         --  Make a place the DEFAULT_WIDTH of the component  to be Put
         --  The DEFAULT_WIDTH has been set for these _IO packages to be
         --  the LONGEST component width, not the normal Ada default
         m := l + 1; --  But WHICH is to be PUT WIDTH 1
         --  Put onto the subString that is exactly the DEFAULT (LONGEST) size
         Put(s(l+1..m), d.which);
         --  Advance the position by 1 to the position to make the blank
         l := m + 1;
         --  Write the blank
         s(l) :=  ' ';
         --  Calculate the next subString, of DEFAULT_WIDTH for next component
         m := l + 1;
         --  Put the next component
         Put(s(l+1..m), d.var);
         --  The following may be necessary to fill the out String
         --  but usually the out String has been specified exactly
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end Decn_Record_IO;

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
      use case_type_io;
      use Gender_Type_IO;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), n.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
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
      use case_type_io;
      use Gender_Type_IO;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), p.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
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
      use case_type_io;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), p.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
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
      use case_type_io;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), a.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
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
      use case_type_io;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), num.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
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
      use person_type_io;
      use number_type_io;
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
         m := l + person_type_io.Default_Width;
         Put(s(l+1..m), v.person);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
         Put(s(l+1..m), v.number);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end verb_record_io;

   package body vpar_record_io is
      use Decn_Record_IO;
      use case_type_io;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), vp.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
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
      use case_type_io;
      use number_type_io;
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
         m := l + case_type_io.Default_Width;
         Put(s(l+1..m), vp.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.Default_Width;
         Put(s(l+1..m), vp.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + Gender_Type_IO.Default_Width;
         Put(s(l+1..m), vp.gender);
         s(m+1..s'Last) := (others => ' ');
      end Put;

   end supine_record_io;

   package body preposition_record_io is
      use case_type_io;

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
         m := l + case_type_io.Default_Width;
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
         ps : Part_Of_Speech_Type := x;
      begin
         Get(f, ps);
         Get(f, spacer);
         case ps is
            when n =>
               Get(f, noun);
               p := (n, noun);
            when pron =>
               Get(f, pronoun);
               p := (pron, pronoun);
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
               Get(f, vparticiple);
               p := (vpar, vparticiple);
            when supine =>
               Get(f, supin);
               p := (supine, supin);
            when prep =>
               Get(f, preposition);
               p := (prep, preposition);
            when conj =>
               Get(f, conjunction);
               p := (conj, conjunction);
            when interj =>
               Get(f, interjection);
               p := (interj, interjection);
            when tackon =>
               Get(f, tackn);
               p := (tackon, tackn);
            when prefix =>
               Get(f, prefx);
               p := (prefix, prefx);
            when suffix =>
               Get(f, suffx);
               p := (suffix, suffx);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end Get;

      procedure Get(p : out quality_record) is
         ps : Part_Of_Speech_Type := x;
      begin
         Get(ps);
         Get(spacer);
         case ps is
            when n =>
               Get(noun);
               p := (n, noun);
            when pron =>
               Get(pronoun);
               p := (pron, pronoun);
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
               Get(vparticiple);
               p := (vpar, vparticiple);
            when supine =>
               Get(supin);
               p := (supine, supin);
            when prep =>
               Get(preposition);
               p := (prep, preposition);
            when conj =>
               Get(conjunction);
               p := (conj, conjunction);
            when interj =>
               Get(interjection);
               p := (interj, interjection);
            when tackon =>
               Get(tackn);
               p := (tackon, tackn);
            when prefix =>
               Get(prefx);
               p := (prefix, prefx);
            when suffix =>
               Get(suffx);
               p := (suffix, suffx);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end Get;

      procedure Put(f : in File_Type; p : in quality_record) is
         c : constant Positive := Positive(Col(f));
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
               Put(f, p.vpar);
            when supine =>
               Put(f, p.supine);
            when prep =>
               Put(f, p.prep);
            when conj =>
               Put(f, p.conj);
            when interj =>
               Put(f, p.interj);
            when tackon =>
               Put(f, p.tackon);
            when prefix =>
               Put(f, p.prefix);
            when suffix =>
               Put(f, p.suffix);
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
               Put(p.vpar);
            when supine =>
               Put(p.supine);
            when prep =>
               Put(p.prep);
            when conj =>
               Put(p.conj);
            when interj =>
               Put(p.interj);
            when tackon =>
               Put(p.tackon);
            when prefix =>
               Put(p.prefix);
            when suffix =>
               Put(p.suffix);
            when others =>
               null;
         end case;
         Put(String'((Integer(Col)..quality_record_io.Default_Width+c-1 => ' ')));
         return;
      end Put;

      procedure Get(s : in String; p : out quality_record; last : out Integer) is
         l : Integer := s'First - 1;
         ps : Part_Of_Speech_Type := x;
      begin
         Get(s, ps, l);
         last := l;         --  In case it is not set later
         l := l + 1;
         case ps is
            when n =>
               Get(s(l+1..s'Last), noun, last);
               p := (n, noun);
            when pron =>
               Get(s(l+1..s'Last), pronoun, last);
               p := (pron, pronoun);
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
               Get(s(l+1..s'Last), vparticiple, last);
               p := (vpar, vparticiple);
            when supine =>
               Get(s(l+1..s'Last), supin, last);
               p := (supine, supin);
            when prep =>
               Get(s(l+1..s'Last), preposition, last);
               p := (prep, preposition);
            when conj =>
               Get(s(l+1..s'Last), conjunction, last);
               p := (conj, conjunction);
            when interj =>
               Get(s(l+1..s'Last), interjection, last);
               p := (interj, interjection);
            when tackon =>
               Get(s(l+1..s'Last), tackn, last);
               p := (tackon, tackn);
            when prefix =>
               Get(s(l+1..s'Last), prefx, last);
               p := (prefix, prefx);
            when suffix =>
               Get(s(l+1..s'Last), suffx, last);
               p := (suffix, suffx);
            when x =>
               p := (pofs => x);
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
            when n =>
               m := l + noun_record_io.Default_Width;
               Put(s(l+1..m), p.n);
            when pron =>
               m := l + pronoun_record_io.Default_Width;
               Put(s(l+1..m), p.pron);
            when pack =>
               m := l + propack_record_io.Default_Width;
               Put(s(l+1..m), p.pack);
            when adj =>
               m := l + adjective_record_io.Default_Width;
               Put(s(l+1..m), p.adj);
            when num =>
               m := l + numeral_record_io.Default_Width;
               Put(s(l+1..m), p.num);
            when adv =>
               m := l + adverb_record_io.Default_Width;
               Put(s(l+1..m), p.adv);
            when v =>
               m := l + verb_record_io.Default_Width;
               Put(s(l+1..m), p.v);
            when vpar =>
               m := l + vpar_record_io.Default_Width;
               Put(s(l+1..m), p.vpar);
            when supine =>
               m := l + supine_record_io.Default_Width;
               Put(s(l+1..m), p.supine);
            when prep =>
               m := l + preposition_record_io.Default_Width;
               Put(s(l+1..m), p.prep);
            when conj =>
               m := l + conjunction_record_io.Default_Width;
               Put(s(l+1..m), p.conj);
            when interj =>
               m := l + interjection_record_io.Default_Width;
               Put(s(l+1..m), p.interj);
            when tackon =>
               m := l + tackon_record_io.Default_Width;
               Put(s(l+1..m), p.tackon);
            when prefix =>
               m := l + prefix_record_io.Default_Width;
               Put(s(l+1..m), p.prefix);
            when suffix =>
               m := l + suffix_record_io.Default_Width;
               Put(s(l+1..m), p.suffix);
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
            Text_IO.Put_Line("ENDING ERRROR " & s);
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
      use age_type_io;
      use frequency_type_io;
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
               exit c4_loop when  lel(i).qual.pofs = pron  and then
                 (lel(i).qual.pron.decl.which = 1  or
                    lel(i).qual.pron.decl.which = 2);

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
      preface.Put("INFLECTION_ARRAY being loaded");
      preface.Set_Col(33);
      preface.Put("--  ");
      load_lel_indexes;                    --  Makes indexes from array
      preface.Put(number_of_inflections, 6);
      preface.Put(" entries");
      preface.Set_Col(55); preface.Put_Line("--  Loaded correctly");
   exception
      when Text_IO.Name_Error  =>
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
   case_type_io.Default_Width := case_type'Width;
   number_type_io.Default_Width := number_type'Width;
   person_type_io.Default_Width := 1;
   Comparison_Type_IO.Default_Width := Comparison_Type'Width;
   tense_type_io.Default_Width := tense_type'Width;
   voice_type_io.Default_Width := voice_type'Width;
   mood_type_io.Default_Width := mood_type'Width;
   Noun_Kind_Type_IO.Default_Width := Noun_Kind_Type'Width;
   Pronoun_Kind_Type_IO.Default_Width := Pronoun_Kind_Type'Width;
   Verb_Kind_Type_IO.Default_Width := Verb_Kind_Type'Width;
   Numeral_Sort_Type_IO.Default_Width := Numeral_Sort_Type'Width;
   age_type_io.Default_Width := age_type'Width;
   frequency_type_io.Default_Width := frequency_type'Width;

   Decn_Record_IO.Default_Width :=
     1 + 1 +   --WHICH_TYPE_IO_DEFAULT_WIDTH + 1 +
     1;        --VARIANT_TYPE_IO_DEFAULT_WIDTH;
   tense_voice_mood_record_io.Default_Width :=
     tense_type_io.Default_Width + 1 +
     voice_type_io.Default_Width + 1 +
     mood_type_io.Default_Width;
   noun_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   pronoun_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   propack_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   adjective_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     Comparison_Type_IO.Default_Width;
   adverb_record_io.Default_Width :=
     Comparison_Type_IO.Default_Width;
   verb_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     tense_voice_mood_record_io.Default_Width + 1 +
     person_type_io.Default_Width + 1 +
     number_type_io.Default_Width;
   vpar_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
     Gender_Type_IO.Default_Width + 1 +
     tense_voice_mood_record_io.Default_Width;
   supine_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
     Gender_Type_IO.Default_Width;
   preposition_record_io.Default_Width := case_type_io.Default_Width;
   conjunction_record_io.Default_Width := 0;
   interjection_record_io.Default_Width := 0;
   numeral_record_io.Default_Width :=
     Decn_Record_IO.Default_Width + 1 +
     case_type_io.Default_Width + 1 +
     number_type_io.Default_Width + 1 +
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
     age_type_io.Default_Width + 1 +
     frequency_type_io.Default_Width;

end Inflections_Package;
