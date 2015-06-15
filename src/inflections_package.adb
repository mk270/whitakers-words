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

with latin_file_names; use latin_file_names;
with preface;
package body inflections_package is

   function "<" (left, right : decn_record) return boolean is
   begin
      if left.which < right.which  or else
        (left.which = right.which  and then
           left.var < right.var)  then
         return true;
      else
         return false;
      end if;
   end "<";

   function "<" (left, right : quality_record) return boolean is
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
                    left.n.gender < right.n.gender)  then
                  return true;
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
                    left.pron.gender < right.pron.gender) then
                  return true;
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
                    left.pack.gender < right.pack.gender)   then
                  return true;
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
                    left.adj.co < right.adj.co)   then
                  return true;
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
                    left.v.person < right.v.person)   then
                  return true;
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
                    left.vpar.gender < right.vpar.gender)  then
                  return true;
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
                    left.supine.gender < right.supine.gender)  then
                  return true;
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
                    left.num.sort < right.num.sort)   then
                  return true;
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
      return false;
   exception
      when constraint_error  =>
         return left.pofs < right.pofs;
   end "<";

   function "<=" (left, right : part_of_speech_type) return boolean is
   begin
      if right = left  or else
        (left = pack and right = pron)  or else
        right = x    then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : decn_record) return boolean is
   begin
      if right = left  or else
        (right = decn_record'(0, 0)  and left.which /= 9)  or else
        right = decn_record'(left.which, 0)  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : gender_type) return boolean is
   begin
      if right = left  or else
        right = x     or else
        (right = c  and then (left = m or left = f))  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : case_type) return boolean is
   begin
      if right = left  or else
        right = x  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : number_type) return boolean is
   begin
      if right = left  or else
        right = x  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : person_type) return boolean is
   begin
      if right = left  or else
        right = 0  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : comparison_type) return boolean is
   begin
      if right = left  or else
        right = x  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : tense_voice_mood_record)  return boolean is
   begin
      if (right.tense = left.tense  or else
            right.tense = x)                    and then
        (right.voice  = left.voice   or else
           right.voice  = x)                    and then
        (right.mood = left.mood  or else
           right.mood = x)                        then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : noun_kind_type)   return boolean is
   begin
      if (right = left   or else
            right = x)  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : pronoun_kind_type)   return boolean is
   begin
      if (right = left   or else
            right = x)  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : stem_key_type)   return boolean is
   begin            --  Only works for 2 stem parts, not verbs
      if (right = left   or else
            right = 0)  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : age_type) return boolean is
   begin
      if right = left  or else
        right = x  then
         return true;
      else
         return false;
      end if;
   end "<=";

   function "<=" (left, right : frequency_type) return boolean is
   begin
      if right = left  or else
        right = x  then
         return true;
      else
         return false;
      end if;
   end "<=";

   package body stem_type_io is
      procedure get(f : in file_type; d : out stem_type) is
         c : character := ' ';
      begin
         d := null_stem_type;
         for i in 1..stem_type_io.default_width  loop
            get(f, c);
            if (c not in 'A'..'Z') and (c not in 'a'..'z')  then
               exit;
            else
               d(i) := c;
            end if;
         end loop;
      end get;

      procedure get(d : out stem_type) is
         c : character := ' ';
      begin
         d := null_stem_type;
         for i in 1..stem_type_io.default_width  loop
            text_io.get(c);
            if (c not in 'A'..'Z') and (c not in 'a'..'z')  then
               exit;
            else
               d(i) := c;
            end if;
         end loop;
      end get;

      procedure put(f : in file_type; d : in stem_type) is
      begin
         text_io.put(f, d);
      end put;

      procedure put(d : in stem_type) is
      begin
         text_io.put(d);
      end put;

      procedure get(s : in string; d : out stem_type;
                                   last : out integer) is
         c : character;
      begin
         d := null_stem_type;
         last := 0;
         for i in 1..stem_type_io.default_width  loop
            c := s(i);
            if (c not in 'A'..'Z') and (c not in 'a'..'z')  then
               exit;
            else
               d(i) := c;
               last := i;
            end if;
         end loop;
      end get;

      procedure put(s : out string; d : in stem_type) is
      begin
         s(s'first..s'first+stem_type_io.default_width-1) := d;
      end put;

   end stem_type_io;

   package body decn_record_io is
      --  This package will carry the documentation for all the following packages
      --  Must have "use" for _IO for each of the components of the record
      use integer_io;
      --  This is a dummy used to GET the space character PUT between components
      spacer : character := ' ';

      --  The standard 6 procedures are defined as in TEXT_IO

      procedure get(f : in file_type; d : out decn_record) is
         --  Get from a file
      begin
         --  Get the first component
         get(f, d.which);
         --  Then Get (and ignore) space character which is Put between components
         get(f, spacer);
         --  Get the next component
         get(f, d.var);
      end get;

      procedure get(d : out decn_record) is
         --  Get from the current input, in the same manner
      begin
         get(d.which);
         get(spacer);
         get(d.var);
      end get;

      procedure put(f : in file_type; d : in decn_record) is
         --  Put to a file
      begin
         --  Put the first component, with whatever Put is applicable (and use'd)
         put(f, d.which, 1);
         --  Put the blank character between components
         put(f, ' ');
         --  Put the next component
         put(f, d.var, 1);
      end put;

      procedure put(d : in decn_record) is
         --  Likewise for Put to current output
      begin
         put(d.which, 1);
         put(' ');
         put(d.var, 1);
      end put;

      procedure get(s : in string;
                    d : out decn_record; last : out integer) is
         --  Get from a string
         --  Initialize the string position parameter
         --  Make it first-1 so the first string specification looks like later ones
         l : integer := s'first - 1;
      begin
         --  Get with the use'd _IO package the first component
         get(s(l+1..s'last), d.which, l);
         --  The L is the last position read, so add one to skip the spacer
         l := l + 1;
         --  Get the next component
         get(s(l+1..s'last), d.var, last);
      end get;

      procedure put(s : out string; d : in decn_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         --  Make a place the DEFAULT_WIDTH of the component  to be Put
         --  The DEFAULT_WIDTH has been set for these _IO packages to be
         --  the LONGEST component width, not the normal Ada default
         m := l + 1;           --  But WHICH is to be PUT WIDTH 1
                               --  Put onto the substring that is exactly the DEFAULT (LONGEST) size
         put(s(l+1..m), d.which);
         --  Advance the position by 1 to the position to make the blank
         l := m + 1;
         --  Write the blank
         s(l) :=  ' ';
         --  Calculate the next substring, of DEFAULT_WIDTH for next component
         m := l + 1;
         --  Put the next component
         put(s(l+1..m), d.var);
         --  The following may be necessary to fill the out string
         --  but usually the out string has been specified exactly
         s(m+1..s'last) := (others => ' ');
      end put;

   end decn_record_io;

   package body tense_voice_mood_record_io is
      use tense_type_io;
      use voice_type_io;
      use mood_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; t : out tense_voice_mood_record) is
      begin
         get(f, t.tense);
         get(f, spacer);
         get(f, t.voice);
         get(f, spacer);
         get(f, t.mood);
      end get;

      procedure get(t : out tense_voice_mood_record) is
      begin
         get(t.tense);
         get(spacer);
         get(t.voice);
         get(spacer);
         get(t.mood);
      end get;

      procedure put(f : in file_type; t : in tense_voice_mood_record) is
      begin
         put(f, t.tense);
         put(f, ' ');
         put(f, t.voice);
         put(f, ' ');
         put(f, t.mood);
      end put;

      procedure put(t : in tense_voice_mood_record) is
      begin
         put(t.tense);
         put(' ');
         put(t.voice);
         put(' ');
         put(t.mood);
      end put;

      procedure get(s : in string;
                    t : out tense_voice_mood_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), t.tense, l);
         l := l + 1;
         get(s(l+1..s'last), t.voice, l);
         l := l + 1;
         get(s(l+1..s'last), t.mood, last);
      end get;

      procedure put(s : out string; t : in tense_voice_mood_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + tense_type_io.default_width;
         put(s(l+1..m), t.tense);
         l := m + 1;
         s(l) :=  ' ';
         m := l + voice_type_io.default_width;
         put(s(l+1..m), t.voice);
         l := m + 1;
         s(l) :=  ' ';
         m := l + mood_type_io.default_width;
         put(s(l+1..m), t.mood);
         s(m+1..s'last) := (others => ' ');
      end put;

   end tense_voice_mood_record_io;

   package body noun_record_io is
      use decn_record_io;
      use case_type_io;
      use gender_type_io;
      use number_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; n : out noun_record) is
      begin
         get(f, n.decl);
         get(f, spacer);
         get(f, n.cs);
         get(f, spacer);
         get(f, n.number);
         get(f, spacer);
         get(f, n.gender);
      end get;

      procedure get(n : out noun_record) is
      begin
         get(n.decl);
         get(spacer);
         get(n.cs);
         get(spacer);
         get(n.number);
         get(spacer);
         get(n.gender);
      end get;

      procedure put(f : in file_type; n : in noun_record) is
      begin
         put(f, n.decl);
         put(f, ' ');
         put(f, n.cs);
         put(f, ' ');
         put(f, n.number);
         put(f, ' ');
         put(f, n.gender);
      end put;

      procedure put(n : in noun_record) is
      begin
         put(n.decl);
         put(' ');
         put(n.cs);
         put(' ');
         put(n.number);
         put(' ');
         put(n.gender);
      end put;

      procedure get(s : in string; n : out noun_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), n.decl, l);
         l := l + 1;
         get(s(l+1..s'last), n.cs, l);
         l := l + 1;
         get(s(l+1..s'last), n.number, l);
         l := l + 1;
         get(s(l+1..s'last), n.gender, last);
      end get;

      procedure put(s : out string; n : in noun_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), n.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), n.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), n.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), n.gender);
         s(m+1..s'last) := (others => ' ');
      end put;

   end noun_record_io;

   package body pronoun_record_io is
      use decn_record_io;
      use case_type_io;
      use gender_type_io;
      use number_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; p : out pronoun_record) is
      begin
         get(f, p.decl);
         get(f, spacer);
         get(f, p.cs);
         get(f, spacer);
         get(f, p.number);
         get(f, spacer);
         get(f, p.gender);
      end get;

      procedure get(p : out pronoun_record) is
      begin
         get(p.decl);
         get(spacer);
         get(p.cs);
         get(spacer);
         get(p.number);
         get(spacer);
         get(p.gender);
      end get;

      procedure put(f : in file_type; p : in pronoun_record) is
      begin
         put(f, p.decl);
         put(f, ' ');
         put(f, p.cs);
         put(f, ' ');
         put(f, p.number);
         put(f, ' ');
         put(f, p.gender);
      end put;

      procedure put(p : in pronoun_record) is
      begin
         put(p.decl);
         put(' ');
         put(p.cs);
         put(' ');
         put(p.number);
         put(' ');
         put(p.gender);
      end put;

      procedure get(s : in string; p : out pronoun_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), p.decl, l);
         l := l + 1;
         get(s(l+1..s'last), p.cs, l);
         l := l + 1;
         get(s(l+1..s'last), p.number, l);
         l := l + 1;
         get(s(l+1..s'last), p.gender, last);
      end get;

      procedure put(s : out string; p : in pronoun_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), p.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), p.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), p.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), p.gender);
         s(m+1..s'last) := (others => ' ');
      end put;

   end pronoun_record_io;

   package body propack_record_io is
      use decn_record_io;
      use case_type_io;
      use number_type_io;
      use gender_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; p : out propack_record) is
      begin
         get(f, p.decl);
         get(f, spacer);
         get(f, p.cs);
         get(f, spacer);
         get(f, p.number);
         get(f, spacer);
         get(f, p.gender);
      end get;

      procedure get(p : out propack_record) is
      begin
         get(p.decl);
         get(spacer);
         get(p.cs);
         get(spacer);
         get(p.number);
         get(spacer);
         get(p.gender);
      end get;

      procedure put(f : in file_type; p : in propack_record) is
      begin
         put(f, p.decl);
         put(f, ' ');
         put(f, p.cs);
         put(f, ' ');
         put(f, p.number);
         put(f, ' ');
         put(f, p.gender);
      end put;

      procedure put(p : in propack_record) is
      begin
         put(p.decl);
         put(' ');
         put(p.cs);
         put(' ');
         put(p.number);
         put(' ');
         put(p.gender);
      end put;

      procedure get(s : in string; p : out propack_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), p.decl, l);
         l := l + 1;
         get(s(l+1..s'last), p.cs, l);
         l := l + 1;
         get(s(l+1..s'last), p.number, l);
         l := l + 1;
         get(s(l+1..s'last), p.gender, last);
      end get;

      procedure put(s : out string; p : in propack_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), p.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), p.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), p.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), p.gender);
         s(m+1..s'last) := (others => ' ');
      end put;

   end propack_record_io;

   package body adjective_record_io is
      use decn_record_io;
      use gender_type_io;
      use case_type_io;
      use number_type_io;
      use comparison_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; a : out adjective_record) is
      begin
         get(f, a.decl);
         get(f, spacer);
         get(f, a.cs);
         get(f, spacer);
         get(f, a.number);
         get(f, spacer);
         get(f, a.gender);
         get(f, spacer);
         get(f, a.co);
      end get;

      procedure get(a : out adjective_record) is
      begin
         get(a.decl);
         get(spacer);
         get(a.cs);
         get(spacer);
         get(a.number);
         get(spacer);
         get(a.gender);
         get(spacer);
         get(a.co);
      end get;

      procedure put(f : in file_type; a : in adjective_record) is
      begin
         put(f, a.decl);
         put(f, ' ');
         put(f, a.cs);
         put(f, ' ');
         put(f, a.number);
         put(f, ' ');
         put(f, a.gender);
         put(f, ' ');
         put(f, a.co);
      end put;

      procedure put(a : in adjective_record) is
      begin
         put(a.decl);
         put(' ');
         put(a.cs);
         put(' ');
         put(a.number);
         put(' ');
         put(a.gender);
         put(' ');
         put(a.co);
      end put;

      procedure get(s : in string; a : out adjective_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), a.decl, l);
         l := l + 1;
         get(s(l+1..s'last), a.cs, l);
         l := l + 1;
         get(s(l+1..s'last), a.number, l);
         l := l + 1;
         get(s(l+1..s'last), a.gender, l);
         l := l + 1;
         get(s(l+1..s'last), a.co, last);
      end get;

      procedure put(s : out string; a : in adjective_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), a.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), a.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), a.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), a.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + comparison_type_io.default_width;
         put(s(l+1..m), a.co);
         s(m+1..s'last) := (others => ' ');
      end put;

   end adjective_record_io;

   package body numeral_record_io is
      use decn_record_io;
      use case_type_io;
      use number_type_io;
      use gender_type_io;
      use numeral_sort_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; num : out numeral_record) is
      begin
         get(f, num.decl);
         get(f, spacer);
         get(f, num.cs);
         get(f, spacer);
         get(f, num.number);
         get(f, spacer);
         get(f, num.gender);
         get(f, spacer);
         get(f, num.sort);
      end get;

      procedure get(num : out numeral_record) is
      begin
         get(num.decl);
         get(spacer);
         get(spacer);
         get(num.number);
         get(spacer);
         get(num.gender);
         get(spacer);
         get(num.sort);
      end get;

      procedure put(f : in file_type; num : in numeral_record) is
      begin
         put(f, num.decl);
         put(f, ' ');
         put(f, num.cs);
         put(f, ' ');
         put(f, num.number);
         put(f, ' ');
         put(f, num.gender);
         put(f, ' ');
         put(f, num.sort);
      end put;

      procedure put(num : in numeral_record) is
      begin
         put(num.decl);
         put(' ');
         put(num.cs);
         put(' ');
         put(num.number);
         put(' ');
         put(num.gender);
         put(' ');
         put(num.sort);
      end put;

      procedure get(s : in string; num : out numeral_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), num.decl, l);
         l := l + 1;
         get(s(l+1..s'last), num.cs, l);
         l := l + 1;
         get(s(l+1..s'last), num.number, l);
         l := l + 1;
         get(s(l+1..s'last), num.gender, l);
         l := l + 1;
         get(s(l+1..s'last), num.sort, last);
      end get;

      procedure put(s : out string; num : in numeral_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), num.decl);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), num.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), num.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), num.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + numeral_sort_type_io.default_width;
         put(s(l+1..m), num.sort);
         s(m+1..s'last) := (others => ' ');
      end put;

   end numeral_record_io;

   package body adverb_record_io is
      use comparison_type_io;

      procedure get(f : in file_type; a : out adverb_record) is
      begin
         get(f, a.co);
      end get;

      procedure get(a : out adverb_record) is
      begin
         get(a.co);
      end get;

      procedure put(f : in file_type; a : in adverb_record) is
      begin
         put(f, a.co);
      end put;

      procedure put(a : in adverb_record) is
      begin
         put(a.co);
      end put;

      procedure get(s : in string; a : out adverb_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         get(s(l+1..s'last), a.co, last);
      end get;

      procedure put(s : out string; a : in adverb_record) is
         l : constant integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + comparison_type_io.default_width;
         put(s(l+1..m), a.co);
         s(m+1..s'last) := (others => ' ');
      end put;

   end adverb_record_io;

   package body verb_record_io is
      use decn_record_io;
      use tense_voice_mood_record_io;
      use person_type_io;
      use number_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; v : out verb_record) is
      begin
         get(f, v.con);
         get(f, spacer);
         get(f, v.tense_voice_mood);
         get(f, spacer);
         get(f, v.person);
         get(f, spacer);
         get(f, v.number);
      end get;

      procedure get(v : out verb_record) is
      begin
         get(v.con);
         get(spacer);
         get(v.tense_voice_mood);
         get(spacer);
         get(v.person);
         get(spacer);
         get(v.number);
      end get;

      procedure put(f : in file_type; v : in verb_record) is
      begin
         put(f, v.con);
         put(f, ' ');
         put(f, v.tense_voice_mood);
         put(f, ' ');
         put(f, v.person);
         put(f, ' ');
         put(f, v.number);
      end put;

      procedure put(v : in verb_record) is
      begin
         put(v.con);
         put(' ');
         put(v.tense_voice_mood);
         put(' ');
         put(v.person);
         put(' ');
         put(v.number);
      end put;

      procedure get(s : in string; v : out verb_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), v.con, l);
         l := l + 1;
         get(s(l+1..s'last), v.tense_voice_mood, l);
         l := l + 1;
         get(s(l+1..s'last), v.person, l);
         l := l + 1;
         get(s(l+1..s'last), v.number, last);
      end get;

      procedure put(s : out string; v : in verb_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), v.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + tense_voice_mood_record_io.default_width;
         put(s(l+1..m), v.tense_voice_mood);
         l := m + 1;
         s(l) :=  ' ';
         m := l + person_type_io.default_width;
         put(s(l+1..m), v.person);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), v.number);
         s(m+1..s'last) := (others => ' ');
      end put;

   end verb_record_io;

   package body vpar_record_io is
      use decn_record_io;
      use case_type_io;
      use number_type_io;
      use gender_type_io;
      use tense_voice_mood_record_io;
      spacer : character := ' ';

      procedure get(f : in file_type; vp : out vpar_record) is
      begin
         get(f, vp.con);
         get(f, spacer);
         get(f, vp.cs);
         get(f, spacer);
         get(f, vp.number);
         get(f, spacer);
         get(f, vp.gender);
         get(f, spacer);
         get(f, vp.tense_voice_mood);
      end get;

      procedure get(vp : out vpar_record) is
      begin
         get(vp.con);
         get(spacer);
         get(vp.cs);
         get(spacer);
         get(vp.number);
         get(spacer);
         get(vp.gender);
         get(spacer);
         get(vp.tense_voice_mood);
      end get;

      procedure put(f : in file_type; vp : in vpar_record) is
      begin
         put(f, vp.con);
         put(f, ' ');
         put(f, vp.cs);
         put(f, ' ');
         put(f, vp.number);
         put(f, ' ');
         put(f, vp.gender);
         put(f, ' ');
         put(f, vp.tense_voice_mood);
      end put;

      procedure put(vp : in vpar_record) is
      begin
         put(vp.con);
         put(' ');
         put(vp.cs);
         put(' ');
         put(vp.number);
         put(' ');
         put(vp.gender);
         put(' ');
         put(vp.tense_voice_mood);
      end put;

      procedure get(s : in string; vp : out vpar_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), vp.con, l);
         l := l + 1;
         get(s(l+1..s'last), vp.cs, l);
         l := l + 1;
         get(s(l+1..s'last), vp.number, l);
         l := l + 1;
         get(s(l+1..s'last), vp.gender, l);
         l := l + 1;
         get(s(l+1..s'last), vp.tense_voice_mood, last);
      end get;

      procedure put(s : out string; vp : in vpar_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), vp.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), vp.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), vp.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), vp.gender);
         l := m + 1;
         s(l) :=  ' ';
         m := l + tense_voice_mood_record_io.default_width;
         put(s(l+1..m), vp.tense_voice_mood);
         s(m+1..s'last) := (others => ' ');
      end put;

   end vpar_record_io;

   package body supine_record_io is
      use decn_record_io;
      use case_type_io;
      use number_type_io;
      use gender_type_io;
      spacer : character := ' ';

      procedure get(f : in file_type; vp : out supine_record) is
      begin
         get(f, vp.con);
         get(f, spacer);
         get(f, vp.cs);
         get(f, spacer);
         get(f, vp.number);
         get(f, spacer);
         get(f, vp.gender);
      end get;

      procedure get(vp : out supine_record) is
      begin
         get(vp.con);
         get(spacer);
         get(vp.cs);
         get(spacer);
         get(vp.number);
         get(spacer);
         get(vp.gender);
      end get;

      procedure put(f : in file_type; vp : in supine_record) is
      begin
         put(f, vp.con);
         put(f, ' ');
         put(f, vp.cs);
         put(f, ' ');
         put(f, vp.number);
         put(f, ' ');
         put(f, vp.gender);
      end put;

      procedure put(vp : in supine_record) is
      begin
         put(vp.con);
         put(' ');
         put(vp.cs);
         put(' ');
         put(vp.number);
         put(' ');
         put(vp.gender);
      end put;

      procedure get(s : in string; vp : out supine_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         get(s(l+1..s'last), vp.con, l);
         l := l + 1;
         get(s(l+1..s'last), vp.cs, l);
         l := l + 1;
         get(s(l+1..s'last), vp.number, l);
         l := l + 1;
         get(s(l+1..s'last), vp.gender, last);
      end get;

      procedure put(s : out string; vp : in supine_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + decn_record_io.default_width;
         put(s(l+1..m), vp.con);
         l := m + 1;
         s(l) :=  ' ';
         m := l + case_type_io.default_width;
         put(s(l+1..m), vp.cs);
         l := m + 1;
         s(l) :=  ' ';
         m := l + number_type_io.default_width;
         put(s(l+1..m), vp.number);
         l := m + 1;
         s(l) :=  ' ';
         m := l + gender_type_io.default_width;
         put(s(l+1..m), vp.gender);
         s(m+1..s'last) := (others => ' ');
      end put;

   end supine_record_io;

   package body preposition_record_io is
      use case_type_io;

      procedure get(f : in file_type; p : out preposition_record) is
      begin
         get(f, p.obj);
      end get;

      procedure get(p : out preposition_record) is
      begin
         get(p.obj);
      end get;

      procedure put(f : in file_type; p : in preposition_record) is
      begin
         put(f, p.obj);
      end put;

      procedure put(p : in preposition_record) is
      begin
         put(p.obj);
      end put;

      procedure get(s : in string; p : out preposition_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         get(s(l+1..s'last), p.obj, last);
      end get;

      procedure put(s : out string; p : in preposition_record) is
         l : constant integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + case_type_io.default_width;
         put(s(l+1..m), p.obj);
         s(m+1..s'last) := (others => ' ');
      end put;

   end preposition_record_io;

   package body conjunction_record_io is
      null_conjunction_record : conjunction_record;
      
      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; c : out conjunction_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");    
         --  There is actually nothing to a CONJUNCTION_RECORD, no compoonents
      begin
         c := null_conjunction_record;
      end get;

      procedure get(c : out conjunction_record) is
      begin
         c := null_conjunction_record;
      end get;

      procedure put(f : in file_type; c : in conjunction_record) is
      begin
         null;
      end put;

      procedure put(c : in conjunction_record) is
      begin
         null;
      end put;

      procedure get(s : in string; c : out conjunction_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         c := null_conjunction_record;
         last := l - 1;  --  LAST did not even get to S'FIRST, since nothing to read
      end get;

      pragma Warnings (Off, "formal parameter ""c"" is not referenced");
      procedure put(s : out string; c : in conjunction_record) is
         pragma Warnings (On, "formal parameter ""c"" is not referenced");    
         --  Since there is no component, just make the out string blank
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end conjunction_record_io;

   package body interjection_record_io is
      null_interjection_record : interjection_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; i : out interjection_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");    
      begin
         i := null_interjection_record;
      end get;

      procedure get(i : out interjection_record) is
      begin
         i := null_interjection_record;
      end get;

      procedure put(f : in file_type; i : in interjection_record) is
      begin
         null;
      end put;

      procedure put(i : in interjection_record) is
      begin
         null;
      end put;

      procedure get(s : in string; i : out interjection_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         i := null_interjection_record;
         last := l - 1;
      end get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure put(s : out string; i : in interjection_record) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");    
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end interjection_record_io;

   package body tackon_record_io is
      null_tackon_record : tackon_record;

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; i : out tackon_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");    
      begin
         i := null_tackon_record;
      end get;

      procedure get(i : out tackon_record) is
      begin
         i := null_tackon_record;
      end get;

      procedure put(f : in file_type; i : in tackon_record) is
      begin
         null;
      end put;

      procedure put(i : in tackon_record) is
      begin
         null;
      end put;

      procedure get(s : in string; i : out tackon_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         i := null_tackon_record;
         last := l - 1;
      end get;

      pragma Warnings (Off, "formal parameter ""i"" is not referenced");
      procedure put(s : out string; i : in tackon_record) is
         pragma Warnings (On, "formal parameter ""i"" is not referenced");    
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end tackon_record_io;

   package body prefix_record_io is

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; p : out prefix_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");    
      begin
         p := null_prefix_record;
      end get;

      procedure get(p : out prefix_record) is
      begin
         p := null_prefix_record;
      end get;

      procedure put(f : in file_type; p : in prefix_record) is
      begin
         null;
      end put;

      procedure put(p : in prefix_record) is
      begin
         null;
      end put;

      procedure get(s : in string; p : out prefix_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         p := null_prefix_record;
         last := l - 1;
      end get;

      pragma Warnings (Off, "formal parameter ""p"" is not referenced");
      procedure put(s : out string; p : in prefix_record) is
         pragma Warnings (On, "formal parameter ""p"" is not referenced");    
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end prefix_record_io;

   package body suffix_record_io is

      pragma Warnings (Off, "formal parameter ""f"" is not referenced");
      procedure get(f : in file_type; p : out suffix_record) is
         pragma Warnings (On, "formal parameter ""f"" is not referenced");    
      begin
         p := null_suffix_record;
      end get;

      procedure get(p : out suffix_record) is
      begin
         p := null_suffix_record;
      end get;

      procedure put(f : in file_type; p : in suffix_record) is
      begin
         null;
      end put;

      procedure put(p : in suffix_record) is
      begin
         null;
      end put;

      procedure get(s : in string; p : out suffix_record; last : out integer) is
         l : constant integer := s'first - 1;
      begin
         p := null_suffix_record;
         last := l - 1;
      end get;

      pragma Warnings (Off, "formal parameter ""p"" is not referenced");
      procedure put(s : out string; p : in suffix_record) is
         pragma Warnings (On, "formal parameter ""p"" is not referenced");    
      begin
         s(s'first..s'last) := (others => ' ');
      end put;

   end suffix_record_io;

   package body quality_record_io is
      use part_of_speech_type_io;
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
      spacer : character := ' ';

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

      procedure get(f : in file_type; p : out quality_record) is
         ps : part_of_speech_type := x;
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
               get(f, vparticiple);
               p := (vpar, vparticiple);
            when supine =>
               get(f, supin);
               p := (supine, supin);
            when prep =>
               get(f, preposition);
               p := (prep, preposition);
            when conj =>
               get(f, conjunction);
               p := (conj, conjunction);
            when interj =>
               get(f, interjection);
               p := (interj, interjection);
            when tackon =>
               get(f, tackn);
               p := (tackon, tackn);
            when prefix =>
               get(f, prefx);
               p := (prefix, prefx);
            when suffix =>
               get(f, suffx);
               p := (suffix, suffx);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end get;

      procedure get(p : out quality_record) is
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
               get(vparticiple);
               p := (vpar, vparticiple);
            when supine =>
               get(supin);
               p := (supine, supin);
            when prep =>
               get(preposition);
               p := (prep, preposition);
            when conj =>
               get(conjunction);
               p := (conj, conjunction);
            when interj =>
               get(interjection);
               p := (interj, interjection);
            when tackon =>
               get(tackn);
               p := (tackon, tackn);
            when prefix =>
               get(prefx);
               p := (prefix, prefx);
            when suffix =>
               get(suffx);
               p := (suffix, suffx);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end get;

      procedure put(f : in file_type; p : in quality_record) is
         c : constant positive := positive(col(f));
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
               put(f, p.vpar);
            when supine =>
               put(f, p.supine);
            when prep =>
               put(f, p.prep);
            when conj =>
               put(f, p.conj);
            when interj =>
               put(f, p.interj);
            when tackon =>
               put(f, p.tackon);
            when prefix =>
               put(f, p.prefix);
            when suffix =>
               put(f, p.suffix);
            when others =>
               null;
         end case;
         put(f, string'((integer(col(f))..quality_record_io.default_width+c-1 => ' ')));
         return;
      end put;

      procedure put(p : in quality_record) is
         c : constant positive := positive(col);
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
               put(p.vpar);
            when supine =>
               put(p.supine);
            when prep =>
               put(p.prep);
            when conj =>
               put(p.conj);
            when interj =>
               put(p.interj);
            when tackon =>
               put(p.tackon);
            when prefix =>
               put(p.prefix);
            when suffix =>
               put(p.suffix);
            when others =>
               null;
         end case;
         put(string'((integer(col)..quality_record_io.default_width+c-1 => ' ')));
         return;
      end put;

      procedure get(s : in string; p : out quality_record; last : out integer) is
         l : integer := s'first - 1;
         ps : part_of_speech_type := x;
      begin
         get(s, ps, l);
         last := l;         --  In case it is not set later
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
               get(s(l+1..s'last), vparticiple, last);
               p := (vpar, vparticiple);
            when supine =>
               get(s(l+1..s'last), supin, last);
               p := (supine, supin);
            when prep =>
               get(s(l+1..s'last), preposition, last);
               p := (prep, preposition);
            when conj =>
               get(s(l+1..s'last), conjunction, last);
               p := (conj, conjunction);
            when interj =>
               get(s(l+1..s'last), interjection, last);
               p := (interj, interjection);
            when tackon =>
               get(s(l+1..s'last), tackn, last);
               p := (tackon, tackn);
            when prefix =>
               get(s(l+1..s'last), prefx, last);
               p := (prefix, prefx);
            when suffix =>
               get(s(l+1..s'last), suffx, last);
               p := (suffix, suffx);
            when x =>
               p := (pofs => x);
         end case;
         return;
      end get;

      procedure put(s : out string; p : in quality_record) is
         --  Note that this does not Put with a uniform width
         --  which would require a constant QUALITY_RECORD_IO.DEFAULT_WIDTH
         --  Rather we Put to minimal size with NOUN_RECORD_IO.DEFAULT_WIDTH,
         --  PRONOUN_RECORD_IO,DEFAULT_WIDTH, ...
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + part_of_speech_type_io.default_width;
         put(s(l+1..m), p.pofs);
         l := m + 1;
         s(l) :=  ' ';
         case p.pofs is
            when n =>
               m := l + noun_record_io.default_width;
               put(s(l+1..m), p.n);
            when pron =>
               m := l + pronoun_record_io.default_width;
               put(s(l+1..m), p.pron);
            when pack =>
               m := l + propack_record_io.default_width;
               put(s(l+1..m), p.pack);
            when adj =>
               m := l + adjective_record_io.default_width;
               put(s(l+1..m), p.adj);
            when num =>
               m := l + numeral_record_io.default_width;
               put(s(l+1..m), p.num);
            when adv =>
               m := l + adverb_record_io.default_width;
               put(s(l+1..m), p.adv);
            when v =>
               m := l + verb_record_io.default_width;
               put(s(l+1..m), p.v);
            when vpar =>
               m := l + vpar_record_io.default_width;
               put(s(l+1..m), p.vpar);
            when supine =>
               m := l + supine_record_io.default_width;
               put(s(l+1..m), p.supine);
            when prep =>
               m := l + preposition_record_io.default_width;
               put(s(l+1..m), p.prep);
            when conj =>
               m := l + conjunction_record_io.default_width;
               put(s(l+1..m), p.conj);
            when interj =>
               m := l + interjection_record_io.default_width;
               put(s(l+1..m), p.interj);
            when tackon =>
               m := l + tackon_record_io.default_width;
               put(s(l+1..m), p.tackon);
            when prefix =>
               m := l + prefix_record_io.default_width;
               put(s(l+1..m), p.prefix);
            when suffix =>
               m := l + suffix_record_io.default_width;
               put(s(l+1..m), p.suffix);
            when others =>
               null;
         end case;
         s(m+1..s'last) := (others => ' ');
      end put;

   end quality_record_io;

   package body ending_record_io is
      use integer_io;
      spacer : character := ' ';

      sf : ending := (others => ' ');
      blanks : constant ending := (others => ' ');
      n : ending_size_type := 0;

      procedure get(f : in file_type; x : out ending_record) is
      begin
         sf := blanks;
         get(f, n);
         if n = 0  then
            x := null_ending_record;
         else
            get(f, spacer);             --  Note this means exactly one blank
            get(f, sf(1..n));
            x := (n, sf);
         end if;
      end get;

      procedure get(x : out ending_record) is
      begin
         sf := blanks;
         get(n);
         if n = 0  then
            x := null_ending_record;
         else
            get(spacer);
            get(sf(1..n));
            x := (n, sf);
         end if;
      end get;

      procedure put(f : in file_type; x : in ending_record) is
      begin
         put(f, x.size, 1);
         put(f, ' ');
         put(f, x.suf(1..x.size) & blanks(x.size+1..max_ending_size));
      end put;

      procedure put(x : in ending_record) is
      begin
         put(x.size, 1);
         put(' ');
         put(x.suf(1..x.size) & blanks(x.size+1..max_ending_size));
      end put;

      procedure get(s : in string; x : out ending_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         sf := blanks;
         get(s(l+1..s'last), n, l);
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
            text_io.put_line("ENDING ERRROR " & s);
      end get;

      procedure put(s : out string; x : in ending_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + 2;
         put(s(l+1..m), x.size);
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
         --  Then to fill out the rest of the out string, if any
         s(m+1..s'last) := (others => ' ');
      end put;

   end ending_record_io;

   package body inflection_record_io is
      use quality_record_io;
      use stem_key_type_io;
      use ending_record_io;
      use age_type_io;
      use frequency_type_io;
      spacer : character := ' ';

      pe : inflection_record;

      procedure get(f : in file_type; p : out inflection_record) is
      begin
         get(f, p.qual);
         get(f, spacer);
         get(f, p.key);
         get(f, spacer);
         get(f, p.ending);
         get(f, spacer);
         get(f, p.age);
         get(f, spacer);
         get(f, p.freq);
      end get;

      procedure get(p : out inflection_record) is
      begin
         get(p.qual);
         get(spacer);
         get(p.key);
         get(spacer);
         get(p.ending);
         get(spacer);
         get(p.age);
         get(spacer);
         get(p.freq);
      end get;

      procedure put(f : in file_type; p : in inflection_record) is
      begin
         put(f, p.qual);
         put(f, ' ');
         put(f, p.key, 1);
         put(f, ' ');
         put(f, p.ending);
         put(f, ' ');
         put(f, p.age);
         put(f, ' ');
         put(f, p.freq);
      end put;

      procedure put(p : in inflection_record) is
      begin
         put(p.qual);
         put(' ');
         put(p.key, 1);
         put(' ');
         put(p.ending);
         put(' ');
         put(p.age);
         put(' ');
         put(p.freq);
      end put;

      procedure get(s : in string; p : out inflection_record; last : out integer) is
         l : integer := s'first - 1;
      begin
         last := 0;
         p := pe;
         get(s(l+1..s'last), p.qual, l);
         l := l + 1;
         get(s(l+1..s'last), p.key, l);
         l := l + 1;
         get(s(l+1..s'last), p.ending, l);
         l := l + 1;
         get(s(l+1..s'last), p.age, l);
         l := l + 1;
         get(s(l+1..s'last), p.freq, last);
      end get;

      procedure put(s : out string; p : in inflection_record) is
         l : integer := s'first - 1;
         m : integer := 0;
      begin
         m := l + quality_record_io.default_width;
         put(s(l+1..m), p.qual);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 1;
         put(s(l+1..m), p.key);
         l := m + 1;
         s(l) :=  ' ';
         m := l + ending_record_io.default_width;
         put(s(l+1..m), p.ending);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 1;
         put(s(l+1..m), p.age);
         l := m + 1;
         s(l) :=  ' ';
         m := l + 1;
         put(s(l+1..m), p.freq);
         s(m+1..s'last) := (others => ' ');
      end put;

   end inflection_record_io;

   procedure establish_inflections_section  is
      --  Loads the inflection array from the file prepared in FILE_INFLECTIONS_SECTION
      --  If N = 0 (an artifical flag for the section for blank inflections = 5)
      --  computes the LELL..LELF indices for use in WORD
      use inflection_record_io;
      use lel_section_io;

      procedure load_lel_indexes is
         --  Load arrays from file
         i  : integer := 0;
         --IR : INFLECTION_RECORD;
         n, xn : integer := 0;
         ch, xch : character := ' ';
         inflections_sections_file : lel_section_io.file_type;
      begin
         open(inflections_sections_file, in_file, inflections_sections_name);
         number_of_inflections := 0;

         lel_section_io.read(inflections_sections_file,
                             lel,
                             lel_section_io.positive_count(5));

         i := 1;
         belf(0, ' ') := i;
         bell(0, ' ') := 0;
         loop
            exit when lel(i) = null_inflection_record;
            bel(i) := lel(i);

            bell(0, ' ') := i;
            i := i + 1;
         end loop;

         number_of_inflections := number_of_inflections + i - 1;

         lel_section_io.read(inflections_sections_file,
                             lel,
                             lel_section_io.positive_count(1));

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
               exit c1_loop when lel(i) = null_inflection_record;

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

         lel_section_io.read(inflections_sections_file,
                             lel,
                             lel_section_io.positive_count(2));

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
               exit c2_loop when lel(i) = null_inflection_record;

               n := lel(i).ending.size;

               ch := lel(i).ending.suf(n);
               exit when ch > 'r';

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

         lel_section_io.read(inflections_sections_file,
                             lel,
                             lel_section_io.positive_count(3));

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
               exit c3_loop when lel(i) = null_inflection_record;

               n := lel(i).ending.size;

               ch := lel(i).ending.suf(n);
               exit when ch > 's';

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

         lel_section_io.read(inflections_sections_file,
                             lel,
                             lel_section_io.positive_count(4));

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
                  exit c_p_loop when lel(i) = null_inflection_record;

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
            when constraint_error => null;
         end;

         pell(xn, xch) := i - 1;
         number_of_inflections := number_of_inflections + i - 1;
         close(inflections_sections_file);

      end load_lel_indexes;

   begin

      preface.put("INFLECTION_ARRAY being loaded");
      preface.set_col(33);
      preface.put("--  ");
      load_lel_indexes;                    --  Makes indexes from array
      preface.put(number_of_inflections, 6);
      preface.put(" entries");
      preface.set_col(55); preface.put_line("--  Loaded correctly");

   exception
      when text_io.name_error  =>
         new_line;
         put_line("There is no " & inflections_sections_name & " file.");
         put_line("The program cannot work without one.");
         put_line("Make sure you are in the subdirectory containing the files");
         put_line("for inflections, dictionary, addons and uniques.");
         raise give_up;

   end establish_inflections_section;

begin  --  initialization of body of INFLECTIONS_PACKAGE
       --TEXT_IO.PUT_LINE("Initializing INFLECTIONS_PACKAGE");

   part_of_speech_type_io.default_width := part_of_speech_type'width;
   gender_type_io.default_width := gender_type'width;
   case_type_io.default_width := case_type'width;
   number_type_io.default_width := number_type'width;
   person_type_io.default_width := 1;
   comparison_type_io.default_width := comparison_type'width;
   tense_type_io.default_width := tense_type'width;
   voice_type_io.default_width := voice_type'width;
   mood_type_io.default_width := mood_type'width;
   noun_kind_type_io.default_width := noun_kind_type'width;
   pronoun_kind_type_io.default_width := pronoun_kind_type'width;
   verb_kind_type_io.default_width := verb_kind_type'width;
   numeral_sort_type_io.default_width := numeral_sort_type'width;
   age_type_io.default_width := age_type'width;
   frequency_type_io.default_width := frequency_type'width;

   decn_record_io.default_width :=
     1 + 1 +   --WHICH_TYPE_IO_DEFAULT_WIDTH + 1 +
     1;        --VARIANT_TYPE_IO_DEFAULT_WIDTH;
   tense_voice_mood_record_io.default_width :=
     tense_type_io.default_width + 1 +
     voice_type_io.default_width + 1 +
     mood_type_io.default_width;
   noun_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width;
   pronoun_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width;
   propack_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width;
   adjective_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width + 1 +
     comparison_type_io.default_width;
   adverb_record_io.default_width :=
     comparison_type_io.default_width;
   verb_record_io.default_width :=
     decn_record_io.default_width + 1 +
     tense_voice_mood_record_io.default_width + 1 +
     person_type_io.default_width + 1 +
     number_type_io.default_width;
   vpar_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width + 1 +
     tense_voice_mood_record_io.default_width;
   supine_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width;
   preposition_record_io.default_width := case_type_io.default_width;
   conjunction_record_io.default_width := 0;
   interjection_record_io.default_width := 0;
   numeral_record_io.default_width :=
     decn_record_io.default_width + 1 +
     case_type_io.default_width + 1 +
     number_type_io.default_width + 1 +
     gender_type_io.default_width + 1 +
     numeral_sort_type_io.default_width;
   tackon_record_io.default_width := 0;
   prefix_record_io.default_width := 0;
   suffix_record_io.default_width := 0;
   quality_record_io.default_width := part_of_speech_type_io.default_width + 1 +
     vpar_record_io.default_width; --  Largest
   ending_record_io.default_width := 3 + 1 +
     max_ending_size;
   inflection_record_io.default_width := quality_record_io.default_width + 1 +
     1  + 1 +
     ending_record_io.default_width + 1 +
     age_type_io.default_width + 1 +
     frequency_type_io.default_width;

end inflections_package;
