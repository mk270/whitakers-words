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

with Ada.Text_IO;
with direct_io;
with Strings_Package; use Strings_Package;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
procedure sorter is
   --  This program sorts a file of lines (Strings) on 5 subStrings Mx..Nx
   --  Sort by Stringwise (different cases), numeric, or POS enumeration

   package Boolean_IO is new Ada.Text_IO.enumeration_io(Boolean);
   use Boolean_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);
   use Integer_IO;
   package float_io is new Ada.Text_IO.float_io(float);
   use float_io;
   use Ada.Text_IO;

   name_length : constant := 80;
   st, enter_line : String(1..name_length) := (others => ' ');
   ls, last : Integer := 0;
   Input_name : String(1..80) := (others => ' ');

   line_length : constant := 300;        --  ##################################
                                         --  Max line length on Input file
                                         --  Shorter => less disk space to sort
   current_length : Integer := 0;
   subtype text_type is String(1..line_length);
   --type LINE_TYPE is
   -- record
   --   CURRENT_LENGTH : CURRENT_LINE_LENGTH_TYPE := 0;
   --   TEXT : TEXT_TYPE;
   -- end record;
   package line_io is new direct_io(text_type);
   use line_io;
   blank_text : text_type := (others => ' ');

   line_text : text_type := blank_text;
   old_line : text_type := blank_text;
   p_line : text_type := blank_text;

   type sort_type is (a, c, g, u, n, f, p, r, s);
   package sort_type_io is new Ada.Text_IO.enumeration_io(sort_type);
   use sort_type_io;

   type way_type is (i, d);
   package way_type_io is new Ada.Text_IO.enumeration_io(way_type);
   use way_type_io;

   Input  : Ada.Text_IO.File_Type;
   Output : Ada.Text_IO.File_Type;
   work   : line_io.File_Type;

   m1, m2, m3, m4, m5 : Natural := 1;
   n1, n2, n3, n4, n5 : Natural := line_length;
   z1, z2, z3, z4, z5 : Natural := 0;

   s1, s2, s3, s4, s5 : sort_type := a;
   w1, w2, w3, w4, w5 : way_type := i;

   entry_finished : exception;

   --  For section numbering of large documents and standards
   type section_type is
      record
         first_level   : Integer := 0;
         second_level  : Integer := 0;
         third_level   : Integer := 0;
         fourth_level  : Integer := 0;
         fifth_level   : Integer := 0;
      end record;

   no_section : constant section_type := (0, 0, 0, 0, 0);

   type appendix_type is (none, a, b, c, d, e, f, g, h, i, j, k, l, m,
                          n, o, p, q, r, s, t, u, v, w, x, y, z);
   package appendix_io is new Ada.Text_IO.enumeration_io(appendix_type);

   type appendix_section_type is    record
      appendix : appendix_type := none;
      section  : section_type  := no_section;
   end record;

   no_appendix_section : constant appendix_section_type :=
     (none, (0, 0, 0, 0, 0));

   procedure Put(Output : Ada.Text_IO.File_Type; s : section_type) is
      level : Integer := 0;

      procedure Put_level(Output : Ada.Text_IO.File_Type; l : Integer) is
      begin
         if l > 9999  then
            Put(Output, "****");
         elsif l > 999  then
            Put(Output, l, 4);
         elsif l > 99  then
            Put(Output, l, 3);
         elsif l > 9  then
            Put(Output, l, 2);
         elsif l >= 0  then
            Put(Output, l, 1);
         else
            Put(Output, "**");
         end if;
      end Put_level;

   begin
      if s.fifth_level <= 0  then
         if s.fourth_level <= 0  then
            if s.third_level <= 0  then
               if s.second_level <= 0  then
                  level := 1;
               else
                  level := 2;
               end if;
            else
               level := 3;
            end if;
         else
            level := 4;
         end if;
      else
         level := 5;
      end if;

      if s.first_level <= 9  then
         Put(Output, ' ');
      end if;
      Put_level(Output, s.first_level);
      if level = 1  then
         Put(Output, '.');
         Put(Output, '0');           --  To match the ATLAS index convention
      end if;
      if level >= 2  then
         Put(Output, '.');
         Put_level(Output, s.second_level);
      end if;
      if level >= 3  then
         Put(Output, '.');
         Put_level(Output, s.third_level);
      end if;
      if level >= 4  then
         Put(Output, '.');
         Put_level(Output, s.fourth_level);
      end if;
      if level >= 5  then
         Put(Output, '.');
         Put_level(Output, s.fifth_level);
      end if;
   end Put;

   procedure Put(s : section_type) is
      level : Integer := 0;

      procedure Put_level(l : Integer) is
      begin
         if l > 9999  then
            Put("****");
         elsif l > 999  then
            Put(l, 4);
         elsif l > 99  then
            Put(l, 3);
         elsif l > 9  then
            Put(l, 2);
         elsif l >= 0  then
            Put(l, 1);
         else
            Put("**");
         end if;
      end Put_level;

   begin
      if s.fifth_level = 0  then
         if s.fourth_level = 0  then
            if s.third_level = 0  then
               if s.second_level = 0  then
                  level := 1;
               else
                  level := 2;
               end if;
            else
               level := 3;
            end if;
         else
            level := 4;
         end if;
      else
         level := 5;
      end if;

      if s.first_level <= 9  then
         Put(' ');
      end if;
      Put_level(s.first_level);
      Put('.');
      if level = 1  then
         Put('0');           --  To match the ATLAS index convention
      end if;
      if level >= 2  then
         Put_level(s.second_level);
      end if;
      if level >= 3  then
         Put('.');
         Put_level(s.third_level);
      end if;
      if level >= 4  then
         Put('.');
         Put_level(s.fourth_level);
      end if;
      if level >= 5  then
         Put('.');
         Put_level(s.fifth_level);
      end if;
   end Put;

   procedure Get(from : in String;
                 s : out section_type; last : out Integer) is
      l  : Integer := 0;
      ft : Integer := from'First;
      lt : Integer := from'Last;
   begin
      s := no_section;
      if Trim (from)'Last < from'First   then
         return;   --  Empty String, no data         --  Return default
      end if;

      Get(from, s.first_level, l);
      if l+1 >= lt  then
         last := l;
         return;
      end if;
      Get(from(l+2..lt), s.second_level, l);
      if l+1 >= lt  then
         last := l;
         return;
      end if;
      Get(from(l+2..lt), s.third_level, l);
      if l+1 >= lt  then
         last := l;
         return;
      end if;
      Get(from(l+2..lt), s.fourth_level, l);
      if l+1 >= lt  then
         last := l;
         return;
      end if;
      Get(from(l+2..lt), s.fifth_level, l);
      last := l;
      return;
   exception
      when Ada.Text_IO.end_error =>
         last := l;
         return;
      when Ada.Text_IO.Data_Error =>
         last := l;
         return;
      when others =>
         Put(" Unexpected exception in GET(FROM; SECTION_TYPE) with Input =>");
         Put(from);
         New_Line;
         last := l;
         raise;
   end Get;

   function "<"(a, b : section_type) return Boolean is
   begin

      if a.first_level > b.first_level  then
         return False;
      elsif a.first_level < b.first_level  then
         return True;
      else
         if a.second_level > b.second_level  then
            return False;
         elsif a.second_level < b.second_level  then
            return True;
         else
            if a.third_level > b.third_level  then
               return False;
            elsif a.third_level < b.third_level  then
               return True;
            else
               if a.fourth_level > b.fourth_level  then
                  return False;
               elsif a.fourth_level < b.fourth_level  then
                  return True;
               else
                  if a.fifth_level > b.fifth_level  then
                     return False;
                  elsif a.fifth_level < b.fifth_level  then
                     return True;
                  else
                     return False;
                  end if;
               end if;
            end if;
         end if;
      end if;

      return False;

   end "<";

   procedure Put(Output : Ada.Text_IO.File_Type; s : appendix_section_type) is
      use appendix_io;
   begin
      Put(Output, s.appendix);
      Put(Output, ' ');
      Put(Output, s.section);
   end Put;

   procedure Put(s : appendix_section_type) is
      use appendix_io;
   begin
      Put(s.appendix);
      Put(' ');
      Put(s.section);
   end Put;

   procedure Get(from : in String;
                 s : out appendix_section_type; last : out Integer) is
      use appendix_io;
      l  : Integer := 0;
      ft : Integer := from'First;
      lt : Integer := from'Last;
   begin

      s := no_appendix_section;
      if (ft = lt)  or else
        (Trim (from)'Length = 0)  then   --  Empty/blank String, no data
         Put("@");
         return;                      --  Return default
      end if;

      --PUT_LINE("In GET =>" & FROM & '|');

      begin
         Get(from, s.appendix, l);
         --PUT("A");
         if l+1 >= lt  then
            last := l;
            return;
         end if;
      exception
         when others  =>
            s.appendix := none;
            l := ft - 2;
      end;

      Get(from(l+2..lt), s.section, l);
      --PUT("F");
      return;
   exception
      when Ada.Text_IO.end_error =>
         last := l;
         return;
      when Ada.Text_IO.Data_Error =>
         last := l;
         return;
      when others =>
         Put
           (" Unexpected exception in GET(FROM; APPENDIX_SECTION_TYPE) with Input =>");
         Put(from);
         New_Line;
         last := l;
         return;
   end Get;

   function "<"(a, b : appendix_section_type) return Boolean is
   begin

      if a.appendix > b.appendix  then
         return False;
      elsif a.appendix < b.appendix  then
         return True;
      else
         if a.section.first_level > b.section.first_level  then
            return False;
         elsif a.section.first_level < b.section.first_level  then
            return True;
         else
            if a.section.second_level > b.section.second_level  then
               return False;
            elsif a.section.second_level < b.section.second_level  then
               return True;
            else
               if a.section.third_level > b.section.third_level  then
                  return False;
               elsif a.section.third_level < b.section.third_level  then
                  return True;
               else
                  if a.section.fourth_level > b.section.fourth_level  then
                     return False;
                  elsif a.section.fourth_level < b.section.fourth_level  then
                     return True;
                  else
                     if a.section.fifth_level > b.section.fifth_level  then
                        return False;
                     elsif a.section.fifth_level < b.section.fifth_level  then
                        return True;
                     else
                        return False;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end "<";

   procedure prompt_for_entry(entry_number : String) is
   begin
      Put("Give starting column and size of ");
      Put(entry_number);
      Put_Line(" significant sort field ");
      Put("  with optional sort type and way  => ");
   end prompt_for_entry;

   procedure Get_entry (mx, nx  : out Natural;
                        sx  : out sort_type;
                        wx  : out way_type ) is
      m : Natural := 1;
      n : Natural := line_length;
      s : sort_type := a;
      w : way_type := i;
      z : Natural := 0;

      procedure echo_entry is
      begin
         Put("                    Sorting on LINE("); Put(m,3);
         Put(".."); Put(n, 3); Put(")");
         Put("  with S = "); Put(s); Put(" and W = "); Put(w);
         New_Line(2);
      end echo_entry;

   begin

      m := 0;
      n := line_length;
      s := a;
      w := i;

      Get_Line(enter_line, ls);
      if ls = 0  then
         raise entry_finished;
      end if;
      Integer_IO.Get(enter_line(1..ls), m, last);
      begin
         Integer_IO.Get(enter_line(last+1..ls), z, last);
         if  m = 0 or z = 0  then
            Put_Line("Start or size of zero, you must be kidding, aborting");
            raise program_error;
         elsif m + z > line_length  then
            Put_Line("Size too large, going to end of line");
            n := line_length;
         else
            n := m + z - 1;
         end if;
         sort_type_io.Get(enter_line(last+1..ls), s, last);
         way_type_io.Get(enter_line(last+1..ls), w, last);
         mx := m; nx := n;  sx := s; wx := w;
         echo_entry;
         return;
      exception
         when program_error  =>
            Put_Line("PROGRAM_ERROR raised in GET_ENTRY");
            raise;
         when others =>
            mx := m; nx := n; sx := s; wx := w;
            echo_entry;
            return;
      end;
   end Get_entry;

   function ignore_separators(s : String) return String is
      t : String(s'First..s'Last) := Lower_Case (s);
   begin
      for i in s'First+1..s'Last-1  loop
         if (s(i-1) /= '-'  and then s(i-1) /= '_')  and then
           (s(i) = '-'  or else s(i) = '_')  and then
           (s(i+1) /= '-'  and then s(i+1) /= '_')  then
            t(i) := ' ';
         end if;
      end loop;
      return t;
   end ignore_separators;

   function ltu(c, d : Character) return Boolean is
   begin
      if (d = 'v')  then
         if (c < 'u')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'j')  then
         if (c < 'i')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'V')  then
         if (c < 'U')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'J')  then
         if (c < 'I')  then
            return True;
         else
            return False;
         end if;
      else
         return c < d;
      end if;
   end ltu;

   function equ(c, d : Character) return Boolean is
   begin
      if (d = 'u') or (d = 'v')  then
         if (c = 'u') or (c = 'v')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'i') or (d = 'j')  then
         if (c = 'i') or (c = 'j')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'U') or (d = 'V')  then
         if (c = 'U') or (c = 'V')  then
            return True;
         else
            return False;
         end if;
      elsif (d = 'I') or (d = 'J')  then
         if (c = 'I') or (c = 'J')  then
            return True;
         else
            return False;
         end if;
      else
         return c = d;
      end if;
   end equ;

   function gtu(c, d : Character) return Boolean is
   begin
      if d = 'u'  then
         if (c > 'v')  then
            return True;
         else
            return False;
         end if;
      elsif d = 'i'  then
         if (c > 'j')  then
            return True;
         else
            return False;
         end if;
      elsif d = 'U'  then
         if (c > 'V')  then
            return True;
         else
            return False;
         end if;
      elsif d = 'I'  then
         if (c > 'J')  then
            return True;
         else
            return False;
         end if;
      else
         return c > d;
      end if;
   end gtu;

   function ltu(s, t : String) return Boolean is
   begin
      for i in 1..s'Length  loop   --  Not TRIMed, so same length
         if equ(s(s'First+i-1), t(t'First+i-1))  then
            null;
         elsif gtu(s(s'First+i-1), t(t'First+i-1))  then
            return False;
         elsif ltu(s(s'First+i-1), t(t'First+i-1))  then
            return True;
         end if;
      end loop;
      return False;
   end ltu;

   function gtu(s, t : String) return Boolean is
   begin
      for i in 1..s'Length  loop
         if equ(s(s'First+i-1), t(t'First+i-1))  then
            null;
         elsif ltu(s(s'First+i-1), t(t'First+i-1))  then
            return False;
         elsif gtu(s(s'First+i-1), t(t'First+i-1))  then
            return True;
         end if;
      end loop;
      return False;
   end gtu;

   function equ(s, t : String) return Boolean is
   begin
      if s'Length /= t'Length  then
         return False;
      end if;

      for i in 1..s'Length  loop
         if not equ(s(s'First+i-1), t(t'First+i-1))  then
            return False;
         end if;
      end loop;

      return True;
   end equ;

   function slt (x, y : String;         --  Make LEFT and RIGHT
                 st : sort_type := a;
                 wt : way_type := i) return Boolean is
      as : String(x'Range) := x;
      bs : String(y'Range) := y;
      mn, nn : Integer := 0;
      fn, gn : float := 0.0;
      --FS, GS : SECTION_TYPE := NO_SECTION;
      fs, gs : appendix_section_type := no_appendix_section;
      px, py : Part_Entry;       --  So I can X here
      rx, ry : Part_Of_Speech_Type;   --  So I can X here
   begin
      if st = a  then
         as := Lower_Case (as);
         bs := Lower_Case (bs);
         if wt = i  then
            return as < bs;
         else
            return as > bs;
         end if;

      elsif st = c  then
         if wt = i  then
            return as < bs;
         else
            return as > bs;
         end if;

      elsif st = g  then
         as := ignore_separators(as);
         bs := ignore_separators(bs);
         if wt = i  then
            return as < bs;
         else
            return as > bs;
         end if;

      elsif st = u  then
         as := Lower_Case (as);
         bs := Lower_Case (bs);
         if wt = i  then
            return ltu(as, bs);
         else
            return gtu(as, bs);
         end if;

      elsif st = n  then
         Integer_IO.Get(as, mn, last);
         Integer_IO.Get(bs, nn, last);
         if wt = i  then
            return mn < nn;
         else
            return mn > nn;
         end if;

      elsif st = f  then
         float_io.Get(as, fn, last);
         float_io.Get(bs, gn, last);
         if wt = i  then
            return fn < gn;
         else
            return fn > gn;
         end if;

      elsif st = p  then
         Part_Entry_IO.Get(as, px, last);
         Part_Entry_IO.Get(bs, py, last);
         if wt = i  then
            return px < py;
         else
            return (not (px < py)) and (not (px = py));
         end if;

      elsif st = r  then
         Part_Of_Speech_Type_IO.Get(as, rx, last);
         Part_Of_Speech_Type_IO.Get(bs, ry, last);
         if wt = i  then
            return rx < ry;
         else
            return (not (rx < ry)) and (not (rx = ry));
         end if;

      elsif st = s  then
         --PUT_LINE("AS =>" & AS & '|');
         Get(as, fs, last);
         --PUT_LINE("BS =>" & BS & '|');
         Get(bs, gs, last);
         --PUT_LINE("GOT AS & BS");
         if wt = i  then
            return fs < gs;
         else
            return (not (fs < gs)) and (not (fs = gs));
         end if;

      else
         return False;
      end if;

   exception
      when others  =>
         Ada.Text_IO.Put_Line("exception in SLT    showing LEFT and RIGHT");
         Ada.Text_IO.Put_Line(x & "&");
         Ada.Text_IO.Put_Line(y & "|");
         raise;

   end slt;

   function sort_equal (x, y : String;
                        st : sort_type := a;
                        wt : way_type := i) return Boolean is
      as : String(x'Range) := x;
      bs : String(y'Range) := y;
      mn, nn : Integer := 0;
      fn, gn : float := 0.0;
      fs, gs : appendix_section_type := no_appendix_section;
      px, py : Part_Entry;
      rx, ry : Part_Of_Speech_Type;
   begin
      if st = a  then
         as := Lower_Case (as);
         bs := Lower_Case (bs);
         return as = bs;

      elsif st = c  then
         return as = bs;

      elsif st = g  then
         as := ignore_separators(as);
         bs := ignore_separators(bs);
         return as = bs;

      elsif st = u  then
         as := Lower_Case (as);
         bs := Lower_Case (bs);
         return equ(as,  bs);

      elsif st = n  then
         Integer_IO.Get(as, mn, last);
         Integer_IO.Get(bs, nn, last);
         return mn = nn;

      elsif st = f  then
         float_io.Get(as, fn, last);
         float_io.Get(bs, gn, last);
         return fn = gn;

      elsif st = p  then
         Part_Entry_IO.Get(as, px, last);
         Part_Entry_IO.Get(bs, py, last);
         return px = py;

      elsif st = r  then
         Part_Of_Speech_Type_IO.Get(as, rx, last);
         Part_Of_Speech_Type_IO.Get(bs, ry, last);
         return rx = ry;

      elsif st = s  then
         Get(as, fs, last);
         Get(bs, gs, last);
         return fs = gs;

      else
         return False;
      end if;

   exception
      when others  =>
         Ada.Text_IO.Put_Line("exception in LT    showing LEFT and RIGHT");
         Ada.Text_IO.Put_Line(x & "|");
         Ada.Text_IO.Put_Line(y & "|");
         raise;

   end sort_equal;

   function lt  (left, right : text_type) return Boolean is
   begin

      if slt(left(m1..n1),  right(m1..n1), s1, w1)  then
         return True;

      elsif sort_equal(left(m1..n1),  right(m1..n1), s1, w1) then
         if ((n2 > 0) and then
               slt(left(m2..n2),  right(m2..n2), s2, w2) ) then
            return True;

         elsif ((n2 > 0) and then
                  sort_equal(left(m2..n2),  right(m2..n2), s2, w2)) then
            if ((n3 > 0) and then
                  slt(left(m3..n3),  right(m3..n3), s3, w3 ))  then
               return True;

            elsif ((n3 > 0) and then
                     sort_equal(left(m3..n3),  right(m3..n3), s3, w3))  then
               if ((n4 > 0) and then
                     slt(left(m4..n4),  right(m4..n4), s4, w4) )  then
                  return True;

               elsif ((n4 > 0) and then
                        sort_equal(left(m4..n4),  right(m4..n4), s4, w4))  then
                  if ((n5 > 0) and then
                        slt(left(m5..n5),  right(m5..n5), s5, w5) )  then
                     return True;

                  end if;
               end if;
            end if;
         end if;
      end if;
      return False;
   exception
      when others =>
         Ada.Text_IO.Put_Line("exception in LT    showing LEFT and RIGHT");
         Ada.Text_IO.Put_Line(left & "|");
         Ada.Text_IO.Put_Line(right & "|");
         raise;
   end lt;

   procedure Open_file_for_Input(Input : in out Ada.Text_IO.File_Type;
                                 prompt : String := "File for Input => ") is
      last : Natural := 0;
   begin
  Get_Input_file:
      loop
     check_Input:
         begin
            New_Line;

            Put(prompt);
            Get_Line(Input_name, last);
            Open(Input, In_File, Input_name(1..last));
            exit;
         exception
            when others  =>
               Put_Line("   !!!!!!!!!  Try Again  !!!!!!!!");
         end check_Input;
      end loop Get_Input_file;

   end Open_file_for_Input;

   procedure Create_file_for_Output(Output : in out Ada.Text_IO.File_Type;
                                    prompt : String := "File for Output => ") is
      name : String(1..80) := (others => ' ');
      last : Natural := 0;
   begin

  Get_Output_file:
      loop
     check_Output:
         begin
            New_Line;

            Put(prompt);
            Get_Line(name, last);
            if Trim (name(1..last))'Length /= 0  then
               Create(Output, Out_File, name(1..last));
            else
               Create(Output, Out_File, Trim (Input_name));
            end if;
            exit;
         exception
            when others  =>
               Put_Line("   !!!!!!!!!  Try Again  !!!!!!!!");
         end check_Output;
      end loop Get_Output_file;

   end Create_file_for_Output;

   function graphic(s : String) return String is
      t : String(1..s'Length) := s;
   begin
      for i in s'Range  loop
         if Character'pos(s(i)) < 32  then
            t(i) := ' ';
         end if;
      end loop;
      return t;
   end graphic;

begin

   New_Line;
   Put_Line("Sorts a text file of lines four times on subStrings M..N");
   Put_Line("A)lphabetic (all case) C)ase sensitive, iG)nore seperators, U)i_is_vj,");
   Put_Line("    iN)teger, F)loating point, S)ection, P)art entry, or paR)t of speech");
   Put_Line("         I)ncreasing or D)ecreasing");
   New_Line;

   Open_file_for_Input(Input, "What file to sort from => ");
   New_Line;

   prompt_for_entry("first");
   begin
      Get_entry(m1, n1, s1, w1);
   exception
      when program_error  =>
         raise;
      when others =>
         null;
   end;

   begin
      prompt_for_entry("second");
      Get_entry(m2, n2, s2, w2);
      prompt_for_entry("third");
      Get_entry(m3, n3, s3, w3);
      prompt_for_entry("fourth");
      Get_entry(m4, n4, s4, w4);
      prompt_for_entry("fifth");
      Get_entry(m5, n5, s5, w5);
   exception
      when program_error  =>
         raise;
      when entry_finished =>
         null;
      when Ada.Text_IO.Data_Error  | Ada.Text_IO.end_error  =>
         null;
   end;

   --PUT_LINE("CREATING WORK FILE");
   New_Line;
   Create (work, inOut_File, "WORK.");
   Put_Line("CREATED  WORK FILE");

   while not End_Of_File(Input)  loop
      --begin
      Get_Line(Input, line_text, current_length);
      --exception when others  =>
      --TEXT_IO.PUT_LINE("INPUT GET exception");
      --TEXT_IO.PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH) & "|");
      --end;
      --PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH));
      --PUT_LINE("=>" & HEAD(LINE_TEXT(1..CURRENT_LENGTH), LINE_LENGTH) & "|");
      if Trim (line_text(1..current_length)) /= ""  then
         --begin
         Write(work, Head(line_text(1..current_length), line_length)  );
         --exception when others  =>
         --TEXT_IO.PUT_LINE("WORK WRITE exception");
         --TEXT_IO.PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH) & "|");
         --end;
      end if;
   end loop;
   Close(Input);

   Put_Line("Begin sorting");

line_heapsort:
    declare

       l    : line_io.Positive_Count := size(work) / 2 + 1;
       ir   : line_io.Positive_Count := size(work);
       i, j : line_io.Positive_Count;

    begin
       Ada.Text_IO.Put_Line("SIZE OF WORK = " & Integer'Image(Integer(size(work))));
   main:
       loop

          if l > 1  then
             l := l - 1;
             Read(work, line_text, l);
             old_line := line_text;
          else
             Read(work, line_text, ir);
             old_line := line_text;
             Read(work, line_text, 1);
             Write(work, line_text, ir);
             ir := ir - 1;
             if ir = 1 then
                Write(work, old_line, 1);
                exit main;
             end if;
          end if;
          i := l;
          j := l + l;

          while j <= ir   loop
             if j < ir  then
                Read(work, line_text, j);
                Read(work, p_line, j+1);
                --if LT (LINE.TEXT, P_LINE.TEXT)  then
                if lt (line_text, p_line)  then
                   j := j + 1;
                end if;
             end if;
             Read(work, line_text, j);
             --if OLD_LINE.TEXT < LINE.TEXT  then
             if lt (old_line , line_text)  then
                Write(work, line_text, i);
                i := j;
                j := j + j;
             else
                j := ir + 1;
             end if;
          end loop;
          Write(work, old_line, i);

       end loop main;

    exception
       when Constraint_Error => Put_Line("HEAP CONSTRAINT_ERROR");
       when others           => Put_Line("HEAP other_ERROR");
    end line_heapsort;

    Put_Line("Finished sorting in WORK");

    Create_file_for_Output(Output, "Where to Put the Output => ");

    --RESET(WORK);
    Set_Index(work, 1);
    while not End_Of_File(work)  loop
       Read(work, line_text);
       if Trim (graphic(line_text))'Length > 0  then
          --PUT_LINE(TRIM(LINE_TEXT, RIGHT));
          Put_Line(Output, Trim (line_text, right));
       end if;
    end loop;

    Close(work);
    Close(Output);
    Put_Line("Done!");
    New_Line;

exception
   when program_error  =>
      Put_Line("SORT terminated on a PROGRAM_ERROR");
      Close(Output);
   when Ada.Text_IO.Data_Error =>     --Terminate on primary start or size = 0
      Put_Line("SORT terminated on a DATA_ERROR");
      Put_Line(line_text);
      Close(Output);
   when Constraint_Error =>       --Terminate on blank line for file name
      Put_Line("SORT terminated on a CONSTRAINT_ERROR");
      Close(Output);
   when Ada.Text_IO.device_error  =>     --Ran out of space to Write Output file
      Put_Line("SORT terminated on a DEVICE_ERROR");
      delete(Output);
      Create_file_for_Output(Output, "Wherelse to Put the Output => ");
      reset(work);
      while not End_Of_File(work)  loop
         Read(work, line_text);
         Put_Line(Output, line_text);    --(1..LINE.CURRENT_LENGTH));
      end loop;
      Close(Output);
end sorter;
