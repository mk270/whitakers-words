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

with text_io; use text_io;
package body strings_package is

   function max(a, b : integer) return integer is
   begin
      if a >= b  then
         return a; end if;
         return b;
   end max;

   function min(a, b : integer) return integer is
   begin
      if a <= b  then
         return a; end if;
         return b;
   end min;

   function lower_case(c : character) return character is
   begin
      if c in 'A'..'Z'  then
         return character'val(character'pos(c) + 32);
      else
         return c;
      end if;
   end lower_case;

   function lower_case(s : string) return string is
      t : string(s'range);
   begin
      for i in s'range  loop
         t(i) := lower_case(s(i));
      end loop;
      return t;
   end lower_case;

   function upper_case(c : character) return character is
   begin
      if c in 'a'..'z'  then
         return character'val(character'pos(c) - 32);
      else
         return c;
      end if;
   end upper_case;

   function upper_case(s : string) return string is
      t : string(s'range);
   begin
      for i in s'range  loop
         t(i) := upper_case(s(i));
      end loop;
      return t;
   end upper_case;

   function trim(source : in string;
                 side   : in trim_end := both) return string is
      --  Removes leading and trailing blanks and returns a STRING staring at 1
      --  For a string of all blanks as input it returns NULL_STRING
      t : string(1..source'length) := source;
      first: natural := source'first;
      last : natural := source'last;

   begin
      if side /= right  then
         first := source'last + 1;
         for i in source'range  loop
            if source(i) /= ' '  then
               first := i;
               exit;
            end if;
         end loop;
      else
         first := source'first;
      end if;

      if side /= left  then
         last := source'first - 1;
         for i in reverse source'range  loop
            if source(i) /= ' '  then
               last := i;
               exit;
            end if;
         end loop;
      else
         last := source'last;
      end if;

      if first > last  then
         return null_string;
      else
         t(1..last-first+1) := source(first..last);
         return t(1..last-first+1);
      end if;
   end trim;

   function head(source : in string;
                 count  : in natural) return string is
      --  Truncates or fills a string to exactly N in length
      t : string(1..count) := (others => ' ');
   begin
      if count < source'length  then
         t(1..count) := source(source'first..source'first+count-1);
      else
         t(1..source'length) := source(source'first..source'last);
      end if;
      return t;
   end head;

   procedure get_non_comment_line(f : in text_io.file_type;
                                  s : out string; last : out integer) is
      --  Reads a text file and outs a string that is as much of the
      --  first line encountered that is not a comment, that is not a comment

      t : string(1..250) := (others => ' ');
      l, lx : integer := 0;
   begin
      last := 0;
  file_loop:
      while not text_io.end_of_file(f)  loop  --  Loop until data - Finish on EOF
         text_io.get_line(f, t, l);
         if (head(trim(t), 250)(1..2) = "  "  or
               head(trim(t), 250)(1..2) = "--")  then
            null;
         else
            lx := l;
        line_loop:
            for i in 2..l  loop
               --  Any leading comment does not get to here
               if (t(i-1) = '-')  and  (t(i) = '-')  then   --  We have a comment
                  lx := i - 2;
                  exit file_loop;
               end if;
            end loop line_loop;
            exit file_loop;
         end if;
      end loop file_loop;
      s(s'first..lx) := t(1..lx);
      last := lx;
   end get_non_comment_line;

end strings_package;
