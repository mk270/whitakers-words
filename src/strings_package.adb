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

with Text_IO; use Text_IO;
package body Strings_package is

   function max(a, b : Integer) return Integer is
   begin
      if a >= b then
         return a;
      end if;
         return b;
   end max;

   function min(a, b : Integer) return Integer is
   begin
      if a <= b then
         return a;
      end if;
         return b;
   end min;

   function lower_case(c : Character) return Character is
   begin
      if c in 'A'..'Z'  then
         return Character'Val(Character'Pos(c) + 32);
      else
         return c;
      end if;
   end lower_case;

   function lower_case(s : String) return String is
      t : String(s'Range);
   begin
      for i in s'Range  loop
         t(i) := lower_case(s(i));
      end loop;
      return t;
   end lower_case;

   function upper_case(c : Character) return Character is
   begin
      if c in 'a'..'z'  then
         return Character'Val(Character'Pos(c) - 32);
      else
         return c;
      end if;
   end upper_case;

   function upper_case(s : String) return String is
      t : String(s'Range);
   begin
      for i in s'Range  loop
         t(i) := upper_case(s(i));
      end loop;
      return t;
   end upper_case;

   function trim(source : in String;
                 side   : in trim_end := both) return String is
      --  Removes leading and trailing blanks and returns a STRING staring at 1
      --  For a String of all blanks as Input it returns NULL_STRING
      t : String(1..source'Length) := source;
      first: Natural := source'First;
      last : Natural := source'Last;

   begin
      if side /= right  then
         first := source'Last + 1;
         for i in source'Range  loop
            if source(i) /= ' '  then
               first := i;
               exit;
            end if;
         end loop;
      else
         first := source'First;
      end if;

      if side /= left  then
         last := source'First - 1;
         for i in reverse source'Range  loop
            if source(i) /= ' '  then
               last := i;
               exit;
            end if;
         end loop;
      else
         last := source'Last;
      end if;

      if first > last  then
         return null_String;
      else
         t(1..last-first+1) := source(first..last);
         return t(1..last-first+1);
      end if;
   end trim;

   function head(source : in String;
                 count  : in Natural) return String is
      --  Truncates or fills a String to exactly N in Length
      t : String(1..count) := (others => ' ');
   begin
      if count < source'Length  then
         t(1..count) := source(source'First..source'First+count-1);
      else
         t(1..source'Length) := source(source'First..source'Last);
      end if;
      return t;
   end head;

   procedure Get_non_comment_line(f : in Text_IO.File_Type;
                                  s : out String; last : out Integer) is
      --  Reads a text file and outs a String that is as much of the
      --  first line encountered that is not a comment, that is not a comment

      t : String(1..250) := (others => ' ');
      l, lx : Integer := 0;
   begin
      last := 0;
      file_loop:
      while not Text_IO.End_Of_File(f)  loop  --  Loop until data - Finish on EOF
         Text_IO.Get_Line(f, t, l);
         if head(trim(t), 250)(1..2) = "  "  or
               head(trim(t), 250)(1..2) = "--"
         then
            null;
         else
            lx := l;
            line_loop:
            for i in 2..l  loop
               --  Any leading comment does not Get to here
               if (t(i-1) = '-')  and  (t(i) = '-')  then   --  We have a comment
                  lx := i - 2;
                  exit file_loop;
               end if;
            end loop line_loop;
            exit file_loop;
         end if;
      end loop file_loop;
      s(s'First..lx) := t(1..lx);
      last := lx;
   end Get_non_comment_line;
end Strings_package;
