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

with config;
package body preface is

   procedure put(s : string) is
   begin
      if not config.suppress_preface  then
         text_io.put(text_io.current_output, s);
      end if;
   end put;

   procedure set_col(pc : text_io.positive_count) is
   begin
      if not config.suppress_preface  then
         text_io.set_col(text_io.current_output, pc);
      end if;
   end set_col;

   procedure put_line(s : string) is
   begin
      if not config.suppress_preface  then
         text_io.put_line(text_io.current_output, s);
      end if;
   end put_line;

   procedure new_line(spacing  : text_io.positive_count := 1) is
   begin
      if not config.suppress_preface  then
         text_io.new_line(text_io.current_output, spacing);
      end if;
   end new_line;

   procedure put(n : integer; width : text_io.field := integer'width) is
      package integer_io is new text_io.integer_io(integer);
   begin
      if not config.suppress_preface  then
         integer_io.put(text_io.current_output, n, width);
      end if;
   end put;

end preface;
