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

with text_io;
package strings_package is
   type trim_end is (left, right, both);

   null_string : constant string(2..1) := (others => ' ');

   function max(a, b : integer) return integer;
   function min(a, b : integer) return integer;

   function lower_case(c : character) return character;
   function lower_case(s : string) return string;

   function upper_case(c : character) return character;
   function upper_case(s : string) return string;

   function trim(source : in string;
                 side   : in trim_end := both) return string;
   --  Equivalent to Ada.Strings.Fixed.Trim(Source, Both);

   function head(source : in string;
                 count  : in natural) return string;

   procedure get_non_comment_line(f : in text_io.file_type;
                                  s : out string; last : out integer);

end strings_package;
