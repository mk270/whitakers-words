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

with Text_IO;
package Strings_package is
   type trim_end is (left, right, both);

   null_String : constant String(2..1) := (others => ' ');

   function max(a, b : Integer) return Integer;
   function min(a, b : Integer) return Integer;

   function lower_case(c : Character) return Character;
   function lower_case(s : String) return String;

   function upper_case(c : Character) return Character;
   function upper_case(s : String) return String;

   function trim(source : in String;
                 side   : in trim_end := both) return String;
   --  Equivalent to Ada.Strings.Fixed.Trim(Source, Both);

   function head(source : in String;
                 count  : in Natural) return String;

   procedure Get_non_comment_line(f : in Text_IO.File_Type;
                                  s : out String; last : out Integer);

end Strings_package;
