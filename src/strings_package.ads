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
				 count  : in natural;
				 pad    : in character := ' ') return string;

   procedure get_non_comment_line(f : in text_io.file_type;
								  s : out string; last : out integer);

end strings_package;
