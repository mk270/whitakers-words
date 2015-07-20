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
with Strings_package; use Strings_package;
procedure invert is
   line, parm : String (1 .. 250);
   l, last : Integer;
   n1, n2 : Integer;

   input, output : File_Type;
   package Integer_IO is new Text_IO.Integer_IO (Integer);

   function invert (s : String) return String is
      t : String (1 .. s'length);
   begin
      for i in 1 .. t'length  loop
         t (i) := s (s'last-i + 1);
      end loop;
      return Head (Trim (t), s'length);

   end invert;

begin
   Put_Line ("Inverts/reverses the order of columns N1 .. N2 of INVERT.IN -> INVERT.OUT");
   put ("Give an N1 and N2 => ");
   Get_Line (parm, last);

   Integer_IO.Get (parm (1 .. last), n1, l);
   Integer_IO.Get (parm (l + 1 .. last), n2, l);

   Create (output, Out_File, "INVERT.OUT");
   Open (input, In_File, "INVERT.IN");

   while not End_Of_File (input)  loop
      Get_Line (input, line, last);

      line (n1 .. n2)  := invert (line (n1 .. n2));
      put ('.');
      Put_Line (output, line (1 .. last));

   end loop;

   Close (output);

exception
   when others  =>
      Close (output);

end invert;
