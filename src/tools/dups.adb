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
--with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
procedure dups is
   package Integer_Text_IO is new Text_IO.Integer_IO (Integer);
   use Integer_Text_IO;
   use Text_IO;

   input, output : File_Type;
   s, blank_line : constant String (1 .. 400) := (others => ' ');
   line, oldline : String (1 .. 400) := (others => ' ');
   last : Integer := 0;
   mx, nx : Natural := 0;

   line_number : Integer := 0;
   number : Integer := 0;

   procedure get_entry (mx, nx  : out Natural) is
      ls : Integer := 0;
      enter_line : String (1 .. 20);

   begin

      Get_Line (enter_line, ls);
      Get (enter_line (1 .. ls), mx, last);
      Get (enter_line (last + 1 .. ls), nx, last);

   end get_entry;

begin
   Put_Line ("DUPS.IN -> DUPS.OUT    For sorted files");
   Put_Line ("DUPS  checks for columns MX .. NX being duplicates");
   get_entry (mx, nx);

   Create (output, Out_File, "DUPS.OUT");
   Open (input, In_File, "DUPS.IN");

   while not End_Of_File (input) loop
      oldline := line;
      line := blank_line;
      Get_Line (input, line, last);
      line_number := line_number + 1;
      if line (mx .. nx) = oldline (mx .. nx)  and then
        (line (111) /= '|') then
         number := number + 1;
         Put (output, line_number); Put (output, "  ");
         Put_Line (output, line (1 .. nx));
      end if;
   end loop;

   Close (output);

   New_Line;
   Put ("Number of entries = "); Put (line_number); New_Line;
   Put ("Number of DUPS    = "); Put (number); New_Line;
   Put ("Ratio             = 1 :"); Put (line_number / number); New_Line;

exception
   when Name_Error  =>
      Put_Line ("No file to process");
      Close (output);

   when others =>
      Put ("Exception on LINE"); Put (line_number); New_Line;
      Put_Line (s (1 .. last));
      Close (output);

end dups;
