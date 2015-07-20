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
with Strings_package; use Strings_package;
procedure dups is
   package Integer_text_io is new text_io.Integer_IO (Integer);
   use Integer_text_io;
   use text_io;

   input, output : File_Type;
   s, line, oldline, blank_line : String (1 .. 400) := (others => ' ');
   j, l, ll, last : Integer := 0;
   mx, nx : natural := 0;

   line_number : Integer := 0;
   number : Integer := 0;

   procedure get_entry (mx, nx  : out natural) is
      z : natural := 0;
      ls : Integer := 0;
      enter_line : String (1 .. 20);

      procedure prompt_for_entry (entry_number : String) is
      begin
         put ("Give starting and ending column ");
      end prompt_for_entry;

   begin

      Get_Line (enter_line, ls);
      Get (enter_line (1 .. ls), mx, last);
      Get (enter_line (last+1 .. ls), nx, last);

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
         number := number + 1 ;
         put (output, line_number); put (output, "  ");
         Put_Line (output, line (1 .. nx));
      end if;
   end loop;

   Close (output);

   New_Line;
   put ("Number of entries = "); put (line_number); New_Line;
   put ("Number of DUPS    = "); put (number); New_Line;
   put ("Ratio             = 1 :"); put (line_number/number); New_Line;

exception
   when name_error  =>
      Put_Line ("No file to process");
      Close (output);

   when others =>
      put ("Exception on LINE"); put (line_number); New_Line;
      Put_Line (s (1 .. last));
      Close (output);

end dups;
