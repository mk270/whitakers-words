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

with ada.text_io;
procedure slash is
   package Integer_IO is new ada.text_io.Integer_IO (Integer);
   use ada.text_io;
   use Integer_IO;

   f1, f2, f3  : File_Type;
   f           : String (1 .. 100);
   s           : String (1 .. 2500);
   bs          : String (1 .. 2500) := (others => ' ');
   n           : Integer := 0;
   l           : Integer := 0;
   ls          : Integer := 0;

   type reply_type is (columns, lines);
   reply : reply_type;
   reply_character : character;

   function which (r : character) return reply_type is
   begin
      case r is
         when 'C' | 'c'  =>  return columns;
         when 'L' | 'l'  =>  return lines;
         when others     =>
            raise data_error;
      end case;
   end which;

begin
   Put_Line ("Breaks a file into two, by row or column.");

   put ("What file to SLASH from =>");
   Get_Line (f, l);
   put ("=> ");
   Open (f1, In_File, f (1 .. l));
   Put_Line ("Opened input file");

   put ("Do you wish to SLASH C)olumns or L)ines? =>");
   Get (reply_character);
   skip_line;
   reply := which (reply_character);
   New_Line;

   put ("How many lines/columns to leave after SLASHing =>");
   Get (n);
   skip_line;
   New_Line;

   put ("Where to put the first  =>");
   Get_Line (f, l);
   put ("=> ");
   Create (f2, Out_File, f (1 .. l));
   Put_Line ("Created SLASH file first");

   put ("Where to put the rest  =>");
   Get_Line (f, l);
   put ("=> ");
   Create (f3, Out_File, f (1 .. l));
   Put_Line ("Created SLASH file rest");

   if reply = columns  then

      while not End_Of_File (f1) loop
         s := bs;
         Get_Line (f1, s, ls);
         if ls <= n then            --  Line shorter than break
            Put_Line (f2, s (1 .. ls));
            Put_Line (f3, "");       --  Put a blank line so there will be a line
         else                       --  Line runs past break
            Put_Line (f2, s (1 .. n));
            Put_Line (f3, s (n + 1 .. ls));
         end if;
      end loop;
      Close (f2);
      Close (f3);

   elsif reply = lines  then

  first:
      begin
         for i in 1 .. n loop
            Get_Line (f1, s, ls);
            Put_Line (f2, s (1 .. ls));
         end loop;
      exception
         when end_error  =>
            null;
      end first;
      Close (f2);

  second:
      begin
         loop
            Get_Line (f1, s, ls);
            Put_Line (f3, s (1 .. ls));
         end loop;
      exception
         when end_error  =>
            null;
      end second;
      Close (f3);

   end if;

   Put_Line ("Done SLASHing");

exception
   when data_error  =>
      Put_Line ("***************** WRONG REPLY *****************");
      New_Line (2);
      Put_Line ("Try again");
   when others      =>
      New_Line (2);
      Put_Line ("Unexpected exception raised in SLASH  *********");
end slash;
