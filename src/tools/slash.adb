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

with Ada.Integer_Text_IO;
with Ada.Text_IO;
procedure Slash is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;

   F1, F2, F3  : File_Type;
   F           : String (1 .. 100);
   S           : String (1 .. 2500);
   Bs          : constant String (1 .. 2500) := (others => ' ');
   N           : Integer := 0;
   L           : Integer := 0;
   Ls          : Integer := 0;

   type Reply_Type is (Columns, Lines);
   Reply : Reply_Type;
   Reply_Character : Character;

   function Which (R : Character) return Reply_Type is
   begin
      case R is
         when 'C' | 'c'  =>  return Columns;
         when 'L' | 'l'  =>  return Lines;
         when others     =>
            raise Data_Error;
      end case;
   end Which;

begin
   Put_Line ("Breaks a file into two, by row or column.");

   Put ("What file to SLASH from =>");
   Get_Line (F, L);
   Put ("=> ");
   Open (F1, In_File, F (1 .. L));
   Put_Line ("Opened input file");

   Put ("Do you wish to SLASH C)olumns or L)ines? =>");
   Get (Reply_Character);
   Skip_Line;
   Reply := Which (Reply_Character);
   New_Line;

   Put ("How many lines/columns to leave after SLASHing =>");
   Get (N);
   Skip_Line;
   New_Line;

   Put ("Where to put the first  =>");
   Get_Line (F, L);
   Put ("=> ");
   Create (F2, Out_File, F (1 .. L));
   Put_Line ("Created SLASH file first");

   Put ("Where to put the rest  =>");
   Get_Line (F, L);
   Put ("=> ");
   Create (F3, Out_File, F (1 .. L));
   Put_Line ("Created SLASH file rest");

   if Reply = Columns  then

      while not End_Of_File (F1) loop
         S := Bs;
         Get_Line (F1, S, Ls);
         if Ls <= N then            --  Line shorter than break
            Put_Line (F2, S (1 .. Ls));
            Put_Line (F3, "");      --  Put a blank line so there will be a line
         else                       --  Line runs past break
            Put_Line (F2, S (1 .. N));
            Put_Line (F3, S (N + 1 .. Ls));
         end if;
      end loop;
      Close (F2);
      Close (F3);

   elsif Reply = Lines  then

      First :
      begin
         for I in 1 .. N loop
            Get_Line (F1, S, Ls);
            Put_Line (F2, S (1 .. Ls));
         end loop;
      exception
         when End_Error  =>
            null;
      end First;
      Close (F2);

      Second :
      begin
         loop
            Get_Line (F1, S, Ls);
            Put_Line (F3, S (1 .. Ls));
         end loop;
      exception
         when End_Error  =>
            null;
      end Second;
      Close (F3);

   end if;

   Put_Line ("Done SLASHing");

exception
   when Data_Error  =>
      Put_Line ("***************** WRONG REPLY *****************");
      New_Line (2);
      Put_Line ("Try again");
   when others      =>
      New_Line (2);
      Put_Line ("Unexpected exception raised in SLASH  *********");
end Slash;
