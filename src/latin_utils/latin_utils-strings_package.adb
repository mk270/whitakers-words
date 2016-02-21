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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
package body Latin_Utils.Strings_Package is

   ---------------------------------------------------------------------------

   function Lower_Case (C : Character) return Character
      renames Ada.Characters.Handling.To_Lower;

   function Lower_Case (S : String) return String
      renames Ada.Characters.Handling.To_Lower;

   function Upper_Case (C : Character) return Character
      renames Ada.Characters.Handling.To_Upper;

   function Upper_Case (S : String) return String
      renames Ada.Characters.Handling.To_Upper;

   ---------------------------------------------------------------------------

   function Trim
      (Source : in String;
       Side   : in Trim_End := Both
      ) return String
   is
   begin
      return Ada.Strings.Fixed.Trim (Source, Ada.Strings.Trim_End (Side));
   end Trim;

   ---------------------------------------------------------------------------

   function Head
      (Source : in String;
       Count  : in Natural
      ) return String is
   begin
      return Ada.Strings.Fixed.Head (Source, Count, ' ');
   end Head;

   ---------------------------------------------------------------------------

   procedure Get_Non_Comment_Line
      (File : in  Ada.Text_IO.File_Type;
       Item : out String;
       Last : out Integer
      ) is
      Line  : String (1 .. 250) := (others => ' ');
      Length, LX : Integer := 0;
      -- LX is Line (Line'First .. Start_Of_Comment)'Length
   begin
      Last := 0;

      --  Loop until data - Finish on EOF
      File_Loop :
      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line, Length);

         declare
            Trimmed_Head : constant String := Head (Trim (Line), 250)(1 .. 2);
         begin
            if Trimmed_Head (1) = Character'Val (13) then
               exit File_Loop;
            end if;

            if Trimmed_Head = "--" then
               null;
            else
               -- Search for start of comment in line (if any).
               LX := Ada.Strings.Fixed.Index (Line, "--", Line'First);
               if LX /= 0 then
                  LX := LX - 1;
               else
                  LX := Length;
               end if;

               exit File_Loop;
            end if;
         end;
      end loop File_Loop;

      Item (Item'First .. LX) := Line (1 .. LX);
      Last := LX;
   end Get_Non_Comment_Line;

   ---------------------------------------------------------------------------

end Latin_Utils.Strings_Package;
