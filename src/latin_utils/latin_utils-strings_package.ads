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

   ---------------------------------------------------------------------------
   -- This package contains basic subprograms operating on Strings and
   -- Characters.
   ---------------------------------------------------------------------------

   -- TODO: Write testbenches for every subprogram in this package.

with Ada.Strings;
with Ada.Text_IO;
package Latin_Utils.Strings_Package is

   ---------------------------------------------------------------------------
   -- Correct values: Left, Right, Both
   type Trim_End is new Ada.Strings.Trim_End;

   Null_String : constant String (2 .. 1) := (others => ' ');

   ---------------------------------------------------------------------------

   function Lower_Case (C : Character) return Character;
   function Lower_Case (S : String) return String;

   function Upper_Case (C : Character) return Character;
   function Upper_Case (S : String) return String;

   ---------------------------------------------------------------------------
   --  Removes leading and/or trailing blanks and returns a STRING starting at 1
   --  For a String of all blanks as Input it returns NULL_STRING
   function Trim
      (Source : in String;
       Side   : in Trim_End := Both
      ) return String;

   --  Truncates or fills a String to exactly N in Length
   function Head
      (Source : in String;
       Count  : in Natural
      ) return String;

   --  Reads a text file and outs a String that is as much of the
   --  first line encountered that is not a comment
   -- FIXME: Will raise exception if file is not open.
   procedure Get_Non_Comment_Line
      (File : in  Ada.Text_IO.File_Type;
       Item : out String;
       Last : out Integer
      );

   ---------------------------------------------------------------------------

end Latin_Utils.Strings_Package;
