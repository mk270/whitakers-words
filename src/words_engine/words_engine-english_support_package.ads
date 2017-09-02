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

with Ada.Text_IO;
with Ada.Direct_IO;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
package Words_Engine.English_Support_Package is

   Eword_Size    : constant := 24;
   Aux_Word_Size : constant := 12;
   Line_Number_Width : constant := 10;
   Priority_Width : constant := 3;

   subtype Eword is String (1 .. Eword_Size);
   Null_Eword : constant Eword := (others => ' ');
   subtype Auxword is String (1 .. Aux_Word_Size);
   Null_Auxword : constant Auxword := (others => ' ');
   subtype Priority_Type is Integer range 0 .. 99;

   Number_Of_Ewords : Integer := 0;

   type Ewds_Record is
      record
         W    : Eword := Null_Eword;
         Aux  : Auxword := Null_Auxword;
         N    : Integer := 0;
         Pofs : Part_Of_Speech_Type := X;
         Freq : Frequency_Type := X;
         Semi : Integer := 0;
         Kind : Integer := 0;
         Rank : Integer := 0;
      end record;

   Null_Ewds_Record : constant Ewds_Record := ((others => ' '),
     (others => ' '), 0, X, X, 0, 0, 0);

   type Ewds_Array is array (Positive range <>) of Ewds_Record;

   package Ewds_Direct_Io is new Ada.Direct_IO (Ewds_Record);

   package Ewds_Record_Io is
      Default_Width : Natural;
      procedure Get (F : in Ada.Text_IO.File_Type; P : out Ewds_Record);
      procedure Get (P : out Ewds_Record);
      procedure Put (F : in Ada.Text_IO.File_Type; P : in Ewds_Record);
      procedure Put (P : in Ewds_Record);
      procedure Get (S : in String; P : out Ewds_Record;
                                    Last : out Integer);
      procedure Put (S : out String; P : in Ewds_Record);
   end Ewds_Record_Io;

   English_Dictionary_Available : array (Dictionary_Kind) of Boolean := (False,
     False, False, False, False, False, False,  --  don't SEARCH
     False, False, False, False);

   Ewds_File : Ewds_Direct_Io.File_Type;

end Words_Engine.English_Support_Package;
