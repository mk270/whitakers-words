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

separate (Latin_Utils.Inflections_Package)
package body Interjection_Record_IO is

   ---------------------------------------------------------------------------

   Null_Interjection_Record : constant Interjection_Record := (null record);

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Interjection_Record)
   is
      pragma Unreferenced (File);
   begin
      Item := Null_Interjection_Record;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Interjection_Record) is
   begin
      Item := Null_Interjection_Record;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Interjection_Record) is
   begin
      null;
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Interjection_Record) is
   begin
      null;
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Interjection_Record;
       Last   : out Integer
      )
   is
   begin
      Target := Null_Interjection_Record;
      Last   := Source'First - 1;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Interjection_Record)
   is
      pragma Unreferenced (Item);
   begin
      Target := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Interjection_Record_IO;
