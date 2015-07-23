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
package body Tackon_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Tackon_Record)
   is
      pragma Unreferenced (File);
   begin
      Item := Null_Tackon_Record;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Tackon_Record) is
   begin
      Item := Null_Tackon_Record;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Tackon_Record) is
   begin
      null;
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Tackon_Record) is
   begin
      null;
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Tackon_Record;
       Last   : out Integer
      )
   is
   begin
      Target := Null_Tackon_Record;
      Last   := Source'First - 1;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Tackon_Record)
   is
      pragma Unreferenced (Item);
   begin
      Target := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Tackon_Record_IO;
