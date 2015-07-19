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
package body Preposition_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Preposition_Record) is
   begin
      Case_Type_IO.Get (File, Item.Of_Case);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Preposition_Record) is
   begin
      Case_Type_IO.Get (Item.Of_Case);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Preposition_Record) is
   begin
      Case_Type_IO.Put (File, Item.Of_Case);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Preposition_Record) is
   begin
      Case_Type_IO.Put (Item.Of_Case);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
     (Source : in  String;
      Target : out Preposition_Record;
      Last   : out Integer
     )
   is
   begin
      Case_Type_IO.Get (Source, Target.Of_Case, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Preposition_Record)
   is
      -- Used for computing upper bound of substring
      High : constant Integer :=
         Target'First - 1 + Case_Type_IO.Default_Width;
   begin
      Case_Type_IO.Put (Target (Target'First .. High), Item.Of_Case);

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Preposition_Record_IO;
