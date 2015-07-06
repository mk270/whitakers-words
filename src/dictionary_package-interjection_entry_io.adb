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

separate (Dictionary_Package)
package body Interjection_Entry_IO is

   ---------------------------------------------------------------------------

   Null_Interjection_Entry : Interjection_Entry;

   ---------------------------------------------------------------------------

   pragma Warnings (Off, "formal parameter ""File"" is not referenced");
   procedure Get (File : in File_Type; Item : out Interjection_Entry) is
      pragma Warnings (On, "formal parameter ""File"" is not referenced");
   begin
      Item := Null_Interjection_Entry;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Interjection_Entry) is
   begin
      Item := Null_Interjection_Entry;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Interjection_Entry) is null;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Interjection_Entry) is null;

   ---------------------------------------------------------------------------

   procedure Get
      ( Source : in String;
        Target : out Interjection_Entry;
        Last   : out Integer
      )
   is
      Low : constant Integer := Source'First - 1;
   begin
      Target := Null_Interjection_Entry;
      Last := Low;
   end Get;

   ---------------------------------------------------------------------------

   pragma Warnings (Off, "formal parameter ""Item"" is not referenced");
   procedure Put (Target : out String; Item : in Interjection_Entry) is
      pragma Warnings (On, "formal parameter ""Item"" is not referenced");
   begin
      Target (Target'First .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Interjection_Entry_IO;
