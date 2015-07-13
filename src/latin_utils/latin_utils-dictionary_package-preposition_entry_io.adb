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

separate (Latin_Utils.Dictionary_Package)
package body Preposition_Entry_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Preposition_Entry)
   is
   begin
      Case_Type_IO.Get (File, Item.Obj);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Preposition_Entry) is
   begin
      Case_Type_IO.Get (Item.Obj);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Preposition_Entry)
   is
   begin
      Case_Type_IO.Put (File, Item.Obj);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Preposition_Entry) is
   begin
      Case_Type_IO.Put (Item.Obj);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Preposition_Entry;
       Last   : out Integer
      )
   is
   begin
      Case_Type_IO.Get (Source, Target.Obj, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Preposition_Entry) is
      -- Used to get bounds of substring
      Low  : constant Integer := Target'First - 1;
      High : Integer := 0;
   begin
      High := Low + Case_Type_IO.Default_Width;
      Case_Type_IO.Put (Target (Low + 1 .. High), Item.Obj);
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Preposition_Entry_IO;
