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
package body Verb_Entry_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Verb_Entry)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (File, Item.Con);
      Ada.Text_IO.Get (File, Spacer);
      Verb_Kind_Type_IO.Get (File, Item.Kind);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Verb_Entry)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (Item.Con);
      Ada.Text_IO.Get (Spacer);
      Verb_Kind_Type_IO.Get (Item.Kind);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Verb_Entry) is
   begin
      Decn_Record_IO.Put (File, Item.Con);
      Ada.Text_IO.Put (File, ' ');
      Verb_Kind_Type_IO.Put (File, Item.Kind);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Verb_Entry) is
   begin
      Decn_Record_IO.Put (Item.Con);
      Ada.Text_IO.Put (' ');
      Verb_Kind_Type_IO.Put (Item.Kind);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Verb_Entry;
       Last   : out Integer
      )
   is
      -- Used to get lower bound of substring
      Low : Integer := Source'First - 1;
   begin
      Decn_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Con, Low);
      Low := Low + 1;
      Verb_Kind_Type_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Kind, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Verb_Entry) is
      -- Used to get bounds of substring
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Decn_Record
      High := Low + Decn_Record_IO.Default_Width;
      Decn_Record_IO.Put (Target (Low + 1 .. High), Item.Con);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Verb_Kind_Type
      High := Low + Verb_Kind_Type_IO.Default_Width;
      Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.Kind);

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Verb_Entry_IO;
