-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936-2010)
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
package body Noun_Entry_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Noun_Entry)
   is
      Spacer1, Spacer2 : Character;
   begin
      Decn_Record_IO.Get (File, Item.Decl);
      Ada.Text_IO.Get (File, Spacer1);
      pragma Unreferenced (Spacer1);
      Gender_Type_IO.Get (File, Item.Gender);
      Ada.Text_IO.Get (File, Spacer2);
      pragma Unreferenced (Spacer2);
      Noun_Kind_Type_IO.Get (File, Item.Kind);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Noun_Entry)
   is
      Spacer1, Spacer2 : Character;
   begin
      Decn_Record_IO.Get (Item.Decl);
      Ada.Text_IO.Get (Spacer1);
      pragma Unreferenced (Spacer1);
      Gender_Type_IO.Get (Item.Gender);
      Ada.Text_IO.Get (Spacer2);
      pragma Unreferenced (Spacer2);
      Noun_Kind_Type_IO.Get (Item.Kind);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Noun_Entry) is
   begin
      Decn_Record_IO.Put (File, Item.Decl);
      Ada.Text_IO.Put (File, ' ');
      Gender_Type_IO.Put (File, Item.Gender);
      Ada.Text_IO.Put (File, ' ');
      Noun_Kind_Type_IO.Put (File, Item.Kind);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Noun_Entry) is
   begin
      Decn_Record_IO.Put (Item.Decl);
      Ada.Text_IO.Put (' ');
      Gender_Type_IO.Put (Item.Gender);
      Ada.Text_IO.Put (' ');
      Noun_Kind_Type_IO.Put (Item.Kind);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in String;
        Target : out Noun_Entry;
        Last   : out Integer
     )
   is
      -- Used for computing lower bound of substring
      Low : Integer := Source'First - 1;
   begin
      Decn_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Decl, Low);
      Low := Low + 1;
      Gender_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Gender, Low);
      Low := Low + 1;
      Noun_Kind_Type_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Kind, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Noun_Entry)
   is
      -- These variables are used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Decn_Record
      High := Low + Decn_Record_IO.Default_Width;
      Decn_Record_IO.Put (Target (Low + 1 .. High), Item.Decl);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Gender_Type
      High := Low + Gender_Type_IO.Default_Width;
      Gender_Type_IO.Put (Target (Low + 1 .. High), Item.Gender);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Noun_Kind_Type
      High := Low + Noun_Kind_Type_IO.Default_Width;
      Noun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.Kind);

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Noun_Entry_IO;
