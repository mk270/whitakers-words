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
package body Adjective_Entry_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Adjective_Entry)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (File, Item.Decl);
      Ada.Text_IO.Get (File, Spacer);
      Comparison_Type_IO.Get (File, Item.Co);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Adjective_Entry) is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (Item.Decl);
      Ada.Text_IO.Get (Spacer);
      Comparison_Type_IO.Get (Item.Co);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Adjective_Entry) is
   begin
      Decn_Record_IO.Put (File, Item.Decl);
      Ada.Text_IO.Put (File, ' ');
      Comparison_Type_IO.Put (File, Item.Co);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Adjective_Entry) is
   begin
      Decn_Record_IO.Put (Item.Decl);
      Ada.Text_IO.Put (' ');
      Comparison_Type_IO.Put (Item.Co);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Adjective_Entry;
       Last   : out Integer
      )
   is
      -- Used for computing lower bound of substring
      Low : Integer := Source'First - 1;
   begin
      Decn_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Decl, Low);
      Low := Low + 1;
      Comparison_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Co, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Adjective_Entry) is
      -- These variables are used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Decn_Record
      High := Low + Decn_Record_IO.Default_Width;
      Decn_Record_IO.Put (Target (Low + 1 .. High), Item.Decl);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Comparison_Type
      High := Low + Comparison_Type_IO.Default_Width;
      Comparison_Type_IO.Put (Target (Low + 1 .. High), Item.Co);

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Adjective_Entry_IO;
