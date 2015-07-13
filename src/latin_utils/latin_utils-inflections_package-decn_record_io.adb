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

with Ada.Integer_Text_IO;
separate (Latin_Utils.Inflections_Package)
package body Decn_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Decn_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Ada.Integer_Text_IO.Get (File, Item.Which);
      Get (File, Spacer);
      Ada.Integer_Text_IO.Get (File, Item.Var);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Decn_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Ada.Integer_Text_IO.Get (Item.Which);
      Get (Spacer);
      Ada.Integer_Text_IO.Get (Item.Var);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Decn_Record) is
   begin
      Ada.Integer_Text_IO.Put (File, Item.Which, 1);
      Put (File, ' ');
      Ada.Integer_Text_IO.Put (File, Item.Var, 1);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Decn_Record) is
   begin
      Ada.Integer_Text_IO.Put (Item.Which, 1);
      Put (' ');
      Ada.Integer_Text_IO.Put (Item.Var, 1);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Decn_Record;
       Last   : out Integer
      )
   is
      -- This variable are used for computing lower bound of substrings
      Low : Integer := Source'First - 1;
   begin
      Ada.Integer_Text_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Which, Low);
      Low := Low + 1;
      Ada.Integer_Text_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Var, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Decn_Record)
   is
      -- These variables are used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Which_Type
      High := Low + 1;
      Ada.Integer_Text_IO.Put (Target (Low + 1 .. High), Item.Which);
      Low := High + 1;

      -- Put Variant_Type
      Target (Low) :=  ' ';
      High := Low + 1;
      Ada.Integer_Text_IO.Put (Target (Low + 1 .. High), Item.Var);

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Decn_Record_IO;
