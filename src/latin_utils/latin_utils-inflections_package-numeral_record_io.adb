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
package body Numeral_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Numeral_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (File, Item.Decl);
      Get (File, Spacer);
      Case_Type_IO.Get (File, Item.Of_Case);
      Get (File, Spacer);
      Number_Type_IO.Get (File, Item.Number);
      Get (File, Spacer);
      Gender_Type_IO.Get (File, Item.Gender);
      Get (File, Spacer);
      Numeral_Sort_Type_IO.Get (File, Item.Sort);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Numeral_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (Item.Decl);
      Get (Spacer);
      Get (Spacer);
      Number_Type_IO.Get (Item.Number);
      Get (Spacer);
      Gender_Type_IO.Get (Item.Gender);
      Get (Spacer);
      Numeral_Sort_Type_IO.Get (Item.Sort);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Numeral_Record) is
   begin
      Decn_Record_IO.Put (File, Item.Decl);
      Put (File, ' ');
      Case_Type_IO.Put (File, Item.Of_Case);
      Put (File, ' ');
      Number_Type_IO.Put (File, Item.Number);
      Put (File, ' ');
      Gender_Type_IO.Put (File, Item.Gender);
      Put (File, ' ');
      Numeral_Sort_Type_IO.Put (File, Item.Sort);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Numeral_Record) is
   begin
      Decn_Record_IO.Put (Item.Decl);
      Put (' ');
      Case_Type_IO.Put (Item.Of_Case);
      Put (' ');
      Number_Type_IO.Put (Item.Number);
      Put (' ');
      Gender_Type_IO.Put (Item.Gender);
      Put (' ');
      Numeral_Sort_Type_IO.Put (Item.Sort);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
     (Source : in  String;
      Target : out Numeral_Record;
      Last   : out Integer
     )
   is
      -- Used for computing lower bound of substrings
      Low : Integer := Source'First - 1;
   begin
      Decn_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Decl, Low);
      Low := Low + 1;
      Case_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Of_Case, Low);
      Low := Low + 1;
      Number_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Number, Low);
      Low := Low + 1;
      Gender_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Gender, Low);
      Low := Low + 1;
      Numeral_Sort_Type_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Sort, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Numeral_Record)
   is
      -- Used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Decn_Record
      High := Low + Decn_Record_IO.Default_Width;
      Decn_Record_IO.Put (Target (Low + 1 .. High), Item.Decl);

      -- Put Case_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Case_Type_IO.Default_Width;
      Case_Type_IO.Put (Target (Low + 1 .. High), Item.Of_Case);

      -- Put Number_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Number_Type_IO.Default_Width;
      Number_Type_IO.Put (Target (Low + 1 .. High), Item.Number);

      -- Put Gender_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Gender_Type_IO.Default_Width;
      Gender_Type_IO.Put (Target (Low + 1 .. High), Item.Gender);

      -- Put Numeral_Sort_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Numeral_Sort_Type_IO.Default_Width;
      Numeral_Sort_Type_IO.Put (Target (Low + 1 .. High), Item.Sort);
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Numeral_Record_IO;
