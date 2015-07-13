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
package body Parse_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Parse_Record)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Stem_Type_IO.Get (File, Item.Stem);
      Ada.Text_IO.Get (File, Spacer);
      Inflection_Record_IO.Get (File, Item.IR);
      Ada.Text_IO.Get (File, Spacer);
      Dictionary_Kind_IO.Get (File, Item.D_K);
      Ada.Text_IO.Get (File, Spacer);
      MNPC_IO.Get (File, Item.MNPC);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Parse_Record)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Stem_Type_IO.Get (Item.Stem);
      Ada.Text_IO.Get (Spacer);
      Inflection_Record_IO.Get (Item.IR);
      Ada.Text_IO.Get (Spacer);
      Dictionary_Kind_IO.Get (Item.D_K);
      Ada.Text_IO.Get (Spacer);
      MNPC_IO.Get (Item.MNPC);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Parse_Record) is
   begin
      Stem_Type_IO.Put (File, Item.Stem);
      Ada.Text_IO.Put (File, ' ');
      Inflection_Record_IO.Put (File, Item.IR);
      Ada.Text_IO.Put (File, ' ');
      Dictionary_Kind_IO.Put (File, Item.D_K);
      Ada.Text_IO.Put (File, ' ');
      MNPC_IO.Put (File, Item.MNPC);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Parse_Record) is
   begin
      Stem_Type_IO.Put (Item.Stem);
      Ada.Text_IO.Put (' ');
      Inflection_Record_IO.Put (Item.IR);
      Ada.Text_IO.Put (' ');
      Dictionary_Kind_IO.Put (Item.D_K);
      Ada.Text_IO.Put (' ');
      MNPC_IO.Put (Item.MNPC);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
        Target : out Parse_Record;
        Last   : out Integer
      )
   is
      -- Used for computing lower bound of substring
      Low : Integer := Source'First - 1;
   begin
      Stem_Type_IO.Get (Source, Target.Stem, Low);
      Low := Low + 1;
      Inflection_Record_IO.Get
         (Source (Low + 1 .. Source'Last), Target.IR, Low);
      Low := Low + 1;
      Dictionary_Kind_IO.Get (Source (Low + 1 .. Source'Last), Target.D_K, Low);
      Low := Low + 1;
      MNPC_IO.Get (Source (Low + 1 .. Source'Last), Target.MNPC, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Parse_Record)
   is
      -- These variables are used for computing bounds of substrings
      Low  : Integer := 0;
      High : Integer := 0;
   begin
      -- Put Stem_Type
      High := Low + Max_Stem_Size;
      Target (Target'First + Low .. Target'First - 1 + High) := Item.Stem;
      Low := High + 1;
      Target (Target'First - 1 + Low) :=  ' ';

      -- Put Inflection_Record
      High := Low + Inflection_Record_IO.Default_Width;
      Inflection_Record_IO.Put
         (Target (Target'First + Low .. Target'First - 1 + High), Item.IR);
      Low := High + 1;
      Target (Target'First - 1 + Low) :=  ' ';

      -- Put Dictionary_Kind
      High := Low + Dictionary_Kind_IO.Default_Width;
      Dictionary_Kind_IO.Put
         (Target (Target'First + Low .. Target'First - 1 + High), Item.D_K);
      Low := High + 1;

      -- Put MNPC
      Target (Target'First - 1 + Low) :=  ' ';
      High := Low + MNPC_IO_Default_Width;
      MNPC_IO.Put
         (Target (Target'First + Low .. Target'First - 1 + High), Item.MNPC);

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Parse_Record_IO;
