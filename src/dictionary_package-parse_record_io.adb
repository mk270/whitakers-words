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

-- TODO: Drop use clauses. It is better to see what exactly is called -
--       helps maintain code in the long run (even if it is not nice to type)
separate (Dictionary_Package)
package body Parse_Record_IO is
   use Inflection_Record_IO;
   use Dictionary_Kind_IO;
   use MNPC_IO;
   Spacer : Character := ' ';

   procedure Get (File : in Text_IO.File_Type; Item : out Parse_Record) is
   begin
      Get (File, Item.Stem);
      Get (File, Spacer);
      Get (File, Item.IR);
      Get (File, Spacer);
      Get (File, Item.D_K);
      Get (File, Spacer);
      Get (File, Item.MNPC);
   end Get;

   procedure Get (Item : out Parse_Record) is
   begin
      Get (Item.Stem);
      Get (Spacer);
      Get (Item.IR);
      Get (Spacer);
      Get (Item.D_K);
      Get (Spacer);
      Get (Item.MNPC);
   end Get;

   procedure Put (File : in Text_IO.File_Type; Item : in Parse_Record) is
   begin
      Put (File, Item.Stem);
      Put (File, ' ');
      Put (File, Item.IR);
      Put (File, ' ');
      Put (File, Item.D_K);
      Put (File, ' ');
      Put (File, Item.MNPC);
   end Put;

   procedure Put (Item : in Parse_Record) is
   begin
      Text_IO.Put (Item.Stem);
      Text_IO.Put (' ');
      Inflection_Record_IO.Put (Item.IR);
      Text_IO.Put (' ');
      Dictionary_Kind_IO.Put (Item.D_K);
      Text_IO.Put (' ');
      MNPC_IO.Put (Item.MNPC);
   end Put;

   procedure Get
      ( Source : in  String;
        Target : out Parse_Record;
        Last   : out Integer
      )
   is
      l : Integer := Source'First - 1;
   begin
      Stem_Type_IO.Get (Source, Target.Stem, l);
      l := l + 1;
      Get (Source (l + 1 .. Source'Last), Target.IR, l);
      l := l + 1;
      Get (Source (l + 1 .. Source'Last), Target.D_K, l);
      l := l + 1;
      Get (Source (l + 1 .. Source'Last), Target.MNPC, Last);
   end Get;

   procedure Put (Target : out String; Item : in Parse_Record) is
      l : Integer := 0;
      m : Integer := 0;
   begin
      m := l + Max_Stem_Size;
      Target (Target'First + l .. Target'First - 1 + m) := Item.Stem;
      l := m + 1;
      Target (Target'First - 1 + l) :=  ' ';
      m := l + Inflection_Record_IO.Default_Width;
      Put (Target (Target'First + l .. Target'First - 1 + m), Item.IR);
      l := m + 1;
      Target (Target'First - 1 + l) :=  ' ';
      m := l + Dictionary_Kind_IO.Default_Width;
      Put (Target (Target'First + l .. Target'First - 1 + m), Item.D_K);
      l := m + 1;
      Target (Target'First - 1 + l) :=  ' ';
      m := l + MNPC_IO_Default_Width;
      Put (Target (Target'First + l .. Target'First - 1 + m), Item.MNPC);
      Target (m + 1 .. Target'Last) := (others => ' ');
   end Put;
end Parse_Record_IO;
