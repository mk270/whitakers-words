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
package body Adverb_Entry_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Adverb_Entry) is
   begin
      Comparison_Type_IO.Get (File, Item.Co);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Adverb_Entry) is
   begin
      Comparison_Type_IO.Get (Item.Co);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Adverb_Entry) is
   begin
      Comparison_Type_IO.Put (File, Item.Co);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Adverb_Entry) is
   begin
      Comparison_Type_IO.Put (Item.Co);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Adverb_Entry;
       Last   : out Integer
      )
   is
   begin
      Comparison_Type_IO.Get (Source, Target.Co, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Adverb_Entry) is
      -- Used to get upper bound of substring
      High : constant Integer :=
         Target'First + Comparison_Type_IO.Default_Width - 1;
   begin
      -- Put Comparison_Type
      Comparison_Type_IO.Put (Target (Target'First .. High), Item.Co);

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Adverb_Entry_IO;
