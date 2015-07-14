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
package body Adverb_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Adverb_Record) is
   begin
      Comparison_Type_IO.Get (File, Item.Comparison);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Adverb_Record) is
   begin
      Comparison_Type_IO.Get (Item.Comparison);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Adverb_Record) is
   begin
      Comparison_Type_IO.Put (File, Item.Comparison);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Adverb_Record) is
   begin
      Comparison_Type_IO.Put (Item.Comparison);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
     (Source : in  String;
      Target : out Adverb_Record;
      Last   : out Integer
     )
   is
   begin
      Comparison_Type_IO.Get (Source, Target.Comparison, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Adverb_Record)
   is
      High : constant Integer :=
         Target'First - 1 + Comparison_Type_IO.Default_Width;
   begin
      Comparison_Type_IO.Put (Target (Target'First .. High), Item.Comparison);
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Adverb_Record_IO;
