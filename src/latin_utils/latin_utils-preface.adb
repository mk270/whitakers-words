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
with Latin_Utils.Config;
use Latin_Utils;
package body Latin_Utils.Preface is

   ---------------------------------------------------------------------------

   procedure Put (Item : String) is
   begin
      if not Config.Suppress_Preface then
         Ada.Text_IO.Put (Ada.Text_IO.Current_Output, Item);
      end if;
   end Put;

   ---------------------------------------------------------------------------

   procedure Set_Col (To : Ada.Text_IO.Positive_Count) is
   begin
      if not Config.Suppress_Preface then
         Ada.Text_IO.Set_Col (Ada.Text_IO.Current_Output, To);
      end if;
   end Set_Col;

   ---------------------------------------------------------------------------

   procedure Put_Line (Item : String) is
   begin
      if not Config.Suppress_Preface then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Output, Item);
      end if;
   end Put_Line;

   ---------------------------------------------------------------------------

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      if not Config.Suppress_Preface then
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Output, Spacing);
      end if;
   end New_Line;

   ---------------------------------------------------------------------------

   procedure Put (Item : Integer; Width : Ada.Text_IO.Field := Integer'Width) is
   begin
      if not Config.Suppress_Preface then
         Ada.Integer_Text_IO.Put (Ada.Text_IO.Current_Output, Item, Width);
      end if;
   end Put;

   ---------------------------------------------------------------------------

end Latin_Utils.Preface;
