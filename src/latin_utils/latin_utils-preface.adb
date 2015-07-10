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

with Latin_Utils.Config;
use Latin_Utils;
package body Latin_Utils.Preface is

   procedure Put(s : String) is
   begin
      if not Config.suppress_preface  then
         Ada.Text_IO.Put(Ada.Text_IO.Current_Output, s);
      end if;
   end Put;

   procedure Set_Col(pc : Ada.Text_IO.Positive_Count) is
   begin
      if not Config.suppress_preface  then
         Ada.Text_IO.Set_Col(Ada.Text_IO.Current_Output, pc);
      end if;
   end Set_Col;

   procedure Put_Line(s : String) is
   begin
      if not Config.suppress_preface  then
         Ada.Text_IO.Put_Line(Ada.Text_IO.Current_Output, s);
      end if;
   end Put_Line;

   procedure New_Line(spacing  : Ada.Text_IO.Positive_Count := 1) is
   begin
      if not Config.suppress_preface  then
         Ada.Text_IO.New_Line(Ada.Text_IO.Current_Output, spacing);
      end if;
   end New_Line;

   procedure Put(n : Integer; width : Ada.Text_IO.Field := Integer'Width) is
      package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);
   begin
      if not Config.suppress_preface  then
         Integer_IO.Put(Ada.Text_IO.Current_Output, n, width);
      end if;
   end Put;

end Latin_Utils.Preface;
