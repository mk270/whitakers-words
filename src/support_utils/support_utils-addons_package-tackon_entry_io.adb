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

separate (Support_Utils.Addons_Package)
package body Tackon_Entry_Io is
   procedure Get (F : in File_Type; I : out Tackon_Entry) is
   begin
      Get (F, I.Base);
   end Get;

   procedure Get (I : out Tackon_Entry) is
   begin
      Get (I.Base);
   end Get;

   procedure Put (F : in File_Type; I : in Tackon_Entry) is
   begin
      Put (F, I.Base);
   end Put;

   procedure Put (I : in Tackon_Entry) is
   begin
      Put (I.Base);
   end Put;

   procedure Get (S : in String; I : out Tackon_Entry; Last : out Integer) is
      L : constant Integer := S'First - 1;
   begin
      Get (S (L + 1 .. S'Last), I.Base, Last);
   end Get;

   procedure Put (S : out String; I : in Tackon_Entry) is
      L : constant Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Target_Entry_Io.Default_Width;
      Put (S (L + 1 .. M), I.Base);
      S (S'First .. S'Last) := (others => ' ');
   end Put;

end Tackon_Entry_Io;
