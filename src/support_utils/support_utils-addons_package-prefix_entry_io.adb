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
package body Prefix_Entry_Io is
   Spacer : Character := ' ';

   procedure Get (F : in File_Type; P : out Prefix_Entry) is
   begin
      Get (F, P.Root);
      Get (F, Spacer);
      Get (F, P.Target);
   end Get;

   procedure Get (P : out Prefix_Entry) is
   begin
      Get (P.Root);
      Get (Spacer);
      Get (P.Target);
   end Get;

   procedure Put (F : in File_Type; P : in Prefix_Entry) is
   begin
      Put (F, P.Root);
      Put (F, ' ');
      Put (F, P.Target);
   end Put;

   procedure Put (P : in Prefix_Entry) is
   begin
      Put (P.Root);
      Put (' ');
      Put (P.Target);
   end Put;

   procedure Get (S : in String; P : out Prefix_Entry; Last : out Integer) is
      L : Integer := S'First - 1;
   begin
      Get (S (L + 1 .. S'Last), P.Root, L);
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Target, Last);
   end Get;

   procedure Put (S : out String; P : in Prefix_Entry) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Part_Of_Speech_Type_IO.Default_Width;
      Put (S (L + 1 .. M), P.Root);
      L := M + 1;
      S (L) :=  ' ';
      M := L + Part_Of_Speech_Type_IO.Default_Width;
      Put (S (L + 1 .. M), P.Target);
      S (M + 1 .. S'Last) := (others => ' ');
   end Put;

end Prefix_Entry_Io;
