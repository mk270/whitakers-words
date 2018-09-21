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
package body Suffix_Entry_Io is
   Spacer : Character := ' ';

   procedure Get (F : in File_Type; P : out Suffix_Entry) is
   begin
      Get (F, P.Root);
      Get (F, Spacer);
      Get (F, P.Root_Key);
      Get (F, Spacer);
      Get (F, P.Target);
      Get (F, Spacer);
      Get (F, P.Target_Key);
   end Get;

   procedure Get (P : out Suffix_Entry) is
   begin
      Get (P.Root);
      Get (Spacer);
      Get (P.Root_Key);
      Get (Spacer);
      Get (P.Target);
      Get (Spacer);
      Get (P.Target_Key);
   end Get;

   procedure Put (F : in File_Type; P : in Suffix_Entry) is
   begin
      Put (F, P.Root);
      Put (F, ' ');
      Put (F, P.Root_Key, 2);
      Put (F, ' ');
      Put (F, P.Target);
      Put (F, ' ');
      Put (F, P.Target_Key, 2);
   end Put;

   procedure Put (P : in Suffix_Entry) is
   begin
      Put (P.Root);
      Put (' ');
      Put (P.Root_Key, 2);
      Put (' ');
      Put (P.Target);
      Put (' ');
      Put (P.Target_Key, 2);
   end Put;

   procedure Get (S : in String; P : out Suffix_Entry; Last : out Integer) is
      L : Integer := S'First - 1;
   begin
      --TEXT_IO.PUT ("#1" & INTEGER'IMAGE (L));
      Get (S (L + 1 .. S'Last), P.Root, L);
      --TEXT_IO.PUT ("#2" & INTEGER'IMAGE (L));
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Root_Key, L);
      --TEXT_IO.PUT ("#3" & INTEGER'IMAGE (L));
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Target, L);
      --TEXT_IO.PUT ("#4" & INTEGER'IMAGE (L));
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Target_Key, Last);
      --TEXT_IO.PUT ("#5" & INTEGER'IMAGE (LAST));
   end Get;

   procedure Put (S : out String; P : in Suffix_Entry) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Part_Of_Speech_Type_IO.Default_Width;
      Put (S (L + 1 .. M), P.Root);
      L := M + 1;
      S (L) :=  ' ';
      M := L + 2;
      Put (S (L + 1 .. M), P.Root_Key);
      L := M + 1;
      S (L) :=  ' ';
      M := L + Target_Entry_Io.Default_Width;
      Put (S (L + 1 .. M), P.Target);
      L := M + 1;
      S (L) :=  ' ';
      M := L + 2;
      Put (S (L + 1 .. M), P.Target_Key);
      S (M + 1 .. S'Last) := (others => ' ');
   end Put;
end Suffix_Entry_Io;
