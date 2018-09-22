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

separate (Support_Utils.Line_Stuff)
package body Prefix_Line_Io is
   use Part_Of_Speech_Type_IO;
   use Prefix_Entry_Io;
   Spacer : Character := ' ';

   procedure Get (F : in File_Type; P : out Prefix_Line) is
   begin
      Get (F, P.Pofs);
      Get (F, Spacer);
      Get (F, P.Fix);
      Get (F, Spacer);
      Get (F, P.Connect);
      Get (F, Spacer);
      Get (F, P.Entr);
      Get (F, Spacer);
      Get (F, P.Mean);
   end Get;

   procedure Get (P : out Prefix_Line) is
   begin
      Get (P.Pofs);
      Get (Spacer);
      Get (P.Fix);
      Get (Spacer);
      Get (P.Connect);
      Get (Spacer);
      Get (P.Entr);
      Get (Spacer);
      Get (P.Mean);
   end Get;

   procedure Put (F : in File_Type; P : in Prefix_Line) is
   begin
      Put (F, P.Pofs);
      Put (F, ' ');
      Put (F, P.Fix);
      Put (F, ' ');
      Put (F, P.Connect);
      Put (F, ' ');
      Put (F, P.Entr);
      Put (F, ' ');
      Put (F, P.Mean);
   end Put;

   procedure Put (P : in Prefix_Line) is
   begin
      Put (P.Pofs);
      Put (' ');
      Put (P.Fix);
      Put (' ');
      Put (P.Connect);
      Put (' ');
      Put (P.Entr);
      Put (' ');
      Put (P.Mean);
   end Put;

   procedure Get (S : in String; P : out Prefix_Line; Last : out Integer) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Dictionary_Kind_IO.Default_Width;
      Get (S (L + 1 .. S'Last), P.Pofs, L);
      L := M;
      L := L + 1;
      M := L + Max_Stem_Size;
      P.Fix := S (L + 1 .. M);
      L := M;
      L := L + 1;
      P.Connect := S (L + 1);
      L := L + 1;
      M := L + Prefix_Entry_Io.Default_Width;
      Get (S (L + 1 .. S'Last), P.Entr, L);
      L := M + 1;
      M := L + Max_Meaning_Size;
      P.Mean := S (L + 1 .. M);
      Last := M;
   end Get;

   procedure Put (S : out String; P : in Prefix_Line) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Dictionary_Kind_IO.Default_Width;
      Put (S (L + 1 .. M), P.Pofs);
      L := M + 1;
      S (L) :=  ' ';
      M := L + Max_Stem_Size;
      S (L + 1 .. M) := P.Fix;
      L := M + 1;
      S (L) :=  ' ';
      S (L + 1) := P.Connect;
      M := L + Prefix_Entry_Io.Default_Width;
      Put (S (L + 1 .. M), P.Entr);
      L := M + 1;
      S (L) :=  ' ';
      M := L + Max_Meaning_Size;
      S (L + 1 .. M) := P.Mean;
      M := L + 1;
      S (M + 1 .. S'Last) := (others => ' ');
   end Put;
end Prefix_Line_Io;
