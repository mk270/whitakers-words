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
package body Unique_Entry_Io is
   use Quality_Record_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
   Spacer : Character;

   procedure Get (F : in File_Type; P : out Unique_Entry) is
      Ue : Unique_Entry;
   begin
      Get (F, Ue.Stem);
      Get (F, Spacer);
      Get (F, Ue.Qual);
      Get (F, Spacer);
      Get (F, Ue.Qual.Pofs, Ue.Kind);
      Get (F, Spacer);
      Get (F, Ue.Tran);
      P := Ue;
   end Get;

   procedure Get (P : out Unique_Entry) is
      Ue : Unique_Entry;
   begin
      Get (P.Stem);
      Get (Spacer);
      Get (Ue.Qual);
      Get (Spacer);
      Get (Ue.Qual.Pofs, Ue.Kind);
      Get (Spacer);
      Get (P.Tran);
   end Get;

   procedure Put (F : in File_Type; P : in Unique_Entry) is
   begin
      Put (F, P.Stem);
      Put (F, ' ');
      Put (F, P.Qual);
      Put (F, ' ');
      Put (F, P.Qual.Pofs, P.Kind);
      Put (F, ' ');
      Put (F, P.Tran);
   end Put;

   procedure Put (P : in Unique_Entry) is
   begin
      Put (P.Stem);
      Put (' ');
      Put (P.Qual);
      Put (' ');
      Put (P.Qual.Pofs, P.Kind);
      Put (' ');
      Put (P.Tran);
   end Put;

   procedure Get (S : in String; P : out Unique_Entry; Last : out Integer) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Max_Stem_Size;
      P.Stem := S (L + 1 .. M);
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Qual, L);
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Qual.Pofs, P.Kind, L);
      L := L + 1;
      Get (S (L + 1 .. S'Last), P.Tran, Last);
   end Get;

   procedure Put (S : out String; P : in Unique_Entry) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Max_Stem_Size;
      S (L + 1 .. M) := P.Stem;
      L := M + 1;
      S (L) :=  ' ';
      M := L + Quality_Record_IO.Default_Width;
      Put (S (L + 1 .. M), P.Qual);
      L := M + 1;
      S (L) :=  ' ';
      M := L + Kind_Entry_IO.Default_Width;
      Put (S (L + 1 .. M), P.Qual.Pofs, P.Kind);
      L := M + 1;
      S (L) :=  ' ';
      M := M + Max_Meaning_Size;
      Put (S (L + 1 .. M), P.Tran);
      S (M + 1 .. S'Last) := (others => ' ');
   end Put;

end Unique_Entry_Io;
