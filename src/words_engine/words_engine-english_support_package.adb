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
package body Words_Engine.English_Support_Package is
   --use EWDS_DIRECT_IO;
   use Ada.Text_IO;

   package body Ewds_Record_Io is
      use Part_Of_Speech_Type_IO;
      use Frequency_Type_IO;
      use Ada.Integer_Text_IO;
      Spacer : Character := ' ';
      Nwidth : constant := 5;

      procedure Get (F : in Ada.Text_IO.File_Type; P : out Ewds_Record) is
      begin
         Get (F, P.W);
         Get (F, Spacer);
         Get (F, P.Aux);
         Get (F, Spacer);
         Get (F, P.N);
         Get (F, Spacer);
         Get (F, P.Pofs);
         Get (F, Spacer);
         Get (F, P.Freq);
         Get (F, Spacer);
         Get (F, P.Semi);
         Get (F, Spacer);
         Get (F, P.Kind);
         Get (F, Spacer);
         Get (F, P.Rank);
      end Get;

      procedure Get (P : out Ewds_Record) is
      begin
         Get (P.W);
         Get (Spacer);
         Get (P.Aux);
         Get (Spacer);
         Get (P.N);
         Get (Spacer);
         Get (P.Pofs);
         Get (Spacer);
         Get (P.Freq);
         Get (Spacer);
         Get (P.Semi);
         Get (Spacer);
         Get (P.Kind);
         Get (Spacer);
         Get (P.Rank);
      end Get;

      procedure Put (F : in Ada.Text_IO.File_Type; P : in Ewds_Record) is
      begin
         Put (F, P.W);
         Put (F, ' ');
         Put (F, P.Aux);
         Put (F, ' ');
         Put (F, P.N);
         Put (F, ' ');
         Put (F, P.Pofs);
         Put (F, ' ');
         Put (F, P.Freq);
         Put (F, ' ');
         Put (F, P.Semi, Nwidth);
         Put (F, ' ');
         Put (F, P.Kind, Nwidth);
         Put (F, ' ');
         Put (F, P.Rank, Nwidth);
      end Put;

      procedure Put (P : in Ewds_Record) is
      begin
         Put (P.W);
         Put (' ');
         Put (P.Aux);
         Put (' ');
         Put (P.N);
         Put (' ');
         Put (P.Pofs);
         Put (' ');
         Put (P.Freq);
         Put (' ');
         Put (P.Semi, Nwidth);
         Put (' ');
         Put (P.Kind, Nwidth);
         Put (' ');
         Put (P.Rank, Nwidth);
      end Put;

      procedure Get (S : in String; P : out Ewds_Record; Last : out Integer) is
         L : Integer := S'First - 1;
      begin
         P.W := S (L + 1 .. L + Eword_Size);
         L := L + Eword_Size + 1;
         P.Aux := S (L + 1 .. L + Aux_Word_Size);
         L := L + Aux_Word_Size + 1;
         Get (S (L + 1 .. S'Last), P.N, L);
         L := L + 1;
         Get (S (L + 1 .. S'Last), P.Pofs, L);
         L := L + 1;
         Get (S (L + 1 .. S'Last), P.Freq, L);
         L := L + 1;
         Get (S (L + 1 .. S'Last), P.Semi, L);
         L := L + 1;
         Get (S (L + 1 .. S'Last), P.Kind, L);
         L := L + 1;
         Get (S (L + 1 .. S'Last), P.Rank, Last);
      end Get;

      procedure Put (S : out String; P : in Ewds_Record) is
         L : Integer := S'First - 1;
         M : Integer := 0;
      begin
         M := L + Eword_Size;
         S (L + 1 .. M) :=  P.W;
         L := M + 1;
         S (L) :=  ' ';
         M := L + Aux_Word_Size;
         S (L + 1 .. M) := P.Aux;
         L := M + 1;
         S (L) :=  ' ';
         M := L + Line_Number_Width;
         Put (S (L + 1 .. M), P.N);
         S (L) :=  ' ';
         M := L + Part_Of_Speech_Type_IO.Default_Width;
         Put (S (L + 1 .. M), P.Pofs);
         S (L) :=  ' ';
         M := L + Frequency_Type_IO.Default_Width;
         Put (S (L + 1 .. M), P.Freq);
         S (L) :=  ' ';
         M := L + Priority_Width;
         Put (S (L + 1 .. M), P.Semi, Nwidth);
         S (L) :=  ' ';
         M := L + Priority_Width;
         Put (S (L + 1 .. M), P.Kind, Nwidth);
         S (L) :=  ' ';
         M := L + Priority_Width;
         Put (S (L + 1 .. M), P.Rank, Nwidth);

         S (M + 1 .. S'Last) := (others => ' ');
      end Put;

   end Ewds_Record_Io;

end Words_Engine.English_Support_Package;
