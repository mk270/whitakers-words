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
package body Target_Entry_Io is
   use Noun_Entry_IO;
   use Pronoun_Entry_IO;
   use Propack_Entry_IO;
   use Adjective_Entry_IO;
   use Numeral_Entry_IO;
   use Adverb_Entry_IO;
   use Verb_Entry_IO;
   --  use KIND_ENTRY_IO;
   --
   --  use NOUN_KIND_TYPE_IO;
   --  use PRONOUN_KIND_TYPE_IO;
   --  use INFLECTIONS_PACKAGE.INTEGER_IO;
   --  use VERB_KIND_TYPE_IO;

   Spacer : Character := ' ';

   Noun  : Noun_Entry;
   Pronoun : Pronoun_Entry;
   Propack : Propack_Entry;
   Adjective : Adjective_Entry;
   Numeral : Numeral_Entry;
   Adverb : Adverb_Entry;
   Verb : Verb_Entry;

   --  NOUN_KIND  : NOUN_KIND_TYPE;
   --  PRONOUN_KIND : PRONOUN_KIND_TYPE;
   --  PROPACK_KIND : PRONOUN_KIND_TYPE;
   --  NUMERAL_VALUE : NUMERAL_VALUE_TYPE;
   --  VERB_KIND : VERB_KIND_TYPE;

   --KIND : KIND_ENTRY;

   procedure Get (F : in File_Type; P : out Target_Entry) is
      Ps : Target_Pofs_Type := X;
   begin
      Get (F, Ps);
      Get (F, Spacer);
      case Ps is
         when N =>
            Get (F, Noun);
            P := (N, Noun);
         when Pron =>
            Get (F, Pronoun);
            P := (Pron, Pronoun);
         when Pack =>
            Get (F, Propack);
            P := (Pack, Propack);
         when Adj =>
            Get (F, Adjective);
            P := (Adj, Adjective);
         when Num =>
            Get (F, Numeral);
            P := (Num, Numeral);
         when Adv =>
            Get (F, Adverb);
            P := (Adv, Adverb);
         when V =>
            Get (F, Verb);
            P := (V, Verb);
         when X =>
            P := (Pofs => X);
      end case;
      return;
   end Get;

   procedure Get (P : out Target_Entry) is
      Ps : Target_Pofs_Type := X;
   begin
      Get (Ps);
      Get (Spacer);
      case Ps is
         when N =>
            Get (Noun);
            P := (N, Noun);
         when Pron =>
            Get (Pronoun);
            P := (Pron, Pronoun);
         when Pack =>
            Get (Propack);
            P := (Pack, Propack);
         when Adj =>
            Get (Adjective);
            P := (Adj, Adjective);
         when Num =>
            Get (Numeral);
            P := (Num, Numeral);
         when Adv =>
            Get (Adverb);
            P := (Adv, Adverb);
         when V =>
            Get (Verb);
            P := (V, Verb);
         when X =>
            P := (Pofs => X);
      end case;
      return;
   end Get;

   procedure Put (F : in File_Type; P : in Target_Entry) is
      C : constant Positive := Positive (Col (F));
   begin
      Put (F, P.Pofs);
      Put (F, ' ');
      case P.Pofs is
         when N =>
            Put (F, P.N);
         when Pron =>
            Put (F, P.Pron);
         when Pack =>
            Put (F, P.Pack);
         when Adj =>
            Put (F, P.Adj);
         when Num =>
            Put (F, P.Num);
         when Adv =>
            Put (F, P.Adv);
         when V =>
            Put (F, P.V);
         when others =>
            null;
      end case;
      Put (F, String'((Integer (Col (F)) ..
        Target_Entry_Io.Default_Width + C - 1 => ' ')));
      return;
   end Put;

   procedure Put (P : in Target_Entry) is
      C : constant Positive := Positive (Col);
   begin
      Put (P.Pofs);
      Put (' ');
      case P.Pofs is
         when N =>
            Put (P.N);
         when Pron =>
            Put (P.Pron);
         when Pack =>
            Put (P.Pack);
         when Adj =>
            Put (P.Adj);
         when Num =>
            Put (P.Num);
         when Adv =>
            Put (P.Adv);
         when V =>
            Put (P.V);
         when others =>
            null;
      end case;
      Put (String'(
        (Integer (Col) .. Target_Entry_Io.Default_Width + C - 1 => ' ')));
      return;
   end Put;

   procedure Get (S : in String; P : out Target_Entry; Last : out Integer) is
      L : Integer := S'First - 1;
      Ps : Target_Pofs_Type := X;
   begin
      Get (S, Ps, L);
      L := L + 1;
      case Ps is
         when N =>
            Get (S (L + 1 .. S'Last), Noun, Last);
            P := (N, Noun);
         when Pron =>
            Get (S (L + 1 .. S'Last), Pronoun, Last);
            P := (Pron, Pronoun);
         when Pack =>
            Get (S (L + 1 .. S'Last), Propack, Last);
            P := (Pack, Propack);
         when Adj =>
            Get (S (L + 1 .. S'Last), Adjective, Last);
            P := (Adj, Adjective);
         when Num =>
            Get (S (L + 1 .. S'Last), Numeral, Last);
            P := (Num, Numeral);
         when Adv =>
            Get (S (L + 1 .. S'Last), Adverb, Last);
            P := (Adv, Adverb);
         when V =>
            Get (S (L + 1 .. S'Last), Verb, Last);
            P := (V, Verb);
         when X =>
            P := (Pofs => X);
      end case;
      return;
   end Get;

   procedure Put (S : out String; P : in Target_Entry) is
      L : Integer := S'First - 1;
      M : Integer := 0;
   begin
      M := L + Part_Of_Speech_Type_IO.Default_Width;
      Put (S (L + 1 .. M), P.Pofs);
      L := M + 1;
      S (L) :=  ' ';
      case P.Pofs is
         when N =>
            M := L + Noun_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.N);
         when Pron =>
            M := L + Pronoun_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.Pron);
         when Pack =>
            M := L + Propack_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.Pack);
         when Adj =>
            M := L + Adjective_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.Adj);
         when Num =>
            M := L + Numeral_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.Num);
         when Adv =>
            M := L + Adverb_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.Adv);
         when V =>
            M := L + Verb_Entry_IO.Default_Width;
            Put (S (L + 1 .. M), P.V);
         when others =>
            null;
      end case;
      S (M + 1 .. S'Last) := (others => ' ');
   end Put;
end Target_Entry_Io;
