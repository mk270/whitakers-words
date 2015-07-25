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

separate (Latin_Utils.Dictionary_Package)
package body Part_Entry_IO is

   ---------------------------------------------------------------------------

   use type Ada.Text_IO.Positive_Count;

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Part_Entry)
   is
      POFS         : Part_Of_Speech_Type := X;
      Starting_Col : constant Ada.Text_IO.Positive_Count :=
         Ada.Text_IO.Col (File);
      Spacer : Character;
      pragma Unreferenced (Spacer);

      --------------------------------------------------------------------------

      Noun           : Noun_Entry;
      Pronoun        : Pronoun_Entry;
      Propack        : Propack_Entry;
      Adjective      : Adjective_Entry;
      Numeral        : Numeral_Entry;
      Adverb         : Adverb_Entry;
      Verb           : Verb_Entry;
      Preposition    : Preposition_Entry;
      Conjunction    : Conjunction_Entry;
      Interjection   : Interjection_Entry;

      --------------------------------------------------------------------------

   begin
      Part_Of_Speech_Type_IO.Get (File, POFS);
      Ada.Text_IO.Get (File, Spacer);
      case POFS is
         when N =>
            Noun_Entry_IO.Get (File, Noun);
            Item := (N, Noun);
         when Pron =>
            Pronoun_Entry_IO.Get (File, Pronoun);
            Item := (Pron, Pronoun);
         when Pack =>
            Propack_Entry_IO.Get (File, Propack);
            Item := (Pack, Propack);
         when Adj =>
            Adjective_Entry_IO.Get (File, Adjective);
            Item := (Adj, Adjective);
         when Num =>
            Numeral_Entry_IO.Get (File, Numeral);
            Item := (Num, Numeral);
         when Adv =>
            Adverb_Entry_IO.Get (File, Adverb);
            Item := (Adv, Adverb);
         when V =>
            Verb_Entry_IO.Get (File, Verb);
            Item := (V, Verb);
         when Vpar =>
            null;                --  No VAPR entry
         when Supine =>
            null;                --  No SUPINE entry
         when Prep =>
            Preposition_Entry_IO.Get (File, Preposition);
            Item := (Prep, Preposition);
         when Conj =>
            Conjunction_Entry_IO.Get (File, Conjunction);
            Item := (Conj, Conjunction);
         when Interj =>
            Interjection_Entry_IO.Get (File, Interjection);
            Item := (Interj, Interjection);
         when Prefix =>
            Item := (Pofs => Prefix);
         when Suffix =>
            Item := (Pofs => Suffix);
         when Tackon =>
            Item := (Pofs => Tackon);
         when X =>
            Item := (Pofs => X);
      end case;
      Ada.Text_IO.Set_Col
         (File,
           Ada.Text_IO.Positive_Count
              (Part_Entry_IO.Default_Width) + Starting_Col
        );
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Part_Entry)
   is
      POFS : Part_Of_Speech_Type := X;
      Spacer : Character;
      pragma Unreferenced (Spacer);

      --------------------------------------------------------------------------

      Noun           : Noun_Entry;
      Pronoun        : Pronoun_Entry;
      Propack        : Propack_Entry;
      Adjective      : Adjective_Entry;
      Numeral        : Numeral_Entry;
      Adverb         : Adverb_Entry;
      Verb           : Verb_Entry;
      Preposition    : Preposition_Entry;
      Conjunction    : Conjunction_Entry;
      Interjection   : Interjection_Entry;

      --------------------------------------------------------------------------

   begin
      Part_Of_Speech_Type_IO.Get (POFS);
      Ada.Text_IO.Get (Spacer);
      case POFS is
         when N =>
            Noun_Entry_IO.Get (Noun);
            Item := (N, Noun);
         when Pron =>
            Pronoun_Entry_IO.Get (Pronoun);
            Item := (Pron, Pronoun);
         when Pack =>
            Propack_Entry_IO.Get (Propack);
            Item := (Pack, Propack);
         when Adj =>
            Adjective_Entry_IO.Get (Adjective);
            Item := (Adj, Adjective);
         when Num =>
            Numeral_Entry_IO.Get (Numeral);
            Item := (Num, Numeral);
         when Adv =>
            Adverb_Entry_IO.Get (Adverb);
            Item := (Adv, Adverb);
         when V =>
            Verb_Entry_IO.Get (Verb);
            Item := (V, Verb);
         when Vpar =>
            null;                --  No VAPR entry
         when Supine =>
            null;                --  No SUPINE entry
         when Prep =>
            Preposition_Entry_IO.Get (Preposition);
            Item := (Prep, Preposition);
         when Conj =>
            Conjunction_Entry_IO.Get (Conjunction);
            Item := (Conj, Conjunction);
         when Interj =>
            Interjection_Entry_IO.Get (Interjection);
            Item := (Interj, Interjection);
         when Prefix =>
            Item := (Pofs => Prefix);
         when Suffix =>
            Item := (Pofs => Suffix);
         when Tackon =>
            Item := (Pofs => Tackon);
         when X =>
            Item := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Part_Entry) is
   begin
      Part_Of_Speech_Type_IO.Put (File, Item.Pofs);
      Ada.Text_IO.Put (File, ' ');
      case Item.Pofs is
         when N =>
            Noun_Entry_IO.Put (File, Item.N);
         when Pron =>
            Pronoun_Entry_IO.Put (File, Item.Pron);
         when Pack =>
            Propack_Entry_IO.Put (File, Item.Pack);
         when Adj =>
            Adjective_Entry_IO.Put (File, Item.Adj);
         when Num =>
            Numeral_Entry_IO.Put (File, Item.Num);
         when Adv =>
            Adverb_Entry_IO.Put (File, Item.Adv);
         when V =>
            Verb_Entry_IO.Put (File, Item.V);
         when Vpar =>
            null;                --  No VAPR entry
         when Supine =>
            null;                --  No SUPINE entry
         when Prep =>
            Preposition_Entry_IO.Put (File, Item.Prep);
         when Conj =>
            Conjunction_Entry_IO.Put (File, Item.Conj);
         when Interj =>
            Interjection_Entry_IO.Put (File, Item.Interj);
         when X =>
            null;
         when Tackon .. Suffix =>
            null;
      end case;
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Part_Entry) is
   begin
      Part_Of_Speech_Type_IO.Put (Item.Pofs);
      Ada.Text_IO.Put (' ');
      case Item.Pofs is
         when N =>
            Noun_Entry_IO.Put (Item.N);
         when Pron =>
            Pronoun_Entry_IO.Put (Item.Pron);
         when Pack =>
            Propack_Entry_IO.Put (Item.Pack);
         when Adj =>
            Adjective_Entry_IO.Put (Item.Adj);
         when Num =>
            Numeral_Entry_IO.Put (Item.Num);
         when Adv =>
            Adverb_Entry_IO.Put (Item.Adv);
         when V =>
            Verb_Entry_IO.Put (Item.V);
         when Vpar =>
            null;                --  No VAPR entry
         when Supine =>
            null;                --  No SUPINE entry
         when Prep =>
            Preposition_Entry_IO.Put (Item.Prep);
         when Conj =>
            Conjunction_Entry_IO.Put (Item.Conj);
         when Interj =>
            Interjection_Entry_IO.Put (Item.Interj);
         when Tackon .. Suffix =>
            null;
         when X =>
            null;
      end case;
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in String;
        Target : out Part_Entry;
        Last   : out Integer
     )
   is
      -- Used to get lower bound of substring
      Low  : Integer := Source'First - 1;
      -- Used to know which variant of Part_Entry shall be constructed
      POFS : Part_Of_Speech_Type := X;

      --------------------------------------------------------------------------

      Noun           : Noun_Entry;
      Pronoun        : Pronoun_Entry;
      Propack        : Propack_Entry;
      Adjective      : Adjective_Entry;
      Numeral        : Numeral_Entry;
      Adverb         : Adverb_Entry;
      Verb           : Verb_Entry;
      Preposition    : Preposition_Entry;
      Conjunction    : Conjunction_Entry;
      Interjection   : Interjection_Entry;

      --------------------------------------------------------------------------

   begin
      Last := Low;   --  In case it is not set later
      Part_Of_Speech_Type_IO.Get (Source, POFS, Low);
      Low := Low + 1;
      case POFS is
         when N =>
            Noun_Entry_IO.Get (Source (Low + 1 .. Source'Last), Noun, Last);
            Target := (N, Noun);
         when Pron =>
            Pronoun_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Pronoun, Last);
            Target := (Pron, Pronoun);
         when Pack =>
            Propack_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Propack, Last);
            Target := (Pack, Propack);
         when Adj =>
            Adjective_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Adjective, Last);
            Target := (Adj, Adjective);
         when Num =>
            Numeral_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Numeral, Last);
            Target := (Num, Numeral);
         when Adv =>
            Adverb_Entry_IO.Get (Source (Low + 1 .. Source'Last), Adverb, Last);
            Target := (Adv, Adverb);
         when V =>
            Verb_Entry_IO.Get (Source (Low + 1 .. Source'Last), Verb, Last);
            Target := (V, Verb);
         when Vpar =>
            null;                --  No VAPR entry
         when Supine =>
            null;                --  No SUPINE entry
         when Prep =>
            Preposition_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Preposition, Last);
            Target := (Prep, Preposition);
         when Conj =>
            Conjunction_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Conjunction, Last);
            Target := (Conj, Conjunction);
         when Interj =>
            Interjection_Entry_IO.Get
               (Source (Low + 1 .. Source'Last), Interjection, Last);
            Target := (Interj, Interjection);
         when Prefix =>
            Target := (Pofs => Prefix);
         when Suffix =>
            Target := (Pofs => Suffix);
         when Tackon =>
            Target := (Pofs => Tackon);
         when X =>
            Target := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Part_Entry) is
      -- Used to get bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Part_Of_Speech_Type
      High := Low + Part_Of_Speech_Type_IO.Default_Width;
      Part_Of_Speech_Type_IO.Put (Target (Low + 1 .. High), Item.Pofs);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Part_Entry
      case Item.Pofs is
         when N =>
            High := Low + Noun_Entry_IO.Default_Width;
            Noun_Entry_IO.Put (Target (Low + 1 .. High), Item.N);
         when Pron =>
            High := Low + Pronoun_Entry_IO.Default_Width;
            Pronoun_Entry_IO.Put (Target (Low + 1 .. High), Item.Pron);
         when Pack =>
            High := Low + Propack_Entry_IO.Default_Width;
            Propack_Entry_IO.Put (Target (Low + 1 .. High), Item.Pack);
         when Adj =>
            High := Low + Adjective_Entry_IO.Default_Width;
            Adjective_Entry_IO.Put (Target (Low + 1 .. High), Item.Adj);
         when Num =>
            High := Low + Numeral_Entry_IO.Default_Width;
            Numeral_Entry_IO.Put (Target (Low + 1 .. High), Item.Num);
         when Adv =>
            High := Low + Adverb_Entry_IO.Default_Width;
            Adverb_Entry_IO.Put (Target (Low + 1 .. High), Item.Adv);
         when V =>
            High := Low + Verb_Entry_IO.Default_Width;
            Verb_Entry_IO.Put (Target (Low + 1 .. High), Item.V);
         when Vpar =>
            null;                --  No VAPR entry
         when Supine =>
            null;                --  No SUPINE entry
         when Prep =>
            High := Low + Preposition_Entry_IO.Default_Width;
            Preposition_Entry_IO.Put (Target (Low + 1 .. High), Item.Prep);
         when Conj =>
            High := Low + Conjunction_Entry_IO.Default_Width;
            Conjunction_Entry_IO.Put (Target (Low + 1 .. High), Item.Conj);
         when Interj =>
            High := Low + Interjection_Entry_IO.Default_Width;
            Interjection_Entry_IO.Put (Target (Low + 1 .. High), Item.Interj);
         when X =>
            null;
         when Tackon .. Suffix =>
            null;
      end case;
   end Put;

   ---------------------------------------------------------------------------

end Part_Entry_IO;
