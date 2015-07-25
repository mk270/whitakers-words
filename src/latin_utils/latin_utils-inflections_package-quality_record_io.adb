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

separate (Latin_Utils.Inflections_Package)
package body Quality_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Quality_Record)
   is
      POFS     : Part_Of_Speech_Type := X;
      Spacer   : Character := ' ';
      pragma Unreferenced (Spacer);
      Noun     : Noun_Record;
      Pronoun  : Pronoun_Record;
      Propack  : Propack_Record;
      Adjective : Adjective_Record;
      Adverb   : Adverb_Record;
      Verb     : Verb_Record;
      Vparticiple : Vpar_Record;
      Supin    : Supine_Record;
      Preposition : Preposition_Record;
      Conjunction : Conjunction_Record;
      Interjection : Interjection_Record;
      Numeral  : Numeral_Record;
      Tackn    : Tackon_Record;
      Prefx    : Prefix_Record;
      Suffx    : Suffix_Record;
   begin
      Part_Of_Speech_Type_IO.Get (File, POFS);
      Get (File, Spacer);
      case POFS is
         when N =>
            Noun_Record_IO.Get (File, Noun);
            Item := (N, Noun);
         when Pron =>
            Pronoun_Record_IO.Get (File, Pronoun);
            Item := (Pron, Pronoun);
         when Pack =>
            Propack_Record_IO.Get (File, Propack);
            Item := (Pack, Propack);
         when Adj =>
            Adjective_Record_IO.Get (File, Adjective);
            Item := (Adj, Adjective);
         when Num =>
            Numeral_Record_IO.Get (File, Numeral);
            Item := (Num, Numeral);
         when Adv =>
            Adverb_Record_IO.Get (File, Adverb);
            Item := (Adv, Adverb);
         when V =>
            Verb_Record_IO.Get (File, Verb);
            Item := (V, Verb);
         when Vpar =>
            Vpar_Record_IO.Get (File, Vparticiple);
            Item := (Vpar, Vparticiple);
         when Supine =>
            Supine_Record_IO.Get (File, Supin);
            Item := (Supine, Supin);
         when Prep =>
            Preposition_Record_IO.Get (File, Preposition);
            Item := (Prep, Preposition);
         when Conj =>
            Conjunction_Record_IO.Get (File, Conjunction);
            Item := (Conj, Conjunction);
         when Interj =>
            Interjection_Record_IO.Get (File, Interjection);
            Item := (Interj, Interjection);
         when Tackon =>
            Tackon_Record_IO.Get (File, Tackn);
            Item := (Tackon, Tackn);
         when Prefix =>
            Prefix_Record_IO.Get (File, Prefx);
            Item := (Prefix, Prefx);
         when Suffix =>
            Suffix_Record_IO.Get (File, Suffx);
            Item := (Suffix, Suffx);
         when X =>
            Item := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Quality_Record)
   is
      POFS     : Part_Of_Speech_Type := X;
      Spacer   : Character := ' ';
      pragma Unreferenced (Spacer);
      Noun     : Noun_Record;
      Pronoun  : Pronoun_Record;
      Propack  : Propack_Record;
      Adjective : Adjective_Record;
      Adverb   : Adverb_Record;
      Verb     : Verb_Record;
      Vparticiple : Vpar_Record;
      Supin    : Supine_Record;
      Preposition : Preposition_Record;
      Conjunction : Conjunction_Record;
      Interjection : Interjection_Record;
      Numeral  : Numeral_Record;
      Tackn    : Tackon_Record;
      Prefx    : Prefix_Record;
      Suffx    : Suffix_Record;
   begin
      Part_Of_Speech_Type_IO.Get (POFS);
      Get (Spacer);
      case POFS is
         when N =>
            Noun_Record_IO.Get (Noun);
            Item := (N, Noun);
         when Pron =>
            Pronoun_Record_IO.Get (Pronoun);
            Item := (Pron, Pronoun);
         when Pack =>
            Propack_Record_IO.Get (Propack);
            Item := (Pack, Propack);
         when Adj =>
            Adjective_Record_IO.Get (Adjective);
            Item := (Adj, Adjective);
         when Num =>
            Numeral_Record_IO.Get (Numeral);
            Item := (Num, Numeral);
         when Adv =>
            Adverb_Record_IO.Get (Adverb);
            Item := (Adv, Adverb);
         when V =>
            Verb_Record_IO.Get (Verb);
            Item := (V, Verb);
         when Vpar =>
            Vpar_Record_IO.Get (Vparticiple);
            Item := (Vpar, Vparticiple);
         when Supine =>
            Supine_Record_IO.Get (Supin);
            Item := (Supine, Supin);
         when Prep =>
            Preposition_Record_IO.Get (Preposition);
            Item := (Prep, Preposition);
         when Conj =>
            Conjunction_Record_IO.Get (Conjunction);
            Item := (Conj, Conjunction);
         when Interj =>
            Interjection_Record_IO.Get (Interjection);
            Item := (Interj, Interjection);
         when Tackon =>
            Tackon_Record_IO.Get (Tackn);
            Item := (Tackon, Tackn);
         when Prefix =>
            Prefix_Record_IO.Get (Prefx);
            Item := (Prefix, Prefx);
         when Suffix =>
            Suffix_Record_IO.Get (Suffx);
            Item := (Suffix, Suffx);
         when X =>
            Item := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Quality_Record)
   is
      C : constant Positive := Positive (Col (File));
   begin
      Part_Of_Speech_Type_IO.Put (File, Item.Pofs);
      Put (File, ' ');
      case Item.Pofs is
         when N =>
            Noun_Record_IO.Put      (File, Item.Noun);
         when Pron =>
            Pronoun_Record_IO.Put   (File, Item.Pron);
         when Pack =>
            Propack_Record_IO.Put   (File, Item.Pack);
         when Adj =>
            Adjective_Record_IO.Put (File, Item.Adj);
         when Num =>
            Numeral_Record_IO.Put   (File, Item.Num);
         when Adv =>
            Adverb_Record_IO.Put   (File, Item.Adv);
         when V =>
            Verb_Record_IO.Put      (File, Item.Verb);
         when Vpar =>
            Vpar_Record_IO.Put      (File, Item.Vpar);
         when Supine =>
            Supine_Record_IO.Put    (File, Item.Supine);
         when Prep =>
            Preposition_Record_IO.Put (File, Item.Prep);
         when Conj =>
            Conjunction_Record_IO.Put (File, Item.Conj);
         when Interj =>
            Interjection_Record_IO.Put (File, Item.Interj);
         when Tackon =>
            Tackon_Record_IO.Put    (File, Item.Tackon);
         when Prefix =>
            Prefix_Record_IO.Put    (File, Item.Prefix);
         when Suffix =>
            Suffix_Record_IO.Put    (File, Item.Suffix);
         when others =>
            null;
      end case;
      Put (File, String'(
        Integer (Col (File)) .. Quality_Record_IO.Default_Width + C - 1
        => ' '));
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Quality_Record)
   is
      C : constant Positive := Positive (Col);
   begin
      Part_Of_Speech_Type_IO.Put (Item.Pofs);
      Put (' ');
      case Item.Pofs is
         when N =>
            Noun_Record_IO.Put      (Item.Noun);
         when Pron =>
            Pronoun_Record_IO.Put   (Item.Pron);
         when Pack =>
            Propack_Record_IO.Put   (Item.Pack);
         when Adj =>
            Adjective_Record_IO.Put (Item.Adj);
         when Num =>
            Numeral_Record_IO.Put   (Item.Num);
         when Adv =>
            Adverb_Record_IO.Put    (Item.Adv);
         when V =>
            Verb_Record_IO.Put      (Item.Verb);
         when Vpar =>
            Vpar_Record_IO.Put      (Item.Vpar);
         when Supine =>
            Supine_Record_IO.Put    (Item.Supine);
         when Prep =>
            Preposition_Record_IO.Put (Item.Prep);
         when Conj =>
            Conjunction_Record_IO.Put (Item.Conj);
         when Interj =>
            Interjection_Record_IO.Put (Item.Interj);
         when Tackon =>
            Tackon_Record_IO.Put    (Item.Tackon);
         when Prefix =>
            Prefix_Record_IO.Put    (Item.Prefix);
         when Suffix =>
            Suffix_Record_IO.Put    (Item.Suffix);
         when others =>
            null;
      end case;
      Put (String'((
        Integer (Col) .. Quality_Record_IO.Default_Width + C - 1 => ' ')));
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Quality_Record;
       Last   : out Integer
      )
   is
      -- Used for computing lower bounds of substrings
      Low  : Integer := Source'First - 1;
      POFS : Part_Of_Speech_Type := X;
      Noun     : Noun_Record;
      Pronoun  : Pronoun_Record;
      Propack  : Propack_Record;
      Adjective : Adjective_Record;
      Adverb   : Adverb_Record;
      Verb     : Verb_Record;
      Vparticiple : Vpar_Record;
      Supin    : Supine_Record;
      Preposition : Preposition_Record;
      Conjunction : Conjunction_Record;
      Interjection : Interjection_Record;
      Numeral  : Numeral_Record;
      Tackn    : Tackon_Record;
      Prefx    : Prefix_Record;
      Suffx    : Suffix_Record;
   begin
      Part_Of_Speech_Type_IO.Get (Source, POFS, Low);
      Last := Low;         --  In case it is not set later
      case POFS is
         when N =>
            Noun_Record_IO.Get (Source (Low + 1 .. Source'Last), Noun, Last);
            Target := (N, Noun);
         when Pron =>
            Pronoun_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Pronoun, Last);
            Target := (Pron, Pronoun);
         when Pack =>
            Propack_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Propack, Last);
            Target := (Pack, Propack);
         when Adj =>
            Adjective_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Adjective, Last);
            Target := (Adj, Adjective);
         when Num =>
            Numeral_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Numeral, Last);
            Target := (Num, Numeral);
         when Adv =>
            Adverb_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Adverb, Last);
            Target := (Adv, Adverb);
         when V =>
            Verb_Record_IO.Get (Source (Low + 1 .. Source'Last), Verb, Last);
            Target := (V, Verb);
         when Vpar =>
            Vpar_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Vparticiple, Last);
            Target := (Vpar, Vparticiple);
         when Supine =>
            Supine_Record_IO.Get (Source (Low + 1 .. Source'Last), Supin, Last);
            Target := (Supine, Supin);
         when Prep =>
            Preposition_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Preposition, Last);
            Target := (Prep, Preposition);
         when Conj =>
            Conjunction_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Conjunction, Last);
            Target := (Conj, Conjunction);
         when Interj =>
            Interjection_Record_IO.Get
               (Source (Low + 1 .. Source'Last), Interjection, Last);
            Target := (Interj, Interjection);
         when Tackon =>
            Tackon_Record_IO.Get (Source (Low + 1 .. Source'Last), Tackn, Last);
            Target := (Tackon, Tackn);
         when Prefix =>
            Prefix_Record_IO.Get (Source (Low + 1 .. Source'Last), Prefx, Last);
            Target := (Prefix, Prefx);
         when Suffix =>
            Suffix_Record_IO.Get (Source (Low + 1 .. Source'Last), Suffx, Last);
            Target := (Suffix, Suffx);
         when X =>
            Target := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Quality_Record)
   is
      --  Note that this does not Put with a uniform width
      --  which would require a constant QUALITY_RECORD_IO.DEFAULT_WIDTH
      --  Rather we Put to minimal size with NOUN_RECORD_IO.DEFAULT_WIDTH,
      --  PRONOUN_RECORD_IO,DEFAULT_WIDTH, . ..

      -- Used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      High := Low + Part_Of_Speech_Type_IO.Default_Width;
      Part_Of_Speech_Type_IO.Put (Target (Low + 1 .. High), Item.Pofs);
      Low := High + 1;
      Target (Low) :=  ' ';
      case Item.Pofs is
         when N =>
            High := Low + Noun_Record_IO.Default_Width;
            Noun_Record_IO.Put (Target (Low + 1 .. High), Item.Noun);
         when Pron =>
            High := Low + Pronoun_Record_IO.Default_Width;
            Pronoun_Record_IO.Put (Target (Low + 1 .. High), Item.Pron);
         when Pack =>
            High := Low + Propack_Record_IO.Default_Width;
            Propack_Record_IO.Put (Target (Low + 1 .. High), Item.Pack);
         when Adj =>
            High := Low + Adjective_Record_IO.Default_Width;
            Adjective_Record_IO.Put (Target (Low + 1 .. High), Item.Adj);
         when Num =>
            High := Low + Numeral_Record_IO.Default_Width;
            Numeral_Record_IO.Put (Target (Low + 1 .. High), Item.Num);
         when Adv =>
            High := Low + Adverb_Record_IO.Default_Width;
            Adverb_Record_IO.Put (Target (Low + 1 .. High), Item.Adv);
         when V =>
            High := Low + Verb_Record_IO.Default_Width;
            Verb_Record_IO.Put (Target (Low + 1 .. High), Item.Verb);
         when Vpar =>
            High := Low + Vpar_Record_IO.Default_Width;
            Vpar_Record_IO.Put (Target (Low + 1 .. High), Item.Vpar);
         when Supine =>
            High := Low + Supine_Record_IO.Default_Width;
            Supine_Record_IO.Put (Target (Low + 1 .. High), Item.Supine);
         when Prep =>
            High := Low + Preposition_Record_IO.Default_Width;
            Preposition_Record_IO.Put (Target (Low + 1 .. High), Item.Prep);
         when Conj =>
            High := Low + Conjunction_Record_IO.Default_Width;
            Conjunction_Record_IO.Put (Target (Low + 1 .. High), Item.Conj);
         when Interj =>
            High := Low + Interjection_Record_IO.Default_Width;
            Interjection_Record_IO.Put (Target (Low + 1 .. High), Item.Interj);
         when Tackon =>
            High := Low + Tackon_Record_IO.Default_Width;
            Tackon_Record_IO.Put (Target (Low + 1 .. High), Item.Tackon);
         when Prefix =>
            High := Low + Prefix_Record_IO.Default_Width;
            Prefix_Record_IO.Put (Target (Low + 1 .. High), Item.Prefix);
         when Suffix =>
            High := Low + Suffix_Record_IO.Default_Width;
            Suffix_Record_IO.Put (Target (Low + 1 .. High), Item.Suffix);
         when others =>
            null;
      end case;

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Quality_Record_IO;
