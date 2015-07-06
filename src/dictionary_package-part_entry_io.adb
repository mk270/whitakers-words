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

separate (Dictionary_Package)
package body Part_Entry_IO is

   ---------------------------------------------------------------------------
   -- Throwaway variable used when reading Parse_Record for "getting rid" of
   -- not needed separator character between different fields of record.
   Spacer : Character := ' ';

   -- Variables used when reading Part_Entry and for constructing
   -- correct variant of Part_Entry.
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

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Part_Entry) is
      POFS         : Part_Of_Speech_Type := x;
      Starting_Col : constant Positive_Count := Col (File);
   begin
      Part_Of_Speech_Type_IO.Get (File, POFS);
      Get (File, Spacer);
      case POFS is
         when n =>
            Noun_Entry_IO.Get (File, Noun);
            Item := (n, Noun);
         when pron =>
            Pronoun_Entry_IO.Get (File, Pronoun);
            Item := (pron, Pronoun);
         when pack =>
            Propack_Entry_IO.Get (File, Propack);
            Item := (pack, Propack);
         when adj =>
            Adjective_Entry_IO.Get (File, Adjective);
            Item := (adj, Adjective);
         when num =>
            Numeral_Entry_IO.Get (File, Numeral);
            Item := (num, Numeral);
         when adv =>
            Adverb_Entry_IO.Get (File, Adverb);
            Item := (adv, Adverb);
         when v =>
            Verb_Entry_IO.Get (File, Verb);
            Item := (v, Verb);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Preposition_Entry_IO.Get (File, Preposition);
            Item := (prep, Preposition);
         when conj =>
            Conjunction_Entry_IO.Get (File, Conjunction);
            Item := (conj, Conjunction);
         when interj =>
            Interjection_Entry_IO.Get (File, Interjection);
            Item := (interj, Interjection);
         when prefix =>
            Item := (pofs => prefix);
         when suffix =>
            Item := (pofs => suffix);
         when tackon =>
            Item := (pofs => tackon);
         when x =>
            Item := (pofs => x);
      end case;
      Set_Col
         ( File,
           Positive_Count (Part_Entry_IO.Default_Width) + Starting_Col
         );
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Part_Entry) is
      POFS : Part_Of_Speech_Type := x;
   begin
      Part_Of_Speech_Type_IO.Get (POFS);
      Get (Spacer);
      case POFS is
         when n =>
            Noun_Entry_IO.Get (Noun);
            Item := (n, Noun);
         when pron =>
            Pronoun_Entry_IO.Get (Pronoun);
            Item := (pron, Pronoun);
         when pack =>
            Propack_Entry_IO.Get (Propack);
            Item := (pack, Propack);
         when adj =>
            Adjective_Entry_IO.Get (Adjective);
            Item := (adj, Adjective);
         when num =>
            Numeral_Entry_IO.Get (Numeral);
            Item := (num, Numeral);
         when adv =>
            Adverb_Entry_IO.Get (Adverb);
            Item := (adv, Adverb);
         when v =>
            Verb_Entry_IO.Get (Verb);
            Item := (v, Verb);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Preposition_Entry_IO.Get (Preposition);
            Item := (prep, Preposition);
         when conj =>
            Conjunction_Entry_IO.Get (Conjunction);
            Item := (conj, Conjunction);
         when interj =>
            Interjection_Entry_IO.Get (Interjection);
            Item := (interj, Interjection);
         when prefix =>
            Item := (pofs => prefix);
         when suffix =>
            Item := (pofs => suffix);
         when tackon =>
            Item := (pofs => tackon);
         when x =>
            Item := (pofs => x);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Part_Entry) is
   begin
      Part_Of_Speech_Type_IO.Put (File, Item.pofs);
      Put (File, ' ');
      case Item.pofs is
         when n =>
            Noun_Entry_IO.Put (File, Item.n);
         when pron =>
            Pronoun_Entry_IO.Put (File, Item.pron);
         when pack =>
            Propack_Entry_IO.Put (File, Item.pack);
         when adj =>
            Adjective_Entry_IO.Put (File, Item.adj);
         when num =>
            Numeral_Entry_IO.Put (File, Item.num);
         when adv =>
            Adverb_Entry_IO.Put (File, Item.adv);
         when v =>
            Verb_Entry_IO.Put (File, Item.v);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Preposition_Entry_IO.Put (File, Item.prep);
         when conj =>
            Conjunction_Entry_IO.Put (File, Item.conj);
         when interj =>
            Interjection_Entry_IO.Put (File, Item.interj);
         when x =>
            null;
         when tackon .. suffix =>
            null;
      end case;
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Part_Entry) is
   begin
      Part_Of_Speech_Type_IO.Put (Item.pofs);
      Put (' ');
      case Item.pofs is
         when n =>
            Noun_Entry_IO.Put (Item.n);
         when pron =>
            Pronoun_Entry_IO.Put (Item.pron);
         when pack =>
            Propack_Entry_IO.Put (Item.pack);
         when adj =>
            Adjective_Entry_IO.Put (Item.adj);
         when num =>
            Numeral_Entry_IO.Put (Item.num);
         when adv =>
            Adverb_Entry_IO.Put (Item.adv);
         when v =>
            Verb_Entry_IO.Put (Item.v);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Preposition_Entry_IO.Put (Item.prep);
         when conj =>
            Conjunction_Entry_IO.Put (Item.conj);
         when interj =>
            Interjection_Entry_IO.Put (Item.interj);
         when tackon .. suffix =>
            null;
         when x =>
            null;
      end case;
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      ( Source : in String;
        Target : out Part_Entry;
        Last   : out Integer
      )
   is
      -- Used to get lower bound of substring
      Low  : Integer := Source'First - 1;
      -- Used to know which variant of Part_Entry shall be constructed
      POFS : Part_Of_Speech_Type := x;
   begin
      Last := Low;   --  In case it is not set later
      Part_Of_Speech_Type_IO.Get (Source, POFS, Low);
      Low := Low + 1;
      case POFS is
         when n =>
            Noun_Entry_IO.Get (Source (Low + 1 .. Source'Last), Noun, Last);
            Target := (n, Noun);
         when pron =>
            Pronoun_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Pronoun, Last );
            Target := (pron, Pronoun);
         when pack =>
            Propack_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Propack, Last );
            Target := (pack, Propack);
         when adj =>
            Adjective_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Adjective, Last );
            Target := (adj, Adjective);
         when num =>
            Numeral_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Numeral, Last );
            Target := (num, Numeral);
         when adv =>
            Adverb_Entry_IO.Get (Source (Low + 1 .. Source'Last), Adverb, Last);
            Target := (adv, Adverb);
         when v =>
            Verb_Entry_IO.Get (Source (Low + 1 .. Source'Last), Verb, Last);
            Target := (v, Verb);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Preposition_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Preposition, Last );
            Target := (prep, Preposition);
         when conj =>
            Conjunction_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Conjunction, Last );
            Target := (conj, Conjunction);
         when interj =>
            Interjection_Entry_IO.Get
               ( Source (Low + 1 .. Source'Last), Interjection, Last );
            Target := (interj, Interjection);
         when prefix =>
            Target := (pofs => prefix);
         when suffix =>
            Target := (pofs => suffix);
         when tackon =>
            Target := (pofs => tackon);
         when x =>
            Target := (pofs => x);
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
      Part_Of_Speech_Type_IO.Put (Target (Low + 1 .. High), Item.pofs);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Part_Entry
      case Item.pofs is
         when n =>
            High := Low + Noun_Entry_IO.Default_Width;
            Noun_Entry_IO.Put (Target (Low + 1 .. High), Item.n);
         when pron =>
            High := Low + Pronoun_Entry_IO.Default_Width;
            Pronoun_Entry_IO.Put (Target (Low + 1 .. High), Item.pron);
         when pack =>
            High := Low + Propack_Entry_IO.Default_Width;
            Propack_Entry_IO.Put (Target (Low + 1 .. High), Item.pack);
         when adj =>
            High := Low + Adjective_Entry_IO.Default_Width;
            Adjective_Entry_IO.Put (Target (Low + 1 .. High), Item.adj);
         when num =>
            High := Low + Numeral_Entry_IO.Default_Width;
            Numeral_Entry_IO.Put (Target (Low + 1 .. High), Item.num);
         when adv =>
            High := Low + Adverb_Entry_IO.Default_Width;
            Adverb_Entry_IO.Put (Target (Low + 1 .. High), Item.adv);
         when v =>
            High := Low + Verb_Entry_IO.Default_Width;
            Verb_Entry_IO.Put (Target (Low + 1 .. High), Item.v);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            High := Low + Preposition_Entry_IO.Default_Width;
            Preposition_Entry_IO.Put (Target (Low + 1 .. High), Item.prep);
         when conj =>
            High := Low + Conjunction_Entry_IO.Default_Width;
            Conjunction_Entry_IO.Put (Target (Low + 1 .. High), Item.conj);
         when interj =>
            High := Low + Interjection_Entry_IO.Default_Width;
            Interjection_Entry_IO.Put (Target (Low + 1 .. High), Item.interj);
         when x =>
            null;
         when tackon .. suffix =>
            null;
      end case;
   end Put;

   ---------------------------------------------------------------------------

end Part_Entry_IO;
