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
   use Part_Of_Speech_Type_IO;
   use Noun_Entry_IO;
   use Pronoun_Entry_IO;
   use Propack_Entry_IO;
   use Adjective_Entry_IO;
   use Numeral_Entry_IO;
   use Adverb_Entry_IO;
   use Verb_Entry_IO;
   use Preposition_Entry_IO;
   use Conjunction_Entry_IO;
   use Interjection_Entry_IO;

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
      Get (File, POFS);
      Get (File, Spacer);
      case POFS is
         when n =>
            Noun_Entry_IO.Get (File, Noun);
            Item := (n, Noun);
         when pron =>
            Get (File, Pronoun);
            Item := (pron, Pronoun);
         when pack =>
            Get (File, Propack);
            Item := (pack, Propack);
         when adj =>
            Get (File, Adjective);
            Item := (adj, Adjective);
         when num =>
            Get (File, Numeral);
            Item := (num, Numeral);
         when adv =>
            Get (File, Adverb);
            Item := (adv, Adverb);
         when v =>
            Get (File, Verb);
            Item := (v, Verb);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Get (File, Preposition);
            Item := (prep, Preposition);
         when conj =>
            Get (File, Conjunction);
            Item := (conj, Conjunction);
         when interj =>
            Get (File, Interjection);
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
      Get (POFS);
      Get (Spacer);
      case POFS is
         when n =>
            Noun_Entry_IO.Get (Noun);
            Item := (n, Noun);
         when pron =>
            Get (Pronoun);
            Item := (pron, Pronoun);
         when pack =>
            Get (Propack);
            Item := (pack, Propack);
         when adj =>
            Get (Adjective);
            Item := (adj, Adjective);
         when num =>
            Get (Numeral);
            Item := (num, Numeral);
         when adv =>
            Get (Adverb);
            Item := (adv, Adverb);
         when v =>
            Get (Verb);
            Item := (v, Verb);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Get (Preposition);
            Item := (prep, Preposition);
         when conj =>
            Get (Conjunction);
            Item := (conj, Conjunction);
         when interj =>
            Get (Interjection);
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
      Put (File, Item.pofs);
      Put (File, ' ');
      case Item.pofs is
         when n =>
            Noun_Entry_IO.Put (File, Item.n);
         when pron =>
            Put (File, Item.pron);
         when pack =>
            Put (File, Item.pack);
         when adj =>
            Put (File, Item.adj);
         when num =>
            Put (File, Item.num);
         when adv =>
            Put (File, Item.adv);
         when v =>
            Put (File, Item.v);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Put (File, Item.prep);
         when conj =>
            Put (File, Item.conj);
         when interj =>
            Put (File, Item.interj);
         when others =>
            null;
      end case;
      --PUT(F, STRING'((INTEGER(COL(F))..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Part_Entry) is
   begin
      Put (Item.pofs);
      Put (' ');
      case Item.pofs is
         when n =>
            Noun_Entry_IO.Put (Item.n);
         when pron =>
            Put (Item.pron);
         when pack =>
            Put (Item.pack);
         when adj =>
            Put (Item.adj);
         when num =>
            Put (Item.num);
         when adv =>
            Put (Item.adv);
         when v =>
            Put (Item.v);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Put (Item.prep);
         when conj =>
            Put (Item.conj);
         when interj =>
            Put (Item.interj);
         when others =>
            null;
      end case;
      --PUT(STRING'((INTEGER(COL)..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
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
      Get (Source, POFS, Low);
      Low := Low + 1;
      case POFS is
         when n =>
            Noun_Entry_IO.Get (Source (Low + 1 .. Source'Last), Noun, Last);
            Target := (n, Noun);
         when pron =>
            Get (Source (Low + 1 .. Source'Last), Pronoun, Last);
            Target := (pron, Pronoun);
         when pack =>
            Get (Source (Low + 1 .. Source'Last), Propack, Last);
            Target := (pack, Propack);
         when adj =>
            Get (Source (Low + 1 .. Source'Last), Adjective, Last);
            Target := (adj, Adjective);
         when num =>
            Get (Source (Low + 1 .. Source'Last), Numeral, Last);
            Target := (num, Numeral);
         when adv =>
            Get (Source (Low + 1 .. Source'Last), Adverb, Last);
            Target := (adv, Adverb);
         when v =>
            Get (Source (Low + 1 .. Source'Last), Verb, Last);
            Target := (v, Verb);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            Get (Source (Low + 1 .. Source'Last), Preposition, Last);
            Target := (prep, Preposition);
         when conj =>
            Get (Source (Low + 1 .. Source'Last), Conjunction, Last);
            Target := (conj, Conjunction);
         when interj =>
            Get (Source (Low + 1 .. Source'Last), Interjection, Last);
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
      High := Low + Part_Of_Speech_Type_IO.Default_Width;
      Put (Target (Low + 1 .. High), Item.pofs);
      Low := High + 1;
      Target (Low) :=  ' ';
      case Item.pofs is
         when n =>
            High := Low + Noun_Entry_IO.Default_Width;
            Noun_Entry_IO.Put (Target (Low + 1 .. High), Item.n);
         when pron =>
            High := Low + Pronoun_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.pron);
         when pack =>
            High := Low + Propack_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.pack);
         when adj =>
            High := Low + Adjective_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.adj);
         when num =>
            High := Low + Numeral_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.num);
         when adv =>
            High := Low + Adverb_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.adv);
         when v =>
            High := Low + Verb_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.v);
         when vpar =>
            null;                --  No VAPR entry
         when supine =>
            null;                --  No SUPINE entry
         when prep =>
            High := Low + Preposition_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.prep);
         when conj =>
            High := Low + Conjunction_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.conj);
         when interj =>
            High := Low + Interjection_Entry_IO.Default_Width;
            Put (Target (Low + 1 .. High), Item.interj);
         when others =>
            null;
      end case;
      --S(M+1..S'LAST) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Part_Entry_IO;
