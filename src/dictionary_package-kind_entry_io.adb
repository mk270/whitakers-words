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
package body Kind_Entry_IO is

   ---------------------------------------------------------------------------
   -- Variables used when reading Part_Entry and for constructing
   -- correct variant of Part_Entry.
   Noun_Kind      : Noun_Kind_Type;
   Pronoun_Kind   : Pronoun_Kind_Type;
   Propack_Kind   : Pronoun_Kind_Type;
   Verb_Kind      : Verb_Kind_Type;
   Vpar_Kind      : Verb_Kind_Type;
   Supine_Kind    : Verb_Kind_Type;
   Numeral_Value  : Numeral_Value_Type;

   ---------------------------------------------------------------------------

   procedure Get
      ( File : in  File_Type;
        POFS : in  Part_Of_Speech_Type;
        Item : out Kind_Entry
      )
   is

      --------------------------------------------------------------------------
      -- Small helper procedure
      procedure Set_Col (File: Ada.Text_IO.File_Type) is
      begin
         Ada.Text_IO.Set_Col
            ( File,
              Col (File) + Positive_Count (Kind_Entry_IO.Default_Width)
            );
      end Set_Col;

      --------------------------------------------------------------------------

   begin
      case POFS is
         when n =>
            Noun_Kind_Type_IO.Get (File, Noun_Kind);
            Item := (n, Noun_Kind);
         when pron =>
            Pronoun_Kind_Type_IO.Get (File, Pronoun_Kind);
            Item := (pron, Pronoun_Kind);
         when pack =>
            Pronoun_Kind_Type_IO.Get (File, Propack_Kind);
            Item := (pack, Propack_Kind);
         when adj =>
            Set_Col (File);
            Item := (pofs => adj);
         when num =>
            Inflections_Package.Integer_IO.Get (File, Numeral_Value);
            Item := (num, Numeral_Value);
         when adv =>
            Set_Col (File);
            Item := (pofs => adv);
         when v =>
            Verb_Kind_Type_IO.Get (File, Verb_Kind);
            Item := (v, Verb_Kind);
         when vpar =>
            Verb_Kind_Type_IO.Get (File, Vpar_Kind);
            Item := (vpar, Vpar_Kind);
         when supine =>
            Verb_Kind_Type_IO.Get (File, Supine_Kind);
            Item := (supine, Supine_Kind);
         when prep =>
            Set_Col (File);
            Item := (pofs => prep);
         when conj =>
            Set_Col (File);
            Item := (pofs => conj);
         when interj =>
            Set_Col (File);
            Item := (pofs => interj);
         when tackon =>
            Set_Col (File);
            Item := (pofs => tackon);
         when prefix =>
            Set_Col (File);
            Item := (pofs => prefix);
         when suffix =>
            Set_Col (File);
            Item := (pofs => suffix);
         when x =>
            Set_Col (File);
            Item := (pofs => x);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (POFS : in Part_Of_Speech_Type; Item : out Kind_Entry) is
   begin
      case POFS is
         when n =>
            Noun_Kind_Type_IO.Get (Noun_Kind);
            Item := (n, Noun_Kind);
         when pron =>
            Pronoun_Kind_Type_IO.Get (Pronoun_Kind);
            Item := (pron, Pronoun_Kind);
         when pack =>
            Pronoun_Kind_Type_IO.Get (Propack_Kind);
            Item := (pack, Propack_Kind);
         when adj =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => adj);
         when num =>
            Inflections_Package.Integer_IO.Get (Numeral_Value);
            Item := (num, Numeral_Value);
         when adv =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => adv);
         when v =>
            Verb_Kind_Type_IO.Get (Verb_Kind);
            Item := (v, Verb_Kind);
         when vpar =>
            Verb_Kind_Type_IO.Get (Vpar_Kind);
            Item := (vpar, Vpar_Kind);
         when supine =>
            Verb_Kind_Type_IO.Get (Supine_Kind);
            Item := (supine, Supine_Kind);
         when prep =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => prep);
         when conj =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => conj);
         when interj =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => interj);
         when tackon =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => tackon);
         when prefix =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => prefix);
         when suffix =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => suffix);
         when x =>
            Set_Col (Col + Positive_Count (Kind_Entry_IO.Default_Width));
            Item := (pofs => x);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put
      ( File : in File_Type;
        POFS : in Part_Of_Speech_Type;
        Item : in Kind_Entry
      )
   is
      pragma Unreferenced (POFS);
      -- Used for computing bounds of substring for filling
      Ending_Col : constant Positive :=
         Kind_Entry_IO.Default_Width + Positive (Col (File)) - 1;
   begin
      case Item.pofs is
         when n =>
            Noun_Kind_Type_IO.Put (File, Item.n_kind);
         when pron =>
            Pronoun_Kind_Type_IO.Put (File, Item.pron_kind);
         when pack =>
            Pronoun_Kind_Type_IO.Put (File, Item.pack_kind);
         when num =>
            Inflections_Package.Integer_IO.Put
               ( File, Item.num_value, Numeral_Value_Type_IO_Default_Width );
         when v =>
            Verb_Kind_Type_IO.Put (File, Item.v_kind);
         when vpar =>
            Verb_Kind_Type_IO.Put (File, Item.vpar_kind);
         when supine =>
            Verb_Kind_Type_IO.Put (File, Item.supine_kind);
         when x | adj | adv =>
            null;
         when prep .. suffix =>
            null;
      end case;
      Put (File, String'(Integer (Col (File)) .. Ending_Col => ' '));
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (POFS : in Part_Of_Speech_Type; Item : in Kind_Entry)
   is
      pragma Unreferenced (POFS);
      -- Used for computing bounds of substring for filling
      Ending_Col     : constant Positive :=
         Kind_Entry_IO.Default_Width + Positive (Col) - 1;
   begin
      case Item.pofs is
         when n =>
            Noun_Kind_Type_IO.Put (Item.n_kind);
         when pron =>
            Pronoun_Kind_Type_IO.Put (Item.pron_kind);
         when pack =>
            Pronoun_Kind_Type_IO.Put (Item.pack_kind);
         when num =>
            Inflections_Package.Integer_IO.Put
               ( Item.num_value, Numeral_Value_Type_IO_Default_Width );
         when v =>
            Verb_Kind_Type_IO.Put (Item.v_kind);
         when vpar =>
            Verb_Kind_Type_IO.Put (Item.vpar_kind);
         when supine =>
            Verb_Kind_Type_IO.Put (Item.supine_kind);
         when x | adj | adv =>
            null;
         when prep .. suffix =>
            null;
      end case;
      Put (String'(Integer (Col) .. Ending_Col => ' '));
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      ( Source : in  String;
        POFS   : in  Part_Of_Speech_Type;
        Target : out Kind_Entry;
        Last   : out Integer
      )
   is
      -- Used to get lower bound of substring
      Low : constant Integer := Source'First - 1;
   begin
      Last := Low;         --  In case it is not set later
      case POFS is
         when n =>
            Noun_Kind_Type_IO.Get
               ( Source (Low + 1 .. Source'Last), Noun_Kind, Last );
            Target := (n, Noun_Kind);
         when pron =>
            Pronoun_Kind_Type_IO.Get
               ( Source (Low + 1 .. Source'Last), Pronoun_Kind, Last );
            Target := (pron, Pronoun_Kind);
         when pack =>
            Pronoun_Kind_Type_IO.Get
               ( Source (Low + 1 .. Source'Last), Propack_Kind, Last );
            Target := (pack, Propack_Kind);
         when adj =>
            Target := (pofs => adj);
         when num =>
            Inflections_Package.Integer_IO.Get
               ( Source (Low + 1 .. Source'Last), Numeral_Value, Last );
            Target := (num, Numeral_Value);
         when adv =>
            Target := (pofs => adv);
         when v =>
            Verb_Kind_Type_IO.Get
               ( Source (Low + 1 .. Source'Last), Verb_Kind, Last );
            Target := (v, Verb_Kind);
         when vpar =>
            Verb_Kind_Type_IO.Get
               ( Source (Low + 1 .. Source'Last), Vpar_Kind, Last );
            Target := (vpar, Vpar_Kind);
         when supine =>
            Verb_Kind_Type_IO.Get
               ( Source (Low + 1 .. Source'Last), Supine_Kind, Last );
            Target := (supine, Supine_Kind);
         when prep =>
            Target := (pofs => prep);
         when conj =>
            Target := (pofs => conj);
         when interj =>
            Target := (pofs => interj);
         when tackon =>
            Target := (pofs => tackon);
         when prefix =>
            Target := (pofs => prefix);
         when suffix =>
            Target := (pofs => suffix);
         when x =>
            Target := (pofs => x);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put
      ( Target : out String;
        POFS   : in  Part_Of_Speech_Type;
        Item   : in  Kind_Entry
      )
   is
      pragma Unreferenced (POFS);
      -- Used to get bounds of substrings
      Low  : constant Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Kind_Entry
      case Item.pofs is
         when n =>
            High := Low + Noun_Kind_Type_IO.Default_Width;
            Noun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.n_kind);
         when pron =>
            High := Low + Pronoun_Kind_Type_IO.Default_Width;
            Pronoun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.pron_kind);
         when pack =>
            High := Low + Pronoun_Kind_Type_IO.Default_Width;
            Pronoun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.pack_kind);
         when num =>
            High := Low + Numeral_Value_Type_IO_Default_Width;
            Inflections_Package.Integer_IO.Put
               ( Target (Low + 1 .. High), Item.num_value );
         when v =>
            High := Low + Verb_Kind_Type_IO.Default_Width;
            Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.v_kind);
         when vpar =>
            High := Low + Verb_Kind_Type_IO.Default_Width;
            Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.vpar_kind);
         when supine =>
            High := Low + Verb_Kind_Type_IO.Default_Width;
            Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.supine_kind);
         when x | adj | adv =>
            null;
         when prep .. suffix =>
            null;
      end case;

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Kind_Entry_IO;
