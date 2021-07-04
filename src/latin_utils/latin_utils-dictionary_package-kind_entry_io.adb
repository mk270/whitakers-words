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
separate (Latin_Utils.Dictionary_Package)
package body Kind_Entry_IO is

   ---------------------------------------------------------------------------

   use type Ada.Text_IO.Positive_Count;

   ---------------------------------------------------------------------------

   procedure Get
      (File : in  Ada.Text_IO.File_Type;
        POFS : in  Part_Of_Speech_Type;
        Item : out Kind_Entry
     )
   is

      --------------------------------------------------------------------------
      -- Helper variables
      Noun_Kind      : Noun_Kind_Type;
      Pronoun_Kind   : Pronoun_Kind_Type;
      Propack_Kind   : Pronoun_Kind_Type;
      Verb_Kind      : Verb_Kind_Type;
      Vpar_Kind      : Verb_Kind_Type;
      Supine_Kind    : Verb_Kind_Type;
      Numeral_Value  : Numeral_Value_Type;

      --------------------------------------------------------------------------
      -- Small helper procedure
      procedure Set_Col (File : Ada.Text_IO.File_Type) is
      begin
         Ada.Text_IO.Set_Col
            (File,
              Ada.Text_IO.Col (File) +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
           );
      end Set_Col;

      --------------------------------------------------------------------------

   begin
      case POFS is
         when N =>
            Noun_Kind_Type_IO.Get (File, Noun_Kind);
            Item := (N, Noun_Kind);
         when Pron =>
            Pronoun_Kind_Type_IO.Get (File, Pronoun_Kind);
            Item := (Pron, Pronoun_Kind);
         when Pack =>
            Pronoun_Kind_Type_IO.Get (File, Propack_Kind);
            Item := (Pack, Propack_Kind);
         when Adj =>
            Set_Col (File);
            Item := (Pofs => Adj);
         when Num =>
            Ada.Integer_Text_IO.Get (File, Numeral_Value);
            Item := (Num, Numeral_Value);
         when Adv =>
            Set_Col (File);
            Item := (Pofs => Adv);
         when V =>
            Verb_Kind_Type_IO.Get (File, Verb_Kind);
            Item := (V, Verb_Kind);
         when Vpar =>
            Verb_Kind_Type_IO.Get (File, Vpar_Kind);
            Item := (Vpar, Vpar_Kind);
         when Supine =>
            Verb_Kind_Type_IO.Get (File, Supine_Kind);
            Item := (Supine, Supine_Kind);
         when Prep =>
            Set_Col (File);
            Item := (Pofs => Prep);
         when Conj =>
            Set_Col (File);
            Item := (Pofs => Conj);
         when Interj =>
            Set_Col (File);
            Item := (Pofs => Interj);
         when Tackon =>
            Set_Col (File);
            Item := (Pofs => Tackon);
         when Prefix =>
            Set_Col (File);
            Item := (Pofs => Prefix);
         when Suffix =>
            Set_Col (File);
            Item := (Pofs => Suffix);
         when X =>
            Set_Col (File);
            Item := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (POFS : in Part_Of_Speech_Type; Item : out Kind_Entry)
   is
      --------------------------------------------------------------------------
      -- Helper variables
      Noun_Kind      : Noun_Kind_Type;
      Pronoun_Kind   : Pronoun_Kind_Type;
      Propack_Kind   : Pronoun_Kind_Type;
      Verb_Kind      : Verb_Kind_Type;
      Vpar_Kind      : Verb_Kind_Type;
      Supine_Kind    : Verb_Kind_Type;
      Numeral_Value  : Numeral_Value_Type;

      --------------------------------------------------------------------------

   begin
      case POFS is
         when N =>
            Noun_Kind_Type_IO.Get (Noun_Kind);
            Item := (N, Noun_Kind);
         when Pron =>
            Pronoun_Kind_Type_IO.Get (Pronoun_Kind);
            Item := (Pron, Pronoun_Kind);
         when Pack =>
            Pronoun_Kind_Type_IO.Get (Propack_Kind);
            Item := (Pack, Propack_Kind);
         when Adj =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Adj);
         when Num =>
            Ada.Integer_Text_IO.Get (Numeral_Value);
            Item := (Num, Numeral_Value);
         when Adv =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Adv);
         when V =>
            Verb_Kind_Type_IO.Get (Verb_Kind);
            Item := (V, Verb_Kind);
         when Vpar =>
            Verb_Kind_Type_IO.Get (Vpar_Kind);
            Item := (Vpar, Vpar_Kind);
         when Supine =>
            Verb_Kind_Type_IO.Get (Supine_Kind);
            Item := (Supine, Supine_Kind);
         when Prep =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Prep);
         when Conj =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Conj);
         when Interj =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Interj);
         when Tackon =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Tackon);
         when Prefix =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Prefix);
         when Suffix =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => Suffix);
         when X =>
            Ada.Text_IO.Set_Col
               (Ada.Text_IO.Col +
                 Ada.Text_IO.Positive_Count (Kind_Entry_IO.Default_Width)
              );
            Item := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put
      (File : in Ada.Text_IO.File_Type;
        POFS : in Part_Of_Speech_Type;
        Item : in Kind_Entry
     )
   is
      pragma Unreferenced (POFS);
      -- Used for computing bounds of substring for filling
      Ending_Col : constant Positive :=
         Kind_Entry_IO.Default_Width + Positive (Ada.Text_IO.Col (File)) - 1;
   begin
      case Item.Pofs is
         when N =>
            Noun_Kind_Type_IO.Put (File, Item.N_Kind);
         when Pron =>
            Pronoun_Kind_Type_IO.Put (File, Item.Pron_Kind);
         when Pack =>
            Pronoun_Kind_Type_IO.Put (File, Item.Pack_Kind);
         when Num =>
            Ada.Integer_Text_IO.Put
               (File, Item.Num_Value, Numeral_Value_Type_IO_Default_Width);
         when V =>
            Verb_Kind_Type_IO.Put (File, Item.V_Kind);
         when Vpar =>
            Verb_Kind_Type_IO.Put (File, Item.Vpar_Kind);
         when Supine =>
            Verb_Kind_Type_IO.Put (File, Item.Supine_Kind);
         when X | Adj | Adv =>
            null;
         when Prep .. Suffix =>
            null;
      end case;
      Ada.Text_IO.Put
         (File,
           String'(Integer (Ada.Text_IO.Col (File)) .. Ending_Col => ' ')
        );
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (POFS : in Part_Of_Speech_Type; Item : in Kind_Entry)
   is
      pragma Unreferenced (POFS);
      -- Used for computing bounds of substring for filling
      Ending_Col     : constant Positive :=
         Kind_Entry_IO.Default_Width + Positive (Ada.Text_IO.Col) - 1;
   begin
      case Item.Pofs is
         when N =>
            Noun_Kind_Type_IO.Put (Item.N_Kind);
         when Pron =>
            Pronoun_Kind_Type_IO.Put (Item.Pron_Kind);
         when Pack =>
            Pronoun_Kind_Type_IO.Put (Item.Pack_Kind);
         when Num =>
            Ada.Integer_Text_IO.Put
               (Item.Num_Value, Numeral_Value_Type_IO_Default_Width);
         when V =>
            Verb_Kind_Type_IO.Put (Item.V_Kind);
         when Vpar =>
            Verb_Kind_Type_IO.Put (Item.Vpar_Kind);
         when Supine =>
            Verb_Kind_Type_IO.Put (Item.Supine_Kind);
         when X | Adj | Adv =>
            null;
         when Prep .. Suffix =>
            null;
      end case;
      Ada.Text_IO.Put (String'(Integer (Ada.Text_IO.Col) .. Ending_Col => ' '));
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
        POFS   : in  Part_Of_Speech_Type;
        Target : out Kind_Entry;
        Last   : out Integer
     )
   is

      --------------------------------------------------------------------------
      -- Helper variables
      Noun_Kind      : Noun_Kind_Type;
      Pronoun_Kind   : Pronoun_Kind_Type;
      Propack_Kind   : Pronoun_Kind_Type;
      Verb_Kind      : Verb_Kind_Type;
      Vpar_Kind      : Verb_Kind_Type;
      Supine_Kind    : Verb_Kind_Type;
      Numeral_Value  : Numeral_Value_Type;

      --------------------------------------------------------------------------
      -- Used to get lower bound of substring
      Low : constant Integer := Source'First - 1;
   begin
      Last := Low;         --  In case it is not set later
      case POFS is
         when N =>
            Noun_Kind_Type_IO.Get
               (Source (Low + 1 .. Source'Last), Noun_Kind, Last);
            Target := (N, Noun_Kind);
         when Pron =>
            Pronoun_Kind_Type_IO.Get
               (Source (Low + 1 .. Source'Last), Pronoun_Kind, Last);
            Target := (Pron, Pronoun_Kind);
         when Pack =>
            Pronoun_Kind_Type_IO.Get
               (Source (Low + 1 .. Source'Last), Propack_Kind, Last);
            Target := (Pack, Propack_Kind);
         when Adj =>
            Target := (Pofs => Adj);
         when Num =>
            Ada.Integer_Text_IO.Get
               (Source (Low + 1 .. Source'Last), Numeral_Value, Last);
            Target := (Num, Numeral_Value);
         when Adv =>
            Target := (Pofs => Adv);
         when V =>
            Verb_Kind_Type_IO.Get
               (Source (Low + 1 .. Source'Last), Verb_Kind, Last);
            Target := (V, Verb_Kind);
         when Vpar =>
            Verb_Kind_Type_IO.Get
               (Source (Low + 1 .. Source'Last), Vpar_Kind, Last);
            Target := (Vpar, Vpar_Kind);
         when Supine =>
            Verb_Kind_Type_IO.Get
               (Source (Low + 1 .. Source'Last), Supine_Kind, Last);
            Target := (Supine, Supine_Kind);
         when Prep =>
            Target := (Pofs => Prep);
         when Conj =>
            Target := (Pofs => Conj);
         when Interj =>
            Target := (Pofs => Interj);
         when Tackon =>
            Target := (Pofs => Tackon);
         when Prefix =>
            Target := (Pofs => Prefix);
         when Suffix =>
            Target := (Pofs => Suffix);
         when X =>
            Target := (Pofs => X);
      end case;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put
      (Target : out String;
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
      case Item.Pofs is
         when N =>
            High := Low + Noun_Kind_Type_IO.Default_Width;
            Noun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.N_Kind);
         when Pron =>
            High := Low + Pronoun_Kind_Type_IO.Default_Width;
            Pronoun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.Pron_Kind);
         when Pack =>
            High := Low + Pronoun_Kind_Type_IO.Default_Width;
            Pronoun_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.Pack_Kind);
         when Num =>
            High := Low + Numeral_Value_Type_IO_Default_Width;
            Ada.Integer_Text_IO.Put
               (Target (Low + 1 .. High), Item.Num_Value);
         when V =>
            High := Low + Verb_Kind_Type_IO.Default_Width;
            Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.V_Kind);
         when Vpar =>
            High := Low + Verb_Kind_Type_IO.Default_Width;
            Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.Vpar_Kind);
         when Supine =>
            High := Low + Verb_Kind_Type_IO.Default_Width;
            Verb_Kind_Type_IO.Put (Target (Low + 1 .. High), Item.Supine_Kind);
         when X | Adj | Adv =>
            null;
         when Prep .. Suffix =>
            null;
      end case;

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Kind_Entry_IO;
