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
package body Dictionary_Entry_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item : out Dictionary_Entry)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      for K in Stem_Key_Type range 1 .. 4 loop
         Stem_Type_IO.Get (File, Item.Stems (K));
         Ada.Text_IO.Get (File, Spacer);
      end loop;

      Part_Entry_IO.Get (File, Item.Part);
      Ada.Text_IO.Get (File, Spacer);
      Translation_Record_IO.Get (File, Item.Tran);
      Ada.Text_IO.Get (File, Spacer);
      Ada.Text_IO.Get (File, Item.Mean);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Dictionary_Entry)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      for K in Stem_Key_Type range 1 .. 4 loop
         Stem_Type_IO.Get (Item.Stems (K));
         Ada.Text_IO.Get (Spacer);
      end loop;

      Part_Entry_IO.Get (Item.Part);
      Ada.Text_IO.Get (Spacer);
      Translation_Record_IO.Get (Item.Tran);
      Ada.Text_IO.Get (Spacer);
      Ada.Text_IO.Get (Item.Mean);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Dictionary_Entry)
   is
      Part_Col : Natural   := 0;
   begin
      for K in Stem_Key_Type range 1 .. 4 loop
         Stem_Type_IO.Put (File, Item.Stems (K));
         Ada.Text_IO.Put (File, ' ');
      end loop;

      Part_Col := Natural (Ada.Text_IO.Col (File));
      Part_Entry_IO.Put (File, Item.Part);
      Ada.Text_IO.Set_Col
         (File,
          Ada.Text_IO.Count (Part_Col + Part_Entry_IO.Default_Width + 1)
        );
      Translation_Record_IO.Put (File, Item.Tran);
      Ada.Text_IO.Put (File, ' ');
      Ada.Text_IO.Put (File, Item.Mean);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Dictionary_Entry)
   is
      Part_Col : Natural   := 0;
   begin
      for K in Stem_Key_Type range 1 .. 4 loop
         Stem_Type_IO.Put (Item.Stems (K));
         Ada.Text_IO.Put (' ');
      end loop;

      Part_Col := Natural (Ada.Text_IO.Col);
      Part_Entry_IO.Put (Item.Part);
      Ada.Text_IO.Set_Col
         (Ada.Text_IO.Count (Part_Col + Part_Entry_IO.Default_Width + 1));
      Translation_Record_IO.Put (Item.Tran);
      Ada.Text_IO.Put (' ');
      Ada.Text_IO.Put (Item.Mean);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Dictionary_Entry;
       Last   : out Integer
     )
   is
      -- Used for computing lower bound of substring
      Low  : Integer := Source'First - 1;
      -- Used for computing Last
      High : Integer := 0;
   begin
      for K in Stem_Key_Type range 1 .. 4 loop
         Stem_Type_IO.Get
            (Source (Low + 1 .. Source'Last),
              Target.Stems (K),
              Low
           );
      end loop;

      Part_Entry_IO.Get (Source (Low + 1 .. Source'Last), Target.Part, Low);
      Low := Low + 1;
      Translation_Record_IO.Get
         (Source (Low + 1 .. Source'Last),
          Target.Tran,
          Low
        );
      Low := Low + 1;
      Target.Mean := Head (Source (Low + 1 .. Source'Last), Max_Meaning_Size);

      High := Low + 1;
      while Source (High) = ' ' loop
         High := High + 1;
      end loop;

      Last := High;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Dictionary_Entry)
   is
      -- Used for computing bounds of substrings
      Low      : Integer := Target'First - 1;
      High     : Integer := 0;
      Part_Col : Natural   := 0;
   begin
      -- Put Stem_Types
      for K in Stem_Key_Type range 1 .. 4 loop
         High := Low + Max_Stem_Size;
         Target (Low + 1 .. High) := Item.Stems (K);
         Low := High + 1;
         Target (Low) :=  ' ';
      end loop;

      -- Put Part_Entry
      Part_Col := Low + 1;
      High := Low + Part_Entry_IO.Default_Width;
      Part_Entry_IO.Put (Target (Low + 1 .. High), Item.Part);

      -- Put Translation_Record
      Low  := Part_Col + Part_Entry_IO.Default_Width + 1;
      High := Low + Translation_Record_IO.Default_Width;
      Translation_Record_IO.Put (Target (Low + 1 .. High), Item.Tran);

      -- Put Meaning_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := High + Max_Meaning_Size;
      Target (Low + 1 .. High) := Item.Mean;

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Dictionary_Entry_IO;
