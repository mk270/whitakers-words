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
package body Ending_Record_IO is

   ---------------------------------------------------------------------------

   Blanks : constant Ending := (others => ' ');

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Ending_Record)
   is
      Ending_Length : Ending_Size_Type := 0;
      Ending_Suf    : Ending := (others => ' ');
      Spacer : Character := ' ';
   begin
      Ending_Suf := Blanks;
      Ada.Integer_Text_IO.Get (File, Ending_Length);
      if Ending_Length = 0 then
         Item := Null_Ending_Record;
      else
         Get (File, Spacer);
         Get (File, Ending_Suf (1 .. Ending_Length));
         Item := (Ending_Length, Ending_Suf);
      end if;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Ending_Record)
   is
      Ending_Length : Ending_Size_Type := 0;
      Ending_Suf    : Ending := (others => ' ');
      Spacer : Character := ' ';
   begin
      Ending_Suf := Blanks;
      Ada.Integer_Text_IO.Get (Ending_Length);
      if Ending_Length = 0 then
         Item := Null_Ending_Record;
      else
         Get (Spacer);
         Get (Ending_Suf (1 .. Ending_Length));
         Item := (Ending_Length, Ending_Suf);
      end if;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Ending_Record) is
   begin
      Ada.Integer_Text_IO.Put (File, Item.Size, 1);
      Put (File, ' ');
      Put
         (File,
          Item.Suf (1 .. Item.Size) & Blanks (Item.Size + 1 .. Max_Ending_Size)
         );
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Ending_Record) is
   begin
      Ada.Integer_Text_IO.Put (Item.Size, 1);
      Put (' ');
      Put
         (Item.Suf (1 .. Item.Size) &
          Blanks (Item.Size + 1 .. Max_Ending_Size)
         );
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
     (Source : in  String;
      Target : out Ending_Record;
      Last   : out Integer
     )
   is
      Ending_Length : Ending_Size_Type := 0;
      Ending_Suf    : Ending := (others => ' ');
      -- Used for computing lower bounds of substrings
      Low : Integer := Source'First - 1;
   begin
      Ending_Suf := Blanks;
      Ada.Integer_Text_IO.Get
         (Source (Low + 1 .. Source'Last), Ending_Length, Low);
      if Ending_Length = 0 then
         Target := Null_Ending_Record;
         Last := Low;
      else
         Low := Low + 1;
         Ending_Suf := Source (Low + 1 .. Low + Ending_Length) &
               Blanks (Ending_Length + 1 .. Max_Ending_Size);
         Last := Low + Ending_Length;
         Target := (Ending_Length, Ending_Suf (1 .. Ending_Length) &
                    Blanks (Ending_Length + 1 .. Max_Ending_Size));
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("ENDING ERROR " & Source);
         raise;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Ending_Record)
   is
      -- Used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := Low + 2;
   begin
      Ada.Integer_Text_IO.Put (Target (Low + 1 .. High), Item.Size);
      High := High + 1;
      Target (High) := ' ';
      if Item.Size > 0 then
         Low  := High;
         High := Low + Item.Size;
         Target (Low + 1 .. High) := Item.Suf (1 .. Item.Size);
      end if;
      --  Being very careful here, first to fill out to the MAX_ENDING_SIZE
      Low := High;
      High := Low + Max_Ending_Size - Item.Size;
      Target (Low + 1 .. High) := (others => ' ');
      --  Then to fill out the rest of the out String, if any
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Ending_Record_IO;
