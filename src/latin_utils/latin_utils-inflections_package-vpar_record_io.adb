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
package body Vpar_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Vpar_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (File, Item.Con);
      Get (File, Spacer);
      Case_Type_IO.Get (File, Item.Of_Case);
      Get (File, Spacer);
      Number_Type_IO.Get (File, Item.Number);
      Get (File, Spacer);
      Gender_Type_IO.Get (File, Item.Gender);
      Get (File, Spacer);
      Tense_Voice_Mood_Record_IO.Get (File, Item.Tense_Voice_Mood);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Vpar_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (Item.Con);
      Get (Spacer);
      Case_Type_IO.Get (Item.Of_Case);
      Get (Spacer);
      Number_Type_IO.Get (Item.Number);
      Get (Spacer);
      Gender_Type_IO.Get (Item.Gender);
      Get (Spacer);
      Tense_Voice_Mood_Record_IO.Get (Item.Tense_Voice_Mood);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Vpar_Record) is
   begin
      Decn_Record_IO.Put (File, Item.Con);
      Put (File, ' ');
      Case_Type_IO.Put (File, Item.Of_Case);
      Put (File, ' ');
      Number_Type_IO.Put (File, Item.Number);
      Put (File, ' ');
      Gender_Type_IO.Put (File, Item.Gender);
      Put (File, ' ');
      Tense_Voice_Mood_Record_IO.Put (File, Item.Tense_Voice_Mood);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Vpar_Record) is
   begin
      Decn_Record_IO.Put (Item.Con);
      Put (' ');
      Case_Type_IO.Put (Item.Of_Case);
      Put (' ');
      Number_Type_IO.Put (Item.Number);
      Put (' ');
      Gender_Type_IO.Put (Item.Gender);
      Put (' ');
      Tense_Voice_Mood_Record_IO.Put (Item.Tense_Voice_Mood);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Vpar_Record;
       Last   : out Integer
      )
   is
      -- Used for computing lower bound of substrings
      Low : Integer := Source'First - 1;
   begin
      Decn_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Con, Low);
      Low := Low + 1;
      Case_Type_IO.Get   (Source (Low + 1 .. Source'Last), Target.Of_Case, Low);
      Low := Low + 1;
      Number_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Number, Low);
      Low := Low + 1;
      Gender_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Gender, Low);
      Low := Low + 1;
      Tense_Voice_Mood_Record_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Tense_Voice_Mood, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Vpar_Record)
   is
      -- Used for computing bounds of substrings
      Low : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Decn_Record
      High := Low + Decn_Record_IO.Default_Width;
      Decn_Record_IO.Put (Target (Low + 1 .. High), Item.Con);

      -- Put Case_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Case_Type_IO.Default_Width;
      Case_Type_IO.Put (Target (Low + 1 .. High), Item.Of_Case);

      -- Put Number_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Number_Type_IO.Default_Width;
      Number_Type_IO.Put (Target (Low + 1 .. High), Item.Number);

      -- Put Gender_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Gender_Type_IO.Default_Width;
      Gender_Type_IO.Put (Target (Low + 1 .. High), Item.Gender);

      -- Put Tense_Voice_Mood_Record
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Tense_Voice_Mood_Record_IO.Default_Width;
      Tense_Voice_Mood_Record_IO.Put
         (Target (Low + 1 .. High), Item.Tense_Voice_Mood);

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Vpar_Record_IO;
