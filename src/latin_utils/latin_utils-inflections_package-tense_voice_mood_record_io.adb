-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936-2010)
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
package body Tense_Voice_Mood_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Tense_Voice_Mood_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Tense_Type_IO.Get (File, Item.Tense);
      Get (File, Spacer);
      Voice_Type_IO.Get (File, Item.Voice);
      Get (File, Spacer);
      Mood_Type_IO.Get (File, Item.Mood);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Tense_Voice_Mood_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Tense_Type_IO.Get (Item.Tense);
      Get (Spacer);
      Voice_Type_IO.Get (Item.Voice);
      Get (Spacer);
      Mood_Type_IO.Get (Item.Mood);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Tense_Voice_Mood_Record) is
   begin
      Tense_Type_IO.Put (File, Item.Tense);
      Put (File, ' ');
      Voice_Type_IO.Put (File, Item.Voice);
      Put (File, ' ');
      Mood_Type_IO.Put (File, Item.Mood);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Tense_Voice_Mood_Record) is
   begin
      Tense_Type_IO.Put (Item.Tense);
      Put (' ');
      Voice_Type_IO.Put (Item.Voice);
      Put (' ');
      Mood_Type_IO.Put (Item.Mood);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Tense_Voice_Mood_Record;
       Last   : out Integer
      )
   is
      -- Used to get lower bound of substring
      Low : Integer := Source'First - 1;
   begin
      Tense_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Tense, Low);
      Low := Low + 1;
      Voice_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Voice, Low);
      Low := Low + 1;
      Mood_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Mood, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Tense_Voice_Mood_Record)
   is
      -- Used to get bounds of substring
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Tense_Type
      High := Low + Tense_Type_IO.Default_Width;
      Tense_Type_IO.Put (Target (Low + 1 .. High), Item.Tense);

      -- Put Voice_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Voice_Type_IO.Default_Width;
      Voice_Type_IO.Put (Target (Low + 1 .. High), Item.Voice);

      -- Put Mood_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Mood_Type_IO.Default_Width;
      Mood_Type_IO.Put (Target (Low + 1 .. High), Item.Mood);

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Tense_Voice_Mood_Record_IO;
